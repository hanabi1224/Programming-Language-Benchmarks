# This is a much faster modification of 1.cr and probably the best what
# can be achieved with the current Hash class design from Crystal 1.3.2
#
# The main idea is to inherit from the standard Hash class and extend it
# with extra methods to support LRU semantics. Because of accessing private
# methods and data structures, this may break (fail to compile or even
# start working incorrectly) if the Hash class implementation changes
# in the future in a significant way.
#
# Additionally, the "key_hash" method overrides the default Crystal's
# hash function https://github.com/funny-falcon/funny_hash with a much
# more simplistic and faster https://github.com/rust-lang/rustc-hash
#
# Note: The "lru_upsert" method is a modified copy of the private "upsert"
# method and the "lru_fetch" method is a modified copy of the private "find_entry"
# method from https://github.com/crystal-lang/crystal/blob/master/src/hash.cr
# So the original open source Apache-2.0 License applies to this part of code.

struct LCG
  A = 1103515245_u32
  C =      12345_u32
  M = 1_u32 << 31

  def initialize(@seed : UInt32)
  end

  def next
    @seed = (A &* @seed &+ C) % M
  end
end

class LRU
  def initialize(@size : Int32)
    @hash = LruHash(UInt32, UInt32).new
  end

  def get(key)
    @hash.lru_fetch(key)
  end

  def put(key, value)
    @hash.lru_upsert(key, value, @size)
  end
end

size = ARGV.size > 0 ? ARGV[0].to_i : 100
n = ARGV.size > 1 ? ARGV[1].to_i : 1000
mod = size * 10

hit = 0
missed = 0
rng0 = LCG.new 0
rng1 = LCG.new 1
lru = LRU.new size
n.times do
  n0 = rng0.next % mod
  lru.put(n0, n0)
  n1 = rng1.next % mod
  if lru.get(n1)
    hit += 1
  else
    missed += 1
  end
end

puts hit
puts missed


class LruHash(K, V) < Hash(K, V)

  # A helper function to move the existing entry to the back of the order
  # list. The arguments 'hash', 'index' and 'entry_index' should have
  # the same values as the local variables of the 'upsert' method.
  private def lru_move_to_back(hash, index, entry_index, key, value) : Nil
    # Mark the existing entry as deleted...
    delete_entry_and_update_counts(entry_index)

    # ... but still try to reuse the existing index and make it point
    # to a new entry at the end of the `@entries` if possible
    unless entries_full?
      # Point the index at the newly created entry and we are done
      set_index(index, entries_size)
      add_entry_and_increment_size(hash, key, value)
      return
    end

    # No space at the end of `@entries` and resize is unavoidable
    resize

    # The index is not valid anymore after resize. So we need to find
    # it again. But a good thing is that we at least don't need to
    # check for key matches here (we have deleted it and the hash table
    # doesn't contain this key anymore)
    index = fit_in_indices(hash)
    until get_index(index) == -1
      index = next_index(index)
    end

    # Point the index at the newly created entry and we are done
    set_index(index, entries_size)
    add_entry_and_increment_size(hash, key, value)
    return
  end

  # Update the existing entry or insert a new entry into the hash table.
  # An updated entry is marked as the most recent for LRU. The size_limit
  # argument prevents the hash table from growing above it (but if the
  # hash table is already oversized, then the current size will be kept).
  # The oldest entry gets evicted to free space.
  def lru_upsert(key, value, size_limit) : Nil

    # TODO: maybe support the linear scan for small sizes too, but for now
    # just do something simple and inefficient
    if @entries.null? || @indices.null?
      shift unless delete(key) || size < size_limit
      upsert(key, value)
      return
    end

    hash = key_hash(key)

    # Fit the hash value into an index in `@indices`
    index = fit_in_indices(hash)

    while true
      entry_index = get_index(index)

      # If the index entry is empty...
      if entry_index == -1
        # If we reached the maximum in `@entries` it's time to resize
        if entries_full?
          resize
          # We have to fit the hash into an index in `@indices` again, and try again
          index = fit_in_indices(hash)
          next
        end

        # We have free space: store the index and then insert the entry
        set_index(index, entries_size)
        add_entry_and_increment_size(hash, key, value)
        # Evict the first entry if we are over the size limit
        delete_entry_and_update_counts(@first) if size > size_limit
        return
      end

      # We found a non-empty slot, let's see if the key we have matches
      entry = get_entry(entry_index)
      if entry_matches?(entry, hash, key)
        # It does!
        lru_move_to_back(hash, index, entry_index, key, value)
        return
      else
        # Otherwise we have to keep looking...
        index = next_index(index)
      end
    end
  end

  # Hash table lookup. If the key is found, then it is also marked as the
  # most recent for LRU. Returns the value or nil if nothing was found.
  def lru_fetch(key)
    # Empty hash table so there's no way it's there
    if @indices_size_pow2 == 0
      return nil
    end

    # TODO: maybe support the linear scan for small sizes too, but for now
    # just do something simple and inefficient
    if @indices.null?
      if value = delete(key)
        upsert(key, value)
        return value
      end
      return nil
    end

    hash = key_hash(key)

    # Fit hash into `@indices` size
    index = fit_in_indices(hash)
    while true
      entry_index = get_index(index)

      # If we find an empty index slot, there's no such key
      if entry_index == -1
        return nil
      end

      # We found a non-empty slot, let's see if the key we have matches
      entry = get_entry(entry_index)
      if entry_matches?(entry, hash, key)
        # It does!
        value = entry.value
        lru_move_to_back(hash, index, entry_index, key, value)
        return value
      else
        # Nope, move on to the next slot
        index = next_index(index)
      end
    end
  end

  private def key_hash(key : UInt32)
    # FXHash (used by hashbrown before moving to AHash) is basically just
    # multiplying the key by a magic constant (0x9e3779b9 on 32-bit systems
    # or 0x517cc1b727220a95 on 64-bit systems):
    # https://github.com/rust-lang/rustc-hash/blob/master/src/lib.rs#L64-L81
    hash = 0x9e3779b9_u32 &* key
    hash == 0 ? UInt32::MAX : hash
  end
end
