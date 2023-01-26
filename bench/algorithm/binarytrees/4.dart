/* The Computer Language Benchmarks game
   https://salsa.debian.org/benchmarksgame-team/benchmarksgame/

   Contributed by Jos Hirth, transliterated from Jarkko Miettinen's Java program
   Optimized and parallelized by by Dwayne Slater
   modified by Isaac Gouy
   mostly same but do stretch tree and longLivedTree earlier
   migrated
*/

import 'dart:async';
import 'dart:io';
import 'dart:isolate';

const int minDepth = 4;

/// Number of workers to startup. A number below the number of processors on the
/// current system works best.
const int workerCount = 4;

/// Whether to output debug information for workers.
const bool workerDebug = false;

class Worker {
  final SendPort workPort;
  bool ready = true;
  Capability? workItem;

  Worker(this.workPort);

  void submitWork(List<Object> work) {
    workPort.send(work);
    ready = false;
    workItem = work[2] as Capability;
  }
}

void worker(SendPort managerPort) {
  // Make worker port
  final worker = RawReceivePort((Object message) {
    // `message` is a work item
    final workItem = message as List;

    // Process the work item
    final iterations = workItem[1] as int;
    var check = 0;
    for (var i = 1; i <= iterations; i++) {
      check += TreeNode.itemCheck(TreeNode.bottomUpTree(workItem[0] as int));
    }

    managerPort.send([check, workItem[2]]);
  });

  managerPort.send(worker.sendPort);
}

/// Manages distributing work to available workers
class Manager {
  final RawReceivePort managerPort;
  final List<Worker> workers;
  final queuedWork = <List<Object>>[];
  final Map<Capability, Completer<int>> workCompleters = {};

  Manager(this.managerPort, this.workers) {
    managerPort.handler = (Object message) {
      if (message is List) {
        final cap = message[1];
        final value = workCompleters.remove(cap);
        if (value != null) {
          value.complete(message[0] as FutureOr<int>?);
        }
        for (var i = 0; i < workers.length; i++) {
          final worker = workers[i];
          if (worker.workItem == cap) {
            if (queuedWork.isNotEmpty) {
              if (workerDebug) print('Submitting more work to $i');
              worker.submitWork(queuedWork.removeLast());
            } else {
              if (workerDebug) print('Worker $i idle');
              worker.ready = true;
            }
            break;
          }
        }
      }
    };
  }

  Future<int> enqueue(int depth, int iterations) {
    final cap = Capability();
    final work = [depth, iterations, cap];
    final completer = Completer<int>.sync();
    workCompleters[cap] = completer;

    // Try submit work item to a ready worker
    for (var i = 0; i < workers.length; i++) {
      final worker = workers[i];
      if (worker.ready) {
        if (workerDebug) print('Dispatched work to worker $i');
        worker.submitWork(work);
        return completer.future;
      }
    }

    // Otherwise, throw into work queue for work stealing
    if (workerDebug) print('Work added to queue');
    queuedWork.add(work);
    return completer.future;
  }

  static Future<Manager> init(int workerCount) async {
    // Spawn and wait for all workers to come online
    final workers = <Worker>[];
    final completer = Completer<void>();
    final managerPort = RawReceivePort((workerPort) {
      workers.add(Worker(workerPort as SendPort));
      if (workers.length == workerCount) {
        completer.complete();
      }
    });
    for (var i = 0; i < workerCount; i++) {
      await Isolate.spawn(worker, managerPort.sendPort);
    }
    await completer.future;
    return Manager(managerPort, workers);
  }
}

Future<void> main(List<String> args) async {
  final n = args.isNotEmpty ? int.parse(args[0]) : 7;

  // Start up the workers, then dispatch work to them
  final futureManager = Manager.init(workerCount);
  final manager = await futureManager;

  final maxDepth = (minDepth + 2 > n) ? minDepth + 2 : n;
  final stretchDepth = maxDepth + 1;

  final check = TreeNode.itemCheck(TreeNode.bottomUpTree(stretchDepth));
  print('stretch tree of depth $stretchDepth\t check: $check');

  final longLivedTree = TreeNode.bottomUpTree(maxDepth);

  final workFuture = <Future<int>>[];
  for (var depth = minDepth; depth <= maxDepth; depth += 2) {
    final iterations = 1 << (maxDepth - depth + minDepth);
    workFuture.add(manager.enqueue(depth, iterations));
  }

  for (var depth = minDepth; depth <= maxDepth; depth += 2) {
    final iterations = 1 << (maxDepth - depth + minDepth);
    final check = await workFuture.removeAt(0);
    print('$iterations\t trees of depth $depth\t check: $check');
  }

  print(
    'long lived tree of depth $maxDepth\t '
    'check: ${TreeNode.itemCheck(longLivedTree)}',
  );

  // It takes time to clean up the workers, so just exit instead
  exit(0);
}

class TreeNode {
  final TreeNode? left, right;

  const TreeNode([this.left, this.right]);

  static TreeNode bottomUpTree(int depth) => depth > 0
      ? TreeNode(bottomUpTree(depth - 1), bottomUpTree(depth - 1))
      : const TreeNode();

  static int itemCheck(TreeNode? node) {
    if (node == null || node.left == null) {
      return 1;
    }
    return 1 + itemCheck(node.left) + itemCheck(node.right);
  }
}
