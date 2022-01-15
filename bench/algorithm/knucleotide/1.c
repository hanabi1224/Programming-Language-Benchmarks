// Contributed by Jeremy Zerfas

// This controls the initial size used for the hash tables. This needs to be a power of two because a mask is also
// calculated from this by using INITIAL_HASH_TABLE_SIZE-1.
#define INITIAL_HASH_TABLE_SIZE 16

// This controls the maximum length for each set of oligonucleotide frequencies and each oligonucleotide count output by
// this program.
#define MAXIMUM_OUTPUT_LENGTH 4096

#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

// intptr_t should be the native integer type on most sane systems.
typedef intptr_t intnative_t;


//******************************************
//*** Start of hash table implementation ***
//******************************************

// In order to prevent too many collisions from occurring the hash table is grown when it is filled to a certain
// percentage. This value sets the percentage that controls when growing should occur. This value must be set as a
// fraction between 0 and 1 but sane values are generally around 3/4. Also do NOT place this value in parentheses since
// it would just perform integer division and evaluate as zero, the value needs to be multiplied by another value first
// before the division is performed in order to generate useful values. Setting the value too low causes the hash table
// to be made larger than it needs to be which reduces the effectiveness of caches and setting it too high will cause a
// large amount of collisions.
#define HASH_TABLE_LOAD_LIMIT 12/16

typedef struct element{
	#define EMPTY_VALUE_KEY -1
	int64_t key;   // If key is negative, then this element is empty, otherwise key and value contain the unmodified key
	               // and value.
	int32_t value;
} element;

typedef struct hash_table{
	intnative_t size;          // The current capacity of the hash table. Never will actually be reached since the hash
	                           // table will be grown first when it reaches element_Limit.
	int64_t     key_Mask;      // ANDed with keys to make sure that hash table indices do not exceed the size of the
	                           // hash table.
	intnative_t element_Limit; // Controls the maximum amount of elements that are allowed in the hash table before it
	                           // will be grown.
	intnative_t element_Count; // The current amount of elements in the hash table.
	element *   elements;
} hash_table;


// Create a hash table with space allocated for requested_Size elements. requested_Size must be a power of two since the
// mask for keys is defined as requested_Size-1.
hash_table * create_Hash_Table(intnative_t requested_Size){

	hash_table * created_Hash_Table=malloc(sizeof(hash_table));

	// Initialize the properties for the created_Hash_Table.
	created_Hash_Table->size=requested_Size;
	created_Hash_Table->key_Mask=requested_Size-1;
	created_Hash_Table->element_Limit=requested_Size*HASH_TABLE_LOAD_LIMIT;
	created_Hash_Table->element_Count=0;
	created_Hash_Table->elements=malloc(requested_Size*sizeof(element));

	// Initialize all elements in the created_Hash_Table to have initial keys set to EMPTY_VALUE_KEY and values set to
	// 0.
	for(intnative_t i=0; i<requested_Size; i++)
		created_Hash_Table->elements[i]=(element){EMPTY_VALUE_KEY, 0};

	return created_Hash_Table;
}


// Destroy hash table pointed to by hash_Table_To_Destroy and all of its elements.
void destroy_Hash_Table(hash_table * hash_Table_To_Destroy){
	free(hash_Table_To_Destroy->elements);
	free(hash_Table_To_Destroy);
}


// Hash function used to hash keys.
#define hash_Key(key) (key ^ key>>7)


// Grow hash_Table_To_Grow by quadrupling it in size. A new elements array is created, the existing elements are
// inserted into the new elements array, the old elements array is deleted, and the properties for hash_Table_To_Grow
// are updated.
void grow_Hash_Table(hash_table * hash_Table_To_Grow){

	intnative_t old_Hash_Table_Size=hash_Table_To_Grow->size;
	intnative_t new_Hash_Table_Size=old_Hash_Table_Size*4;

	// Keep a reference to old_Hash_Table_Elements and allocate space for new_Hash_Table_Elements.
	element * old_Hash_Table_Elements=hash_Table_To_Grow->elements;
	element * new_Hash_Table_Elements=malloc(new_Hash_Table_Size*sizeof(element));

	// Update the properties for the hash_Table_To_Grow.
	hash_Table_To_Grow->size=new_Hash_Table_Size;
	hash_Table_To_Grow->key_Mask=new_Hash_Table_Size-1;
	hash_Table_To_Grow->element_Limit=new_Hash_Table_Size*HASH_TABLE_LOAD_LIMIT;
	hash_Table_To_Grow->elements=new_Hash_Table_Elements;

	// Initialize all elements in new_Hash_Table_Elements to have initial keys set to EMPTY_VALUE_KEY and values set to
	// 0.
	for(intnative_t i=0; i<new_Hash_Table_Size; i++)
		new_Hash_Table_Elements[i]=(element){EMPTY_VALUE_KEY, 0};

	// Copy all old_Hash_Table_Elements to new_Hash_Table_Elements. This code is simpler and faster than using the
	// find_Or_Add_Element_For_Key() function since we don't need to worry about updating element_Count and checking to
	// see if we have reached element_Limit.
	for(intnative_t i=0; i<old_Hash_Table_Size; i++){
		if(old_Hash_Table_Elements[i].key>=0){

			int64_t elements_Index=hash_Key(old_Hash_Table_Elements[i].key) & hash_Table_To_Grow->key_Mask;

			// Find the first free spot in new_Hash_Table_Elements and copy the old element to it.
			while(new_Hash_Table_Elements[elements_Index].key>=0){
				elements_Index++;
				elements_Index&=hash_Table_To_Grow->key_Mask;
			}
			new_Hash_Table_Elements[elements_Index]=old_Hash_Table_Elements[i];
		}
	}

	free(old_Hash_Table_Elements);
}


// See if key is already in hash_Table and if so then return the element for it, otherwise add the key to hash_table
// (and grow it if necessary) and return the element for it.
element * find_Or_Add_Element_For_Key(hash_table * hash_Table, int64_t key){

	int64_t elements_Index=hash_Key(key) & hash_Table->key_Mask;

	// Search hash_Table for key.
	element * elements=hash_Table->elements;
	while(elements[elements_Index].key!=key){

		// If we reach a key with a negative value then that means that key is not in hash_Table so we will go ahead and
		// add it.
		if(elements[elements_Index].key<0){

			// If we're at the hash table's load limit then grow the hash table and call this function a second time to
			// add and return an item.
			if(hash_Table->element_Count>=hash_Table->element_Limit){
				grow_Hash_Table(hash_Table);
				return find_Or_Add_Element_For_Key(hash_Table, key);
			}

			// Set the key for this element to key, increment element_Count, and break out of the loop so that this
			// element will be returned.
			elements[elements_Index].key=key;
			hash_Table->element_Count++;
			break;
		}

		// Still haven't found key or a free spot so continue to the next index.
		elements_Index++;
		elements_Index&=hash_Table->key_Mask;
	}
	return &elements[elements_Index];
}
//******************************************
//***  End of hash table implementation  ***
//******************************************


// Macro to convert a nucleotide character to a code. Note that upper and lower case ASCII letters only differ in the
// fifth bit from the right and we only need the three least significant bits to differentiate the letters 'A', 'C',
// 'G', and 'T'. Spaces in this array/string will never be used as long as characters other than 'A', 'C', 'G', and 'T'
// aren't used.
#define code_For_Nucleotide(nucleotide) (" \0 \1\3  \2"[nucleotide & 0b111])


// And one more macro to convert the codes back to nucleotide characters.
#define nucleotide_For_Code(code) ("ACGT"[code & 0b11])


// Function to use when sorting elements with qsort() later. Elements with larger values will come first and in cases of
// identical values then elements with smaller keys will come first.
int element_Compare(const void * uncasted_Left_Element, const void * uncasted_Right_Element){

	const element * left_Element=uncasted_Left_Element, * right_Element=uncasted_Right_Element;

	// Sort based on element values.
	if(left_Element->value < right_Element->value) return 1;
	if(left_Element->value > right_Element->value) return -1;

	// If we got here then both items have the same value so then sort based on key.
	if(left_Element->key > right_Element->key)
		return 1;
	else
		return -1;
}


// Generate frequences for all oligonucleotides in polynucleotide that are of desired_Length_For_Oligonucleotides and
// then save it to output.
void generate_Frequencies_For_Desired_Length_Oligonucleotides(char * polynucleotide, intnative_t polynucleotide_Length
  , intnative_t desired_Length_For_Oligonucleotides, char * output){

	hash_table * hash_Table=create_Hash_Table(INITIAL_HASH_TABLE_SIZE);

	// Add all the complete oligonucleotides of desired_Length_For_Oligonucleotides to hash_Table.
	int64_t code=0;
	for(intnative_t i=0; i<polynucleotide_Length; i++){
		int64_t mask=((int64_t)1<<2*desired_Length_For_Oligonucleotides)-1;
		code=(code<<2 & mask) | polynucleotide[i];
		if(i>=desired_Length_For_Oligonucleotides-1)
			find_Or_Add_Element_For_Key(hash_Table, code)->value++;
	}

	// Create an array of elements from hash_Table.
	intnative_t elements_Array_Size=hash_Table->element_Count;
	element * elements_Array=malloc(elements_Array_Size*sizeof(element));
	for(intnative_t i=0, j=0; i<hash_Table->size; i++){
		if(hash_Table->elements[i].key>=0){
			elements_Array[j].key=hash_Table->elements[i].key;
			elements_Array[j].value=hash_Table->elements[i].value;
			j++;
		}
	}

	destroy_Hash_Table(hash_Table);

	// Sort elements_Array.
	qsort(elements_Array, elements_Array_Size, sizeof(element), &element_Compare);

	// Calculate the total_Count of all elements.
	intnative_t total_Count=0;
	for(intnative_t i=0; i<elements_Array_Size; i++)
		total_Count+=elements_Array[i].value;

	// Print the frequencies for each element.
	for(intnative_t output_Position=0, i=0; i<elements_Array_Size; i++){

		// Decode key back into a oligonucleotide.
		char oligonucleotide[desired_Length_For_Oligonucleotides+1];
		for(intnative_t j=desired_Length_For_Oligonucleotides-1; j>-1; j--){
			oligonucleotide[j]=nucleotide_For_Code(elements_Array[i].key);
			elements_Array[i].key>>=2;
		}
		oligonucleotide[desired_Length_For_Oligonucleotides]='\0';

		// Output the frequency for oligonucleotide to output.
		output_Position+=snprintf(output+output_Position, MAXIMUM_OUTPUT_LENGTH-output_Position, "%s %.3f\n"
		  , oligonucleotide, 100.0f*elements_Array[i].value/total_Count);
	}

	free(elements_Array);
}


// Generate a count for the number of times oligonucleotide appears in polynucleotide and then save it to output.
void generate_Count_For_Oligonucleotide(char * polynucleotide, intnative_t polynucleotide_Length, char * oligonucleotide
  , char * output){

	intnative_t oligonucleotide_Length=strlen(oligonucleotide);

	hash_table * hash_Table=create_Hash_Table(INITIAL_HASH_TABLE_SIZE);

	// Add all the complete oligonucleotides of oligonucleotide_Length to hash_Table.
	int64_t key=0;
	for(intnative_t i=0; i<polynucleotide_Length; i++){
		int64_t mask=((int64_t)1<<2*oligonucleotide_Length)-1;
		key=(key<<2 & mask) | polynucleotide[i];
		if(i>=oligonucleotide_Length)
			find_Or_Add_Element_For_Key(hash_Table, key)->value++;
	}

	// Generate key for the oligonucleotide.
	key=0;
	for(intnative_t i=0; i<oligonucleotide_Length; i++)
		key=(key<<2) | code_For_Nucleotide(oligonucleotide[i]);

	// Output the count for oligonucleotide to output.
	intnative_t count=find_Or_Add_Element_For_Key(hash_Table, key)->value;
	snprintf(output, MAXIMUM_OUTPUT_LENGTH, "%jd\t%s", (intmax_t)count, oligonucleotide);

	destroy_Hash_Table(hash_Table);
}


int main(int argc, char * argv[]){
	char buffer[4096];

	// Open the file that was specified as a command line argument.
	FILE * input_File=fopen(argv[1], "r");

	// Find the start of the third polynucleotide.
	while(fgets(buffer, sizeof(buffer), input_File) && memcmp(">THREE", buffer, sizeof(">THREE")-1));

	// Start with 64 KiB of storage for reading in the polynucleotide and grow geometrically.
	intnative_t polynucleotide_Capacity=65536;
	intnative_t polynucleotide_Length=0;
	char * polynucleotide=malloc(polynucleotide_Capacity);

	// Start reading and encoding the third polynucleotide.
	while(fgets(buffer, sizeof(buffer), input_File) && buffer[0]!='>'){
		for(intnative_t i=0; buffer[i]!='\0'; i++)
			if(buffer[i]!='\n')
				polynucleotide[polynucleotide_Length++]=code_For_Nucleotide(buffer[i]);

		// Make sure we still have enough memory allocated for any potential nucleotides in the next line.
		if(polynucleotide_Capacity-polynucleotide_Length < sizeof(buffer))
			polynucleotide=realloc(polynucleotide, polynucleotide_Capacity*=2);
	}

	// Free up any leftover memory.
	polynucleotide=realloc(polynucleotide, polynucleotide_Length);

	char output_Buffer[7][MAXIMUM_OUTPUT_LENGTH];

	// Do the following functions in parallel.
	#pragma omp parallel sections
	{
		#pragma omp section
		generate_Frequencies_For_Desired_Length_Oligonucleotides(polynucleotide, polynucleotide_Length, 1
		  , output_Buffer[0]);

		#pragma omp section
		generate_Frequencies_For_Desired_Length_Oligonucleotides(polynucleotide, polynucleotide_Length, 2
		  , output_Buffer[1]);

		#pragma omp section
		generate_Count_For_Oligonucleotide(polynucleotide, polynucleotide_Length, "GGT", output_Buffer[2]);

		#pragma omp section
		generate_Count_For_Oligonucleotide(polynucleotide, polynucleotide_Length, "GGTA", output_Buffer[3]);

		#pragma omp section
		generate_Count_For_Oligonucleotide(polynucleotide, polynucleotide_Length, "GGTATT", output_Buffer[4]);

		#pragma omp section
		generate_Count_For_Oligonucleotide(polynucleotide, polynucleotide_Length, "GGTATTTTAATT", output_Buffer[5]);

		#pragma omp section
		generate_Count_For_Oligonucleotide(polynucleotide, polynucleotide_Length, "GGTATTTTAATTTATAGT"
		  , output_Buffer[6]);
	}

	for(intnative_t i=0; i<7; printf("%s\n", output_Buffer[i++]));

	free(polynucleotide);
}
