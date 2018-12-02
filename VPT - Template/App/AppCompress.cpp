#include "StdAfx.h"
#include "AppCompress.h"

CAppCompress::CAppCompress(void)
{
	// Class Constructor
}

CAppCompress::~CAppCompress(void)
{
	// Class Destructor
	// Must call Final() function in the base class

	Final();
}

void CAppCompress::CustomInit(CView *pView) {
	// Add custom initialization code here
	// This initialization code will be called when this application is added to a processing task lists
}

void CAppCompress::CustomFinal(void) {
	// Add custom finalization code here
}

int predictEval(unsigned char *buf, int x, int y, int width, int height, int &diffValue) {

	int predT;
	int predL;
	int predTL;
	int pred;
	int left;
	int top;
	int topLeft;
	int mode;
	int actual;
	int diff;

	if (x <= 0) {
		left = 0;
		topLeft = 0;
	}
	if (y <= 0) {
		top = 0;
		topLeft = 0;
	}
	if (y > 0) {
		top = buf[x + (y - 1) * width];
	}
	if (x > 0 && y >= 0) {
		left = buf[(x - 1) + y * width];
	}
	if (x > 0 && y > 0) {
		topLeft = buf[(x - 1) + (y - 1) * width];
	}

	predT = top;
	predL = left;
	predTL = topLeft;
	actual = buf[x + y * width];

	if (predL <= predT && predL <= predTL) {
		mode = 1;
		pred = predL;
	}
	else if (predT <= predL && predT <= predTL) {
		mode = 2;
		pred = predT;
	}
	else if (predTL <= predL && predTL <= predT) {
		mode = 3;
		pred = predTL;
	}
	diff = actual - pred;
	diff = diff >= 0 ? diff : -diff;
	if (diff >= 8) {
		mode = 4;
		diffValue = actual;
	}
	else {
		diffValue = actual - pred;
	}
	return mode;
}

unsigned char predDiff(unsigned char *buf, int x, int y, int width, int height, int mode, int diffValue) {

	int predT;
	int predL;
	int predTL;
	int pred;
	int left;
	int top;
	int topLeft;

	if (x <= 0) {
		left = 0;
		topLeft = 0;
	}
	if (y <= 0) {
		top = 0;
		topLeft = 0;
	}
	if (y > 0) {
		top = buf[x + (y - 1) * width];
	}
	if (x > 0 && y >= 0) {
		left = buf[(x - 1) + y * width];
	}
	if (x > 0 && y > 0) {
		topLeft = buf[(x - 1) + (y - 1) * width];
	}
	predT = top;
	predL = left;
	predTL = topLeft;

	switch (mode) {
	case 1:
		pred = predL + diffValue;
		break;
	case 2:
		pred = predT + diffValue;
		break;
	case 3:
		pred = predTL + diffValue;
		break;
	case 4:
		pred = diffValue;
		break;
	}

	return (unsigned char)pred;
}




//    =============================================================huffman  encoding & decoding===============================================================


#include <time.h>
#include <stdlib.h>

short           father[512];
unsigned short  code[256], heap_length;
unsigned long   compress_charcount, file_size, heap[257];
unsigned char   code_length[256];
long            frequency_count[512];

short           decomp_tree[512];

//FILE            *ifile, *ofile;

int total_time;
clock_t start, finish;

unsigned char *pimg;

void reheap(unsigned short heap_entry);



void get_frequency_count()
{
	register unsigned long  loop;//如某个变量使用过于频繁，可能把变量放在 寄存器 更合适  

	for (loop = 0; loop < file_size; loop++)
		//frequency_count[getc(ifile)]++;//getc (ifile)从文件指针指向的文件读入一个字符  
	{
		//todo
		frequency_count[pimg[loop]]++;
	}
}

void build_initial_heap()
{
	//void    reheap();//从当前的堆树结构中建立逻辑堆结构  

	register unsigned short  loop;

	heap_length = 0;

	for (loop = 0; loop < 256; loop++)
		if (frequency_count[loop])
			heap[++heap_length] = (unsigned long)loop;

	for (loop = heap_length; loop > 0; loop--)
		reheap(loop);
}



void reheap(unsigned short heap_entry)
//unsigned short  heap_entry;
{
	register unsigned short  index;
	register unsigned short  flag = 1;

	unsigned long   heap_value;

	heap_value = heap[heap_entry];

	while ((heap_entry <= (heap_length >> 1)) && (flag))
	{
		index = heap_entry << 1;

		if (index < heap_length)
			if (frequency_count[heap[index]] >= frequency_count[heap[index + 1]])
				index++;

		if (frequency_count[heap_value] < frequency_count[heap[index]])
			flag--;
		else
		{
			heap[heap_entry] = heap[index];
			heap_entry = index;
		}
	}

	heap[heap_entry] = heap_value;
}



void build_code_tree()
{
	//void    reheap();

	register unsigned short  findex;
	register unsigned long   heap_value;


	while (heap_length != 1)
	{
		heap_value = heap[1];
		heap[1] = heap[heap_length--];

		reheap(1);
		findex = heap_length + 255;

		frequency_count[findex] = frequency_count[heap[1]] +
			frequency_count[heap_value];
		father[heap_value] = findex;
		father[heap[1]] = -findex;
		heap[1] = findex;

		reheap(1);
	}

	father[256] = 0;
}

void  build_decomp_tree()
{
	register unsigned short  loop1;
	register unsigned short  current_index;

	unsigned short  loop;
	unsigned short  current_node = 1;


	decomp_tree[1] = 1;

	for (loop = 0; loop < 256; loop++)
	{
		if (code_length[loop])
		{
			current_index = 1;
			for (loop1 = code_length[loop] - 1; loop1 > 0; loop1--)
			{
				current_index = (decomp_tree[current_index] << 1) +
					((code[loop] >> loop1) & 1);
				if (!(decomp_tree[current_index]))
					decomp_tree[current_index] = ++current_node;
			}
			decomp_tree[(decomp_tree[current_index] << 1) +
				(code[loop] & 1)] = -loop;
		}
	}
}



unsigned short  generate_code_table()
{
	register unsigned short  loop;
	register unsigned short  current_length;
	register unsigned short  current_bit;

	unsigned short  bitcode;
	short           parent;


	for (loop = 0; loop < 256; loop++)
		if (frequency_count[loop])
		{
			current_length = bitcode = 0;
			current_bit = 1;
			parent = father[loop];

			while (parent)
			{
				if (parent < 0)
				{
					bitcode += current_bit;
					parent = -parent;
				}
				parent = father[parent];
				current_bit <<= 1;
				current_length++;
			}

			code[loop] = bitcode;

			if (current_length > 16)
				return (0);
			else
				code_length[loop] = (unsigned char)current_length;
		}
		else
			code[loop] = code_length[loop] = 0;

	return (1);
}




// This function compresses input 24-bit image (8-8-8 format, in pInput pointer).
// This function shall allocate storage space for compressedData, and return it as a pointer.
// The input reference variable cDataSize, is also serve as an output variable to indicate the size (in bytes) of the compressed data.
unsigned char *CAppCompress::Compress(int &cDataSize) {

	// You can modify anything within this function, but you cannot change the function prototype.


	//unsigned char *compressedData;
	unsigned char * tmp;

	cDataSize = width * height * 3;			  // You need to determine the size of the compressed data. 
															  // Here, we simply set it to the size of the original image
	tmp = new unsigned char[cDataSize]; // As an example, we just copy the original data as compressedData.

	file_size = cDataSize;
	pimg = pInput;



	register unsigned int    thebyte = 0;
	register short           loop1;
	register unsigned short  current_code;
	register unsigned long   loop;

	unsigned short  current_length, dvalue;
	unsigned long   curbyte = 0;
	short           curbit = 7;


	get_frequency_count();


	build_initial_heap();

	build_code_tree();

	generate_code_table();


	unsigned int idx = 0;
	//unsigned int cidx = 0;
	for (loop = 0L; loop < file_size; loop++)
	{
		//dvalue = (unsigned short)getc(ifile);
		dvalue = (unsigned short)pInput[idx++];
		current_code = code[dvalue];
		current_length = (unsigned short)code_length[dvalue];

		for (loop1 = current_length - 1; loop1 >= 0; --loop1)
		{
			if ((current_code >> loop1) & 1)
				thebyte |= (char)(1 << curbit);

			if (--curbit < 0)
			{
				//putc(thebyte, ofile);
				tmp[curbyte] = thebyte;
				thebyte = 0;
				curbyte++;
				curbit = 7;
			}
		}
	}
//	putc(thebyte, ofile);
//	compress_charcount = ++curbyte;

	tmp[curbyte] = thebyte;
	cDataSize = curbyte;
	
	unsigned char *compressedData;

	compressedData = new unsigned char[cDataSize];

	memcpy(compressedData, tmp, cDataSize);

	return compressedData;		// return the compressed data
}

// This function takes in compressedData with size cDatasize, and decompresses it into 8-8-8 image.
// The decompressed image data should be stored into the uncompressedData buffer, with 8-8-8 image format
void CAppCompress::Decompress(unsigned char *compressedData, int cDataSize, unsigned char *uncompressedData) {
	// You can modify anything within this function, but you cannot change the function prototype.

	build_decomp_tree();

	register unsigned short  cindex = 1;
	register char            curchar;
	register short           bitshift;

	unsigned long  charcount = 0L;

	unsigned int compressed_idx = 0;
	unsigned int uncompressed_idx = 0;
	while (charcount < cDataSize)
	{
		curchar = compressedData[compressed_idx++];

		for (bitshift = 7; bitshift >= 0; --bitshift)
		{
			cindex = (cindex << 1) + ((curchar >> bitshift) & 1);

			if (decomp_tree[cindex] <= 0)
			{
				uncompressedData[uncompressed_idx++] = (int)(-decomp_tree[cindex]);

				if ((++charcount) == file_size)
					bitshift = 0;
				else
					cindex = 1;
			}
			else
				cindex = decomp_tree[cindex];
		}
	}
}


void CAppCompress::Process(void) {

	// Don't change anything within this function.

	int i, cDataSize;

	unsigned char *compressedData;
	unsigned char *verifyCompressedData;

	SetTitle(pOutput, _T("Lossless Decompressed Image"));

	compressedData = Compress(cDataSize);

	verifyCompressedData = new unsigned char[cDataSize];

	memcpy(verifyCompressedData, compressedData, cDataSize);

	delete[] compressedData;

	Decompress(verifyCompressedData, cDataSize, pOutput);

	for (i = 0; i < width * height * 3; i++) {
		if (pInput[i] != pOutput[i]) {
			printf(_T("Caution: Decoded Image is not identical to the Original Image!\r\n"));
			break;
		}
	}

	printf(_T("Original Size = %d, Compressed Size = %d, Compression Ratio = %2.2f\r\n"), width * height * 3, cDataSize, (double)width * height * 3 / cDataSize);

	PutDC(pOutput);
}
