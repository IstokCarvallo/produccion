$PBExportHeader$n_zipapi.sru
$PBExportComments$Zlib Functions
forward
global type n_zipapi from nonvisualobject
end type
type tm_unz from structure within n_zipapi
end type
type unz_global_info from structure within n_zipapi
end type
type zip_fileinfo from structure within n_zipapi
end type
type filetime from structure within n_zipapi
end type
type win32_find_data from structure within n_zipapi
end type
type systemtime from structure within n_zipapi
end type
type unz_fileinfo from structure within n_zipapi
end type
end forward

type tm_unz from structure
	unsignedinteger		tm_sec
	unsignedinteger		tm_gap1
	unsignedinteger		tm_min
	unsignedinteger		tm_gap2
	unsignedinteger		tm_hour
	unsignedinteger		tm_gap3
	unsignedinteger		tm_mday
	unsignedinteger		tm_gap4
	unsignedinteger		tm_mon
	unsignedinteger		tm_gap5
	unsignedinteger		tm_year
	unsignedinteger		tm_gap6
end type

type unz_global_info from structure
	unsignedlong		number_entry
	unsignedlong		size_comment
end type

type zip_fileinfo from structure
	tm_unz		tmz_date
	unsignedlong		dos_date
	unsignedlong		internal_fa
	unsignedlong		external_fa
end type

type filetime from structure
	unsignedlong		dwlowdatetime
	unsignedlong		dwhighdatetime
end type

type win32_find_data from structure
	unsignedlong		dwfileattributes
	FILETIME		ftcreationtime
	FILETIME		ftlastaccesstime
	FILETIME		ftlastwritetime
	unsignedlong		nfilesizehigh
	unsignedlong		nfilesizelow
	unsignedlong		dwreserved0
	unsignedlong		dwreserved1
	character		cfilename[260]
	character		calternatefilename[14]
end type

type systemtime from structure
	unsignedinteger		wyear
	unsignedinteger		wmonth
	unsignedinteger		wdayofweek
	unsignedinteger		wday
	unsignedinteger		whour
	unsignedinteger		wminute
	unsignedinteger		wsecond
	unsignedinteger		wmilliseconds
end type

type unz_fileinfo from structure
	unsignedlong		version
	unsignedlong		version_needed
	unsignedlong		flag
	unsignedlong		compression_method
	unsignedlong		dosdate
	unsignedlong		crc
	unsignedlong		compressed_size
	unsignedlong		uncompressed_size
	unsignedlong		size_filename
	unsignedlong		size_file_extra
	unsignedlong		size_file_comment
	unsignedlong		disk_num_start
	unsignedlong		internal_fa
	unsignedlong		external_fa
	tm_unz		tmu_date
end type

global type n_zipapi from nonvisualobject
end type
global n_zipapi n_zipapi

type prototypes
// zip lib functions
Function integer compress (Ref blob dest, Ref ulong destLen, Ref blob src, 	ulong srcLen ) Library "zipapi.dll"
Function integer compress2 (Ref blob dest, Ref ulong destLen, Ref blob src, ulong srcLen, int level ) Library "zipapi.dll"
Function integer uncompress (Ref blob dest, Ref ulong destLen, Ref blob src, ulong srcLen ) Library "zipapi.dll"
Function string zlibVersion () Library "zipapi.dll" alias for "zlibVersion;Ansi"
Function ulong unzOpen (string filename) Library "zipapi.dll" alias for "unzOpen;Ansi"
Function integer unzClose (ulong unzFile) Library "zipapi.dll"
Function integer unzGetGlobalComment (ulong unzFile, Ref string szComment, ulong uSizeBuf) Library "zipapi.dll" alias for "unzGetGlobalComment;Ansi"
Function integer unzGetGlobalInfo (ulong unzFile, Ref unz_global_info str_global_info) Library "zipapi.dll" alias for "unzGetGlobalInfo;Ansi"
Function integer unzGoToFirstFile (ulong unzFile) Library "zipapi.dll"
Function integer unzGoToNextFile (ulong unzFile) Library "zipapi.dll"
Function integer unzGetCurrentFileInfo (ulong unzFile, Ref unz_fileinfo file_info, Ref string szFileName, ulong filenamesize, Ref string extrafield, &
					ulong extrasize, Ref string comment, ulong cmtsize) Library "zipapi.dll" alias for "unzGetCurrentFileInfo;Ansi"
Function integer unzLocateFile (ulong unzFile, string szFileName, int iCase) Library "zipapi.dll" alias for "unzLocateFile;Ansi"
Function integer unzOpenCurrentFile (ulong unzFile) Library "zipapi.dll"
Function integer unzOpenCurrentFile2 (ulong unzFile, Ref int method, Ref int level, int raw) Library "zipapi.dll"
Function integer unzCloseCurrentFile (ulong unzFile) Library "zipapi.dll"
Function integer unzReadCurrentFile (ulong unzFile, Ref blob data, ulong dataLen) Library "zipapi.dll"
Function ulong zipOpen (Ref string filename, int append) Library "zipapi.dll" alias for "zipOpen;Ansi"
Function integer zipClose (ulong zipFile, Ref string szComment) Library "zipapi.dll" alias for "zipClose;Ansi"
Function integer zipOpenNewFileInZip (ulong file, Ref string filename, Ref zip_fileinfo zipfi, Ref ulong extrafield_local, uint size_extrafield_local, &
					Ref ulong extrafield_global, uint size_extrafield_global, Ref string comment, int method, int level) Library "zipapi.dll" alias for "zipOpenNewFileInZip;Ansi"
Function integer zipOpenNewFileInZip2 (ulong file, Ref string filename, Ref zip_fileinfo zipfi, Ref ulong extrafield_local, uint size_extrafield_local, &
					Ref ulong extrafield_global, uint size_extrafield_global, Ref string comment, int method, int level, int raw) Library "zipapi.dll" alias for "zipOpenNewFileInZip2;Ansi"
Function integer zipWriteInFileInZip (ulong zipFile, Ref blob buffer, long uSizeBuf) Library "zipapi.dll"
Function integer zipCloseFileInZip (ulong zipFile) Library "zipapi.dll"
Function integer zipCloseFileInZipRaw (ulong zipFile, ulong uncompressed_size, ulong crc32) Library "zipapi.dll"

// Windows API functions
Function ulong FindFirstFile (Ref string filename, Ref WIN32_FIND_DATA findfiledata) Library "kernel32.dll" Alias For "FindFirstFileA;Ansi"
Function boolean FindClose (ulong handle) Library "kernel32.dll"
Function boolean FileTimeToSystemTime (Ref FILETIME lpFileTime, Ref SYSTEMTIME lpSystemTime) Library "kernel32.dll" alias for "FileTimeToSystemTime;Ansi"
Function boolean FileTimeToLocalFileTime (Ref FILETIME lpFileTime, Ref FILETIME lpLocalFileTime) Library "kernel32.dll" alias for "FileTimeToLocalFileTime;Ansi"
Function boolean SetCurrentDirectoryA( ref string cdir) library "kernel32.dll"
Function ulong GetTempPathA (long nBufferLength, ref string lpBuffer ) LIBRARY "KERNEL32.DLL"
//Function ulong FindExecutable (ref string lpFile, ref string lpDirectory, ref string lpResult) Library "SHELL32.DLL" Alias for "FindExecutableA"
 Function Long FindExecutable(ref String lpFile, Ref String lpDirectory, ref String lpResult) Library "shell32.dll" Alias For "FindExecutableA"
end prototypes

type variables
CONSTANT Integer Z_OK = 0
CONSTANT Integer Z_STREAM_END = 1
CONSTANT Integer Z_NEED_DICT = 2
CONSTANT Integer Z_ERRNO = -1
CONSTANT Integer Z_STREAM_ERROR = -2
CONSTANT Integer Z_DATA_ERROR = -3
CONSTANT Integer Z_MEM_ERROR = -4
CONSTANT Integer Z_BUF_ERROR = -5
CONSTANT Integer Z_VERSION_ERROR = -6

CONSTANT Integer UNZ_OK = 0
CONSTANT Integer UNZ_END_OF_LIST_OF_FILE = -100
CONSTANT Integer UNZ_PARAMERROR = -102
CONSTANT Integer UNZ_BADZIPFILE = -103
CONSTANT Integer UNZ_INTERNALERROR = -104
CONSTANT Integer UNZ_CRCERROR = -105

CONSTANT Integer ZIP_OK = 0
CONSTANT Integer ZIP_ERRNO = Z_ERRNO
CONSTANT Integer ZIP_PARAMERROR = -102
CONSTANT Integer ZIP_INTERNALERROR = -104

CONSTANT Integer Z_DEFLATED = 8
CONSTANT Integer Z_DEFAULT_COMPRESSION = -1


Transaction	itr_trans

String		is_file_full, is_file, is_tmp_windows
Boolean	ib_Autocommit
end variables

forward prototypes
public function unsignedlong of_unzopen (string as_filename)
public function integer of_unzclose (unsignedlong aul_unzfile)
public function string of_unzgetglobalcomment (unsignedlong aul_unzfile)
public function integer of_unzgotofirstfile (unsignedlong aul_unzfile)
public function integer of_unzgotonextfile (unsignedlong aul_unzfile)
public function integer of_unzgetglobalinfo (unsignedlong aul_unzfile, ref unsignedlong aul_entries, ref unsignedlong aul_cmtsize)
public function integer of_unzlocatefile (unsignedlong aul_unzfile, string as_filename)
public function integer of_unzopencurrentfile (unsignedlong aul_unzfile)
public function integer of_unzclosecurrentfile (unsignedlong aul_unzfile)
public function integer of_unzreadcurrentfile (unsignedlong aul_unzfile, ref blob ablob_data, unsignedlong aul_destlen)
public function boolean of_extractfile (unsignedlong aul_unzfile, string as_filename, string as_filepath)
public function boolean of_compress (blob ablob_source, ref blob ablob_dest)
public function boolean of_compress (string as_source, ref blob ablob_dest)
public function boolean of_uncompress (blob ablob_src, unsignedlong aul_destlen, ref blob ablob_dest)
public function boolean of_uncompress (blob ablob_src, unsignedlong aul_destlen, ref string as_dest)
public function string of_dwdirectory (string as_filename)
public function string of_unzgetcurrentfileinfo (unsignedlong aul_unzfile)
public function integer of_zipclosefileinzip (unsignedlong aul_zipfile)
public function integer of_zipwriteinfileinzip (unsignedlong aul_zipfile, ref blob ablob_data)
public function integer of_zipopennewfileinzip (unsignedlong aul_zipfile, string as_filename)
public function integer of_importfile (unsignedlong aul_zipfile, string as_filename)
public function integer of_zipclose (unsignedlong aul_zipfile, string as_comment)
public function unsignedlong of_zipopen (string as_filename)
public function string of_replaceall (string as_oldstring, string as_findstr, string as_replace)
public function boolean of_getbit (unsignedlong al_decimal, unsignedinteger aui_bit)
public function integer of_internal_fa (string as_extension)
public function string of_zlibversion ()
public function boolean of_compress2 (blob ablob_source, ref blob ablob_dest, integer ai_level)
public function boolean of_compress2 (string as_source, ref blob ablob_dest, integer ai_level)
public function integer of_unzopencurrentfile2 (unsignedlong aul_unzfile, ref integer ai_method, ref integer ai_level)
public function integer of_zipclosefileinzipraw (unsignedlong aul_zipfile, unsignedlong aul_origsize, unsignedlong aul_crc)
public function integer of_zipopennewfileinzip2 (unsignedlong aul_zipfile, string as_filename, integer ai_method, integer ai_level)
public function boolean of_extractblob (unsignedlong aul_unzfile, ref string as_filename, ref blob ablob_data)
public function boolean of_extractblobraw (unsignedlong aul_unzfile, ref string as_filename, ref blob ablob_data, ref unsignedlong aul_origsize, ref unsignedlong aul_crc, ref integer ai_method, ref integer ai_level)
public function boolean f_put_filefromblob (string ls_file, blob lblob_content)
public function string f_getfilefrompath (string cpath)
public function string f_getextfromfile (string ls_file)
public function string f_get_tmp_windows ()
public function blob f_get_file2blob (string ls_archivo)
public subroutine of_graba (string as_archivo)
public subroutine of_lectura (datawindow adw)
end prototypes

public function unsignedlong of_unzopen (string as_filename);// -----------------------------------------------------------------------------
// SCRIPT:     n_ziplib.of_unzOpen
//
// PURPOSE:    This function will open the zip archive file for unzipping.
//
// ARGUMENTS:  as_filename		-	Handle to open zip archive file.
//
// RETURN:     ULong				-	Handle to open zip archive file.
//										-	0 = file not found or unopenable
//										-	See instance constants for error codes
//
// DATE        PROG/ID 		DESCRIPTION OF CHANGE / REASON
// ----------  --------		-------------------------------------------------------
// 12/06/2001	RSmith		Initial creation
// -----------------------------------------------------------------------------

ULong lul_unzFile

lul_unzFile = unzOpen(as_filename)
If lul_unzFile = 0 Then
	MessageBox("Ziplib Error in of_unzOpen", &
			"The zip file is invalid or can't be found!", StopSign!)
End If

Return lul_unzFile

end function

public function integer of_unzclose (unsignedlong aul_unzfile);// -----------------------------------------------------------------------------
// SCRIPT:     n_ziplib.of_unzClose
//
// PURPOSE:    This function will close the zip archive file.
//
// ARGUMENTS:  aul_unzfile		-	Handle to open zip archive file.
//
// RETURN:     Integer			-	0 = Success
//										-	See instance constants for error codes
//
// DATE        PROG/ID 		DESCRIPTION OF CHANGE / REASON
// ----------  --------		-------------------------------------------------------
// 12/06/2001	RSmith		Initial creation
// -----------------------------------------------------------------------------

Integer li_rc

li_rc = unzClose(aul_unzfile)

Return li_rc

end function

public function string of_unzgetglobalcomment (unsignedlong aul_unzfile);// -----------------------------------------------------------------------------
// SCRIPT:     n_ziplib.of_unzGetGlobalComment
//
// PURPOSE:    This function returns the global comment for the
//					zip archive file.
//
// ARGUMENTS:  aul_unzfile		-	Handle to open zip archive file.
//
// RETURN:     String			-	Comment text.
//
// DATE        PROG/ID 		DESCRIPTION OF CHANGE / REASON
// ----------  --------		-------------------------------------------------------
// 12/06/2001	RSmith		Initial creation
// -----------------------------------------------------------------------------

String ls_comment
Ulong lul_sizebuf
Integer li_rc

lul_sizebuf = 500
ls_comment = Space(lul_sizebuf)

li_rc = unzGetGlobalComment(aul_unzfile, ls_comment, lul_sizebuf)

Return ls_comment

end function

public function integer of_unzgotofirstfile (unsignedlong aul_unzfile);// -----------------------------------------------------------------------------
// SCRIPT:     n_ziplib.of_unzGotoFirstFile
//
// PURPOSE:    This function makes the first file in the zip archive
//					the current file.
//
// ARGUMENTS:  aul_unzfile		-	Handle to open zip archive file.
//
// RETURN:     Integer			-	0 = Success
//										-	See instance constants for error codes
//
// DATE        PROG/ID 		DESCRIPTION OF CHANGE / REASON
// ----------  --------		-------------------------------------------------------
// 12/06/2001	RSmith		Initial creation
// -----------------------------------------------------------------------------

Integer li_rc

li_rc = unzGoToFirstFile(aul_unzfile)
CHOOSE CASE li_rc
	CASE UNZ_PARAMERROR
		MessageBox("Ziplib Error in of_unzGotoFirstFile", &
				"Parameter Error!", StopSign!)
	CASE UNZ_BADZIPFILE
		MessageBox("Ziplib Error in of_unzGotoFirstFile", &
				"Bad zip file!", StopSign!)
	CASE UNZ_INTERNALERROR
		MessageBox("Ziplib Error in of_unzGotoFirstFile", &
				"Internal Error!", StopSign!)
	CASE UNZ_CRCERROR
		MessageBox("Ziplib Error in of_unzGotoFirstFile", &
				"CRC Error!", StopSign!)
END CHOOSE

Return li_rc

end function

public function integer of_unzgotonextfile (unsignedlong aul_unzfile);// -----------------------------------------------------------------------------
// SCRIPT:     n_ziplib.of_unzGotoNextFile
//
// PURPOSE:    This function makes the next file in the zip archive
//					the current file.
//
// ARGUMENTS:  aul_unzfile		-	Handle to open zip archive file.
//
// RETURN:     Integer			-	0 = Success
//										-	See instance constants for error codes
//
// DATE        PROG/ID 		DESCRIPTION OF CHANGE / REASON
// ----------  --------		-------------------------------------------------------
// 12/06/2001	RSmith		Initial creation
// -----------------------------------------------------------------------------

Integer li_rc

li_rc = unzGoToNextFile(aul_unzfile)
CHOOSE CASE li_rc
	CASE UNZ_PARAMERROR
		MessageBox("Ziplib Error in of_unzGotoNextFile", &
				"Parameter Error!", StopSign!)
	CASE UNZ_BADZIPFILE
		MessageBox("Ziplib Error in of_unzGotoNextFile", &
				"Bad zip file!", StopSign!)
	CASE UNZ_INTERNALERROR
		MessageBox("Ziplib Error in of_unzGotoNextFile", &
				"Internal Error!", StopSign!)
	CASE UNZ_CRCERROR
		MessageBox("Ziplib Error in of_unzGotoNextFile", &
				"CRC Error!", StopSign!)
END CHOOSE

Return li_rc

end function

public function integer of_unzgetglobalinfo (unsignedlong aul_unzfile, ref unsignedlong aul_entries, ref unsignedlong aul_cmtsize);// -----------------------------------------------------------------------------
// SCRIPT:     n_ziplib.of_unzGetGlobalInfo
//
// PURPOSE:    This function returns the number of entries and
//					global comment length for the zip archive file.
//
// ARGUMENTS:  aul_unzfile		-	Handle to open zip archive file.
//					aul_entries		-	Number of entries (output).
//					aul_cmtsize		-	Length of global comment (output).
//
// RETURN:     Integer			-	0 = Success
//										-	See instance constants for error codes
//
// DATE        PROG/ID 		DESCRIPTION OF CHANGE / REASON
// ----------  --------		-------------------------------------------------------
// 12/06/2001	RSmith		Initial creation
// -----------------------------------------------------------------------------

Integer li_rc
unz_global_info lstr_info

li_rc = unzGetGlobalInfo(aul_unzfile, lstr_info)

aul_entries = lstr_info.number_entry
aul_cmtsize = lstr_info.size_comment

Return li_rc

end function

public function integer of_unzlocatefile (unsignedlong aul_unzfile, string as_filename);// -----------------------------------------------------------------------------
// SCRIPT:     n_ziplib.of_unzGotoFirstFile
//
// PURPOSE:    This function makes the passed file the current
//					file in the zip archive.
//
// ARGUMENTS:  aul_unzfile		-	Handle to open zip archive file.
//					as_filename		-	Name of file to make current.
//
// RETURN:     Integer			-	0 = Success
//										-	See instance constants for error codes
//
// DATE        PROG/ID 		DESCRIPTION OF CHANGE / REASON
// ----------  --------		-------------------------------------------------------
// 12/06/2001	RSmith		Initial creation
// -----------------------------------------------------------------------------

Integer li_rc

li_rc = unzLocateFile(aul_unzfile, as_filename, 2)

Return li_rc

end function

public function integer of_unzopencurrentfile (unsignedlong aul_unzfile);// -----------------------------------------------------------------------------
// SCRIPT:     n_ziplib.of_unzOpenCurrentFile
//
// PURPOSE:    This function opens the current file in the zip archive file.
//
// ARGUMENTS:  aul_unzfile		-	Handle to open zip archive file.
//
// RETURN:     Integer			-	0 = Success
//										-	See instance constants for error codes
//
// DATE        PROG/ID 		DESCRIPTION OF CHANGE / REASON
// ----------  --------		-------------------------------------------------------
// 12/06/2001	RSmith		Initial creation
// -----------------------------------------------------------------------------

Integer li_rc

li_rc = unzOpenCurrentFile(aul_unzfile)

Return li_rc

end function

public function integer of_unzclosecurrentfile (unsignedlong aul_unzfile);// -----------------------------------------------------------------------------
// SCRIPT:     n_ziplib.of_unzClose
//
// PURPOSE:    This function will close the open file within a zip archive file.
//
// ARGUMENTS:  aul_unzfile		-	Handle to open zip archive file.
//
// RETURN:     Integer			-	0 = Success
//										-	See instance constants for error codes
//
// DATE        PROG/ID 		DESCRIPTION OF CHANGE / REASON
// ----------  --------		-------------------------------------------------------
// 12/06/2001	RSmith		Initial creation
// -----------------------------------------------------------------------------

Integer li_rc

li_rc = unzCloseCurrentFile(aul_unzfile)

Return li_rc

end function

public function integer of_unzreadcurrentfile (unsignedlong aul_unzfile, ref blob ablob_data, unsignedlong aul_destlen);// -----------------------------------------------------------------------------
// SCRIPT:     n_ziplib.of_unzReadCurrentFile
//
// PURPOSE:    This function uncompresses the current file in the zip archive
//					file and places it in a blob variable.
//
// ARGUMENTS:  aul_unzfile		-	Handle to open zip archive file.
//					ablob_data		-	Blob to contain file (output).
//					aul_destlen		-	The uncompressed size of the file.
//
// RETURN:     Integer			-	0 = Success
//										-	See instance constants for error codes
//
// DATE        PROG/ID 		DESCRIPTION OF CHANGE / REASON
// ----------  --------		-------------------------------------------------------
// 12/06/2001	RSmith		Initial creation
// -----------------------------------------------------------------------------

Integer li_rc
Blob lblob_data
ULong lul_next
UInt lui_datalen

// allocate buffer space
lui_datalen = 8192
lblob_data = Blob(Space(lui_datalen))
ablob_data = Blob(Space(aul_destlen))
lul_next = 1

// read file 8k bytes at a time until done
li_rc = unzReadCurrentFile(aul_unzfile, lblob_data, lui_datalen)
DO WHILE li_rc > 0
	// concatenate to output blob
	lul_next = BlobEdit(ablob_data, lul_next, BlobMid(lblob_data, 1, li_rc))
	// read next 8k bytes
	li_rc = unzReadCurrentFile(aul_unzfile, lblob_data, lui_datalen)
LOOP

Return li_rc

end function

public function boolean of_extractfile (unsignedlong aul_unzfile, string as_filename, string as_filepath);// -----------------------------------------------------------------------------
// SCRIPT:     n_ziplib.of_ExtractFile
//
// PURPOSE:    This function will extract a file from the zip archive and
//             save it to disk.  The zip archive must have already been
//					opened using the using the of_unzOpen function.
//
// ARGUMENTS:  aul_unzfile	-	Handle of currently open zip archive
//					as_filename	-	Name of file within the archive
//					as_filepath	-	Name of file to write to disk
//
// RETURN:     Boolean		-	True = Success, False = Error
//
// DATE        PROG/ID 		DESCRIPTION OF CHANGE / REASON
// ----------  --------		-------------------------------------------------------
// 12/06/2001	RSmith		Initial creation
// 08/12/2003	RSmith		If as_filename is blank, extract current file
// -----------------------------------------------------------------------------

Integer li_rc, li_fnum
Blob lblob_data
ULong lul_datalen

// locate the file within the archive
If Len(as_filename) > 0 Then
	li_rc = unzLocateFile(aul_unzfile, as_filename, 2)
	If li_rc < 0 Then
		MessageBox("Ziplib Error in of_ExtractFile", &
				"Unable to locate file '" + as_filename + "' within archive!", StopSign!)
		Return False
	End If
End If

// open the current file
li_rc = unzOpenCurrentFile(aul_unzfile)

// allocate buffer space
lul_datalen = 8192
lblob_data = Blob(Space(lul_datalen))

// open file
li_fnum = FileOpen(as_filepath, StreamMode!, Write!, LockReadWrite!, Replace!)
If li_fnum > 0 Then
	// read file 8k bytes at a time until done
	li_rc = unzReadCurrentFile(aul_unzfile, lblob_data, lul_datalen)
	DO WHILE li_rc > 0
		// write data to file
		li_rc = FileWrite(li_fnum, BlobMid(lblob_data, 1, li_rc))
		// read next 8k bytes
		li_rc = unzReadCurrentFile(aul_unzfile, lblob_data, lul_datalen)
	LOOP
	// close file
	FileClose(li_fnum)
Else
	MessageBox("Ziplib Error in of_ExtractFile", &
			"Unable to open the output file!", StopSign!)
	Return False
End If

// close the current file
li_rc = unzCloseCurrentFile(aul_unzfile)

Return True

end function

public function boolean of_compress (blob ablob_source, ref blob ablob_dest);// -----------------------------------------------------------------------------
// SCRIPT:     n_ziplib.of_Compress
//
// PURPOSE:    This function will compress the passed blob.
//
// ARGUMENTS:  ablob_source	-	Input blob variable
//					ablob_dest		-	Output blob variable
//
// RETURN:     Boolean		-	True = Success, False = Error
//
// DATE        PROG/ID 		DESCRIPTION OF CHANGE / REASON
// ----------  --------		-------------------------------------------------------
// 12/06/2001	RSmith		Initial creation
// -----------------------------------------------------------------------------

Integer li_rc
ULong lul_srclen, lul_destlen
Blob lblob_dest

// allocate buffer space
lul_srclen  = Len(ablob_source)
lul_destlen = (lul_srclen * 1.01) + 12  // zlib needs a little extra room
lblob_dest = Blob(Space(lul_destlen))
// compress the blob
li_rc = compress(lblob_dest, lul_destlen, ablob_source, lul_srclen)

CHOOSE CASE li_rc
	CASE Z_OK
		ablob_dest = BlobMid(lblob_dest, 1, lul_destlen)
	CASE Z_MEM_ERROR
		SetNull(ablob_dest)
		MessageBox("Ziplib Error in of_Compress", &
				"Not enough memory!", StopSign!)
		Return False
	CASE Z_BUF_ERROR
		SetNull(ablob_dest)
		MessageBox("Ziplib Error in of_Compress", &
				"Not enough room in the output buffer!", StopSign!)
		Return False
	CASE ELSE
		SetNull(ablob_dest)
		MessageBox("Ziplib Error in of_Compress", &
				"Undefined error: " + String(li_rc), StopSign!)
		Return False
END CHOOSE

Return True

end function

public function boolean of_compress (string as_source, ref blob ablob_dest);// -----------------------------------------------------------------------------
// SCRIPT:     n_ziplib.of_Compress
//
// PURPOSE:    This function will compress the passed string.
//
// ARGUMENTS:  as_source		-	Input string variable
//					ablob_dest		-	Output blob variable
//
// RETURN:     Boolean		-	True = Success, False = Error
//
// DATE        PROG/ID 		DESCRIPTION OF CHANGE / REASON
// ----------  --------		-------------------------------------------------------
// 12/06/2001	RSmith		Initial creation
// -----------------------------------------------------------------------------

Integer li_rc
ULong lul_srclen, lul_destlen
Blob lblob_src, lblob_dest

// convert string to blob
lblob_src  = Blob(as_source)

// allocate buffer space
lul_srclen  = Len(as_source)
lul_destlen = (lul_srclen * 1.01) + 12  // zlib needs a little extra room
lblob_dest = Blob(Space(lul_destlen))

// compress the blob
li_rc = compress(lblob_dest, lul_destlen, lblob_src, lul_srclen)
CHOOSE CASE li_rc
	CASE Z_OK
		ablob_dest = BlobMid(lblob_dest, 1, lul_destlen)
	CASE Z_MEM_ERROR
		SetNull(ablob_dest)
		MessageBox("Ziplib Error in of_Compress", &
				"Not enough memory!", StopSign!)
		Return False
	CASE Z_BUF_ERROR
		SetNull(ablob_dest)
		MessageBox("Ziplib Error in of_Compress", &
				"Not enough room in the output buffer!", StopSign!)
		Return False
	CASE ELSE
		SetNull(ablob_dest)
		MessageBox("Ziplib Error in of_Compress", &
				"Undefined error: " + String(li_rc), StopSign!)
		Return False
END CHOOSE

Return True

end function

public function boolean of_uncompress (blob ablob_src, unsignedlong aul_destlen, ref blob ablob_dest);// -----------------------------------------------------------------------------
// SCRIPT:     n_ziplib.of_UnCompress
//
// PURPOSE:    This function will uncompress the passed blob to a blob.
//
// ARGUMENTS:  ablob_source	-	Input blob variable
//					aul_destlen		-	Original length of the input
//					ablob_dest		-	Output blob variable
//
// RETURN:     Boolean		-	True = Success, False = Error
//
// DATE        PROG/ID 		DESCRIPTION OF CHANGE / REASON
// ----------  --------		-------------------------------------------------------
// 12/06/2001	RSmith		Initial creation
// -----------------------------------------------------------------------------

Integer li_rc
ULong lul_srclen
Blob lblob_dest

// allocate buffer space
lblob_dest = Blob(Space(aul_destlen))
lul_srclen = Len(ablob_src)

// uncompress the blob
li_rc = uncompress(lblob_dest, aul_destlen, ablob_src, lul_srclen)
CHOOSE CASE li_rc
	CASE Z_OK
		ablob_dest = lblob_dest
	CASE Z_MEM_ERROR
		SetNull(ablob_dest)
		MessageBox("Ziplib Error in of_UnCompress", &
				"Not enough memory!", StopSign!)
		Return False
	CASE Z_BUF_ERROR
		SetNull(ablob_dest)
		MessageBox("Ziplib Error in of_UnCompress", &
				"Not enough room in the output buffer!", StopSign!)
		Return False
	CASE Z_DATA_ERROR
		SetNull(ablob_dest)
		MessageBox("Ziplib Error in of_UnCompress", &
				"The input data was corrupted!", StopSign!)
		Return False
	CASE ELSE
		SetNull(ablob_dest)
		MessageBox("Ziplib Error in of_UnCompress", &
				"Undefined error: " + String(li_rc), StopSign!)
		Return False
END CHOOSE

Return True

end function

public function boolean of_uncompress (blob ablob_src, unsignedlong aul_destlen, ref string as_dest);// -----------------------------------------------------------------------------
// SCRIPT:     n_ziplib.of_UnCompress
//
// PURPOSE:    This function will uncompress the passed blob to a string.
//
// ARGUMENTS:  ablob_source	-	Input blob variable
//					aul_destlen		-	Original length of the input
//					as_dest			-	Output string variable
//
// RETURN:     Boolean		-	True = Success, False = Error
//
// DATE        PROG/ID 		DESCRIPTION OF CHANGE / REASON
// ----------  --------		-------------------------------------------------------
// 12/06/2001	RSmith		Initial creation
// -----------------------------------------------------------------------------

Integer li_rc
ULong lul_srclen
Blob lblob_dest

// allocate buffer space
lblob_dest = Blob(Space(aul_destlen))
lul_srclen = Len(ablob_src)

// uncompress the blob
li_rc = uncompress(lblob_dest, aul_destlen, ablob_src, lul_srclen)
CHOOSE CASE li_rc
	CASE Z_OK
		as_dest = String(lblob_dest)
	CASE Z_MEM_ERROR
		SetNull(as_dest)
		MessageBox("Ziplib Error in of_UnCompress", &
				"Not enough memory!", StopSign!)
		Return False
	CASE Z_BUF_ERROR
		SetNull(as_dest)
		MessageBox("Ziplib Error in of_UnCompress", &
				"Not enough room in the output buffer!", StopSign!)
		Return False
	CASE Z_DATA_ERROR
		SetNull(as_dest)
		MessageBox("Ziplib Error in of_UnCompress", &
				"The input data was corrupted!", StopSign!)
		Return False
	CASE ELSE
		SetNull(as_dest)
		MessageBox("Ziplib Error in of_UnCompress", &
				"Undefined error: " + String(li_rc), StopSign!)
		Return False
END CHOOSE

Return True

end function

public function string of_dwdirectory (string as_filename);// -----------------------------------------------------------------------------
// SCRIPT:     n_ziplib.of_DWDirectory
//
// PURPOSE:    This function will build a datawindow import string containing
//					the directory of the specified zip archive file.
//
// ARGUMENTS:  as_filename		-	Full path/file name of zip archive file.
//
// RETURN:     String			-	Datawindow import string
//
// DATE        PROG/ID 		DESCRIPTION OF CHANGE / REASON
// ----------  --------		-------------------------------------------------------
// 12/06/2001	RSmith		Initial creation
// -----------------------------------------------------------------------------

ULong lul_unzFile
Integer li_rc
String ls_dwdata, ls_rowdata

// open the zip archive
lul_unzFile = this.of_unzOpen(as_filename)

If lul_unzFile > 0 Then
	// walk thru each file in the zip archive
	li_rc = this.of_unzGoToFirstFile(lul_unzFile)
	DO WHILE li_rc = 0
		ls_rowdata = this.of_unzGetCurrentFileInfo(lul_unzFile)
		If Len(ls_rowdata) > 0 Then
			If Len(ls_dwdata) = 0 Then
				ls_dwdata = ls_rowdata
			Else
				ls_dwdata = ls_dwdata + "~r~n" + ls_rowdata
			End If
		End If
		li_rc = this.of_unzGoToNextFile(lul_unzFile)
	LOOP
	// close the zip archive
	this.of_unzClose(lul_unzFile)
Else
	MessageBox("Ziplib Error in of_DWDirectory", &
			"Unable to open the zip archive file!", StopSign!)
	Return ""
End If

Return ls_dwdata

end function

public function string of_unzgetcurrentfileinfo (unsignedlong aul_unzfile);// -----------------------------------------------------------------------------
// SCRIPT:     n_ziplib.of_unzGetCurrentFileInfo
//
// PURPOSE:    This function returns directory information about the
//					current file in the zip archive file.
//
// ARGUMENTS:  aul_unzfile		-	Handle to open zip archive file.
//					as_dwdata		-	Datawindow import string (output)
//
// RETURN:     Integer			-	0 = Success
//										-	See instance constants for error codes
//
// DATE        PROG/ID 		DESCRIPTION OF CHANGE / REASON
// ----------  --------		-----------------------------------------------------
// 12/06/2001	RSmith		Initial creation
// 01/07/2003	RSmith		Updated build of datetime so would work outside USA
// -----------------------------------------------------------------------------

Integer li_rc
String ls_fullname, ls_extra, ls_cmts, ls_datetime, ls_filepath, ls_temp
String ls_filename, ls_filetype, ls_regpath, ls_regkey, ls_dwdata, ls_attrib
ULong lul_fname, lul_extra, lul_cmts
Decimal{4} ldec_ratio
Long ll_pos, ll_pos1
Date ld_date
Time lt_time
unz_fileinfo lstr_info

// initialize buffers
lul_fname = 256
ls_fullname  = Space(lul_fname)
lul_extra = 0
ls_extra  = Space(lul_extra)
lul_cmts  = 500
ls_cmts   = Space(lul_cmts)

// get file info
li_rc = unzGetCurrentFileInfo(aul_unzfile, lstr_info, ls_fullname, &
							lul_fname, ls_extra, lul_extra, ls_cmts, lul_cmts)

// extract filename/filepath from full path
ls_temp = this.of_replaceall(ls_fullname, "/", "\")
ll_pos1 = Pos(ls_temp, "\")
DO While ll_pos1 > 0
	ll_pos1 = Pos(Mid(ls_temp,ll_pos1,Len(ls_temp) - ll_pos1 + 1), "\")
	ll_pos = ll_pos + ll_pos1
LOOP

If ll_pos = 0 Then
	ls_filename = ls_temp
	ls_filepath = ""
Else
	ls_filename = Mid(ls_fullname, ll_pos + 1)
	ls_filepath = Left(ls_temp, ll_pos)
End If

// don't include directories in datawindow data
If ls_filename = "" Then Return ""

// determine filetype
ll_pos = 0
ll_pos1 = Pos(ls_filename, ".")
DO While ll_pos1 > 0
	ll_pos1 = Pos(Mid(ls_filename,ll_pos1,Len(ls_temp) - ll_pos1 + 1), ".")
	ll_pos = ll_pos + ll_pos1
LOOP

If ll_pos = 0 Then
	ls_filetype = "File"
Else
	ls_filetype = Lower(Mid(ls_filename, ll_pos))
	ls_regpath = "HKEY_CLASSES_ROOT\" + ls_filetype
	RegistryGet(ls_regpath, "", ls_regkey)
	If ls_regkey = "" Then
		ls_filetype = Upper(Mid(ls_filename, ll_pos + 1)) + " File"
	Else
		ls_regpath = "HKEY_CLASSES_ROOT\" + ls_regkey
		RegistryGet(ls_regpath, "", ls_filetype)
		If ls_filetype = "" Then
			ls_filetype = Upper(Mid(ls_filename, ll_pos + 1)) + " File"
		End If
	End If
End If

// calc ratio
If lstr_info.uncompressed_size > 0 Then
	If lstr_info.compressed_size = lstr_info.uncompressed_size Then
		ldec_ratio = 0
	Else
		ldec_ratio = lstr_info.compressed_size / lstr_info.uncompressed_size
	End If
Else
	ldec_ratio = 0
End If

// build datetime
ld_date = Date(lstr_info.tmu_date.tm_year, &
					lstr_info.tmu_date.tm_mon + 1, &
					lstr_info.tmu_date.tm_mday)
lt_time = Time(lstr_info.tmu_date.tm_hour, &
					lstr_info.tmu_date.tm_min, &
					lstr_info.tmu_date.tm_sec)
ls_datetime = String(DateTime(ld_date, lt_time))

// build file attributes
ls_attrib = ""
If this.of_getbit(lstr_info.external_fa, 1) Then
	ls_attrib += "R"
End If
If this.of_getbit(lstr_info.external_fa, 2) Then
	ls_attrib += "H"
End If
If this.of_getbit(lstr_info.external_fa, 3) Then
	ls_attrib += "S"
End If
If this.of_getbit(lstr_info.external_fa, 6) Then
	ls_attrib += "A"
End If

// build datawindow import string
ls_dwdata  = Trim(ls_filename) + "~t"
ls_dwdata += Trim(ls_filetype) + "~t"
ls_dwdata += ls_datetime + "~t"
ls_dwdata += String(lstr_info.uncompressed_size) + "~t"
ls_dwdata += String(ldec_ratio) + "~t"
ls_dwdata += String(lstr_info.compressed_size) + "~t"
ls_dwdata += Trim(ls_attrib) + "~t"
ls_dwdata += Trim(ls_filepath) + "~t"
ls_dwdata += Trim(ls_fullname)

Return ls_dwdata

end function

public function integer of_zipclosefileinzip (unsignedlong aul_zipfile);// -----------------------------------------------------------------------------
// SCRIPT:     n_ziplib.of_zipCloseFileInZip
//
// PURPOSE:    This function will close a new file in the zip archive file.
//
// ARGUMENTS:  aul_zipfile		-	Handle to open zip archive file.
//
// RETURN:     Integer			-	0 = Success
//										-	See instance constants for error codes
//
// DATE        PROG/ID 		DESCRIPTION OF CHANGE / REASON
// ----------  --------		-------------------------------------------------------
// 10/05/2002	RSmith		Initial creation
// -----------------------------------------------------------------------------

Integer li_rc

li_rc = zipCloseFileInZip(aul_zipfile)

Return li_rc

end function

public function integer of_zipwriteinfileinzip (unsignedlong aul_zipfile, ref blob ablob_data);// -----------------------------------------------------------------------------
// SCRIPT:     n_ziplib.of_zipWriteInFileInZip
//
// PURPOSE:    This function will write a new file to the zip archive file.
//
// ARGUMENTS:  aul_zipfile		-	Handle to open zip archive file.
//
// RETURN:     Integer			-	0 = Success
//										-	See instance constants for error codes
//
// DATE        PROG/ID 		DESCRIPTION OF CHANGE / REASON
// ----------  --------		-------------------------------------------------------
// 10/05/2002	RSmith		Initial creation
// -----------------------------------------------------------------------------

Integer li_rc
Long ll_sizebuf

ll_sizebuf = Len(ablob_data)

li_rc = zipWriteInFileInZip(aul_zipfile, ablob_data, ll_sizebuf)

Return li_rc

end function

public function integer of_zipopennewfileinzip (unsignedlong aul_zipfile, string as_filename);// -----------------------------------------------------------------------------
// SCRIPT:     n_ziplib.of_zipOpenNewFileInZip
//
// PURPOSE:    This function will add a new file to the zip archive file.
//
// ARGUMENTS:  aul_zipfile		-	Handle to open zip archive file.
//					as_filename		-	Name of file being added to archive.
//
// RETURN:     Integer			-	0 = Success
//										-	See instance constants for error codes
//
// DATE        PROG/ID 		DESCRIPTION OF CHANGE / REASON
// ----------  --------		-------------------------------------------------------
// 10/05/2002	RSmith		Initial creation
// -----------------------------------------------------------------------------

Integer li_rc
ULong lul_Handle, lul_null
zip_fileinfo lstr_fileinfo
WIN32_FIND_DATA lstr_FD
FILETIME lstr_LT
SYSTEMTIME lstr_ST
String ls_cmt, ls_filename

SetNull(ls_cmt)
SetNull(lul_null)

// Get the file information
lul_Handle = FindFirstFile(as_filename, lstr_FD)
If lul_Handle <= 0 Then Return -1
FindClose(lul_Handle)

// Value of 1 indicates text file, 0 indicates binary
lstr_fileinfo.internal_fa = this.of_Internal_FA(Right(as_filename, 3))

// get file attributes (Read-Only, Hidden, System, Archive)
lstr_fileinfo.external_fa = lstr_FD.dwfileattributes

// convert create datetime to usable format
FileTimeToLocalFileTime(lstr_FD.ftlastwritetime, lstr_LT)
FileTimeToSystemTime(lstr_LT, lstr_ST)

// remove drive letter
If Pos(as_filename, ":\") = 2 Then
	as_filename = Mid(as_filename, 4)
End If

// .zip standard is to use forward slash
ls_filename = this.of_replaceall(as_filename, "\", "/")

// copy datetime to structure
lstr_fileinfo.tmz_date.tm_mon = lstr_ST.wMonth - 1
lstr_fileinfo.tmz_date.tm_mday = lstr_ST.wDay
lstr_fileinfo.tmz_date.tm_year = lstr_ST.wYear
lstr_fileinfo.tmz_date.tm_hour = lstr_ST.wHour
lstr_fileinfo.tmz_date.tm_min = lstr_ST.wMinute
lstr_fileinfo.tmz_date.tm_sec = lstr_ST.wSecond

li_rc = zipOpenNewFileInZip(aul_zipfile, ls_filename, lstr_fileinfo, &
					lul_null, 0, lul_null, 0, ls_cmt, Z_DEFLATED, Z_DEFAULT_COMPRESSION)

Return li_rc

end function

public function integer of_importfile (unsignedlong aul_zipfile, string as_filename);// -----------------------------------------------------------------------------
// SCRIPT:     n_ziplib.of_ImportFile
//
// PURPOSE:    This function will add a new file to the zip archive file.
//
// ARGUMENTS:  aul_zipfile		-	Handle to open zip archive file.
//					as_filename		-	Name of file being added to archive.
//
// RETURN:     Integer			-	0 = Success
//										-	See instance constants for error codes
//
// DATE        PROG/ID 		DESCRIPTION OF CHANGE / REASON
// ----------  --------		-------------------------------------------------------
// 10/05/2002	RSmith		Initial creation
// -----------------------------------------------------------------------------

Integer li_rc, li_fnum
Blob lblob_file

li_fnum = FileOpen(as_filename, StreamMode!, Read!, Shared!)
If li_fnum > 0 Then
	li_rc = this.of_zipOpenNewFileInZip(aul_zipFile, as_filename)
	If li_rc = 0 Then
		DO WHILE FileRead(li_fnum, lblob_file) > 0
			li_rc = this.of_zipWriteInFileInZip(aul_zipFile, lblob_file)
		LOOP
		li_rc = this.of_zipCloseFileInZip(aul_zipFile)
	End If
	FileClose(li_fnum)
Else
	li_rc = -1
End If

Return li_rc

end function

public function integer of_zipclose (unsignedlong aul_zipfile, string as_comment);// -----------------------------------------------------------------------------
// SCRIPT:     n_ziplib.of_zipClose
//
// PURPOSE:    This function will close the zip archive file.
//
// ARGUMENTS:  aul_zipfile		-	Handle to open zip archive file.
//					as_comment		-	Any global comments to place in file.
//
// RETURN:     Integer			-	0 = Success
//										-	See instance constants for error codes
//
// DATE        PROG/ID 		DESCRIPTION OF CHANGE / REASON
// ----------  --------		-------------------------------------------------------
// 10/05/2002	RSmith		Initial creation
// -----------------------------------------------------------------------------

Integer li_rc
String ls_cmt

li_rc = zipClose(aul_zipfile, as_comment)

Return li_rc

end function

public function unsignedlong of_zipopen (string as_filename);// -----------------------------------------------------------------------------
// SCRIPT:     n_ziplib.of_zipOpen
//
// PURPOSE:    This function will open a zip archive file for adding files.
//
// ARGUMENTS:  as_filename		-	Handle to open zip archive file.
//
// RETURN:     ULong				-	Handle to open zip archive file.
//										-	0 = file not found or unopenable
//										-	See instance constants for error codes
//
// DATE        PROG/ID 		DESCRIPTION OF CHANGE / REASON
// ----------  --------		-------------------------------------------------------
// 10/05/2002	RSmith		Initial creation
// -----------------------------------------------------------------------------

ULong lul_zipFile

// open zip file
lul_zipFile = zipOpen(as_filename, 0)
If lul_zipFile = 0 Then
	MessageBox("Ziplib Error in of_zipOpen", &
			"The zip file could not be opened!", StopSign!)
End If

Return lul_zipFile

end function

public function string of_replaceall (string as_oldstring, string as_findstr, string as_replace);String ls_newstring
Long ll_findstr, ll_replace, ll_pos

// get length of strings
ll_findstr = Len(as_findstr)
ll_replace = Len(as_replace)

// find first occurrence
ls_newstring = as_oldstring
ll_pos = Pos(ls_newstring, as_findstr)

Do While ll_pos > 0
	// replace old with new
	ls_newstring = Replace(ls_newstring, ll_pos, ll_findstr, as_replace)
	// find next occurrence
	ll_pos = Pos(ls_newstring, as_findstr, (ll_pos + ll_replace))
Loop

Return ls_newstring

end function

public function boolean of_getbit (unsignedlong al_decimal, unsignedinteger aui_bit);// return whether specified bit is on or off

Boolean lb_null

If Int(Mod(al_decimal / (2 ^(aui_bit - 1)), 2)) > 0 Then
	Return True
End If

Return False

end function

public function integer of_internal_fa (string as_extension);// -----------------------------------------------------------------------------
// SCRIPT:     n_ziplib.of_Internal_FA
//
// PURPOSE:    This function will determine if the file is likely to be a
//					text file or a binary file.  This is used for the internal_fa
//					property when adding a file to a .zip archive.  It is only
//					for documentation, it doesn't need to be accurate.
//
// ARGUMENTS:  as_extension	-	Three letter file extension
//
// RETURN:     Integer			-	1 indicates text file, 0 indicates binary
//
// DATE        PROG/ID 		DESCRIPTION OF CHANGE / REASON
// ----------  --------		-------------------------------------------------------
// 12/06/2001	RSmith		Initial creation
// -----------------------------------------------------------------------------

Integer li_rtn
String ls_regkey, ls_regpath, ls_command

CHOOSE CASE Upper(as_extension)
	CASE "TXT", "BAT", "INI", "HTM", "HTML"
		li_rtn = 1	// file is text
	CASE "COM", "DLL", "EXE", "DOC", "XLS", "PBL", "PBD"
		li_rtn = 0	// file is binary
	CASE ELSE
		// see if extension is assigned to Notepad or Wordpad
		ls_regpath = "HKEY_CLASSES_ROOT\." + Lower(as_extension)
		RegistryGet(ls_regpath, "", ls_regkey)
		If Len(ls_regkey) = 0 Then
			ls_regkey = Lower(as_extension) + "_auto_file"
		End If
		If Len(ls_regkey) > 0 Then
			ls_regpath = "HKEY_CLASSES_ROOT\" + ls_regkey + "\shell\open\command"
			RegistryGet(ls_regpath, "", ls_command)
			If Len(ls_command) > 0 Then
				If Pos(Lower(ls_command), "notepad.exe") > 1 Or &
					Pos(Lower(ls_command), "wordpad.exe") > 1 Then
					li_rtn = 1	// file is text
				End If
			End If
		End If
END CHOOSE

Return li_rtn

end function

public function string of_zlibversion ();// -----------------------------------------------------------------------------
// SCRIPT:     n_ziplib.of_zlibVersion
//
// PURPOSE:    This function will return the zlib version.
//
// RETURN:     Striing			-	Version text.
//
// DATE        PROG/ID 		DESCRIPTION OF CHANGE / REASON
// ----------  --------		-------------------------------------------------------
// 08/11/2003	RSmith		Initial creation
// -----------------------------------------------------------------------------

String ls_version

ls_version = Space(20)

ls_version = zlibVersion()

Return ls_version

end function

public function boolean of_compress2 (blob ablob_source, ref blob ablob_dest, integer ai_level);// -----------------------------------------------------------------------------
// SCRIPT:     n_ziplib.of_Compress2
//
// PURPOSE:    This function will compress the passed blob.
//
// ARGUMENTS:  ablob_source	-	Input blob variable
//					ablob_dest		-	Output blob variable
//					ai_level			-	Compression level
//											  -1 = Use Default Level
//												0 = None
//												1 = Best Speed
//												9 = Best Compression
//
// RETURN:     Boolean		-	True = Success, False = Error
//
// DATE        PROG/ID 		DESCRIPTION OF CHANGE / REASON
// ----------  --------		-------------------------------------------------------
// 08/11/2003	RSmith		Initial creation
// -----------------------------------------------------------------------------

Integer li_rc
ULong lul_srclen, lul_destlen
Blob lblob_dest

// allocate buffer space
lul_srclen  = Len(ablob_source)
lul_destlen = (lul_srclen * 1.01) + 12  // zlib needs a little extra room
lblob_dest = Blob(Space(lul_destlen))

// compress the blob
li_rc = compress2(lblob_dest, lul_destlen, ablob_source, lul_srclen, ai_level)
CHOOSE CASE li_rc
	CASE Z_OK
		ablob_dest = BlobMid(lblob_dest, 1, lul_destlen)
	CASE Z_MEM_ERROR
		SetNull(ablob_dest)
		MessageBox("Ziplib Error in of_Compress2", &
				"Not enough memory!", StopSign!)
		Return False
	CASE Z_BUF_ERROR
		SetNull(ablob_dest)
		MessageBox("Ziplib Error in of_Compress2", &
				"Not enough room in the output buffer!", StopSign!)
		Return False
	CASE ELSE
		SetNull(ablob_dest)
		MessageBox("Ziplib Error in of_Compress2", &
				"Undefined error: " + String(li_rc), StopSign!)
		Return False
END CHOOSE

Return True

end function

public function boolean of_compress2 (string as_source, ref blob ablob_dest, integer ai_level);// -----------------------------------------------------------------------------
// SCRIPT:     n_ziplib.of_Compress2
//
// PURPOSE:    This function will compress the passed string.
//
// ARGUMENTS:  as_source		-	Input string variable
//					ablob_dest		-	Output blob variable
//					ai_level			-	Compression level
//											  -1 = Use Default Level
//												0 = None
//												1 = Best Speed
//												9 = Best Compression
//
// RETURN:     Boolean		-	True = Success, False = Error
//
// DATE        PROG/ID 		DESCRIPTION OF CHANGE / REASON
// ----------  --------		-------------------------------------------------------
// 08/11/2003	RSmith		Initial creation
// -----------------------------------------------------------------------------

Integer li_rc
ULong lul_srclen, lul_destlen
Blob lblob_src, lblob_dest

// convert string to blob
lblob_src  = Blob(as_source)

// allocate buffer space
lul_srclen  = Len(as_source)
lul_destlen = (lul_srclen * 1.01) + 12  // zlib needs a little extra room
lblob_dest = Blob(Space(lul_destlen))

// compress the blob
li_rc = compress2(lblob_dest, lul_destlen, lblob_src, lul_srclen, ai_level)
CHOOSE CASE li_rc
	CASE Z_OK
		ablob_dest = BlobMid(lblob_dest, 1, lul_destlen)
	CASE Z_MEM_ERROR
		SetNull(ablob_dest)
		MessageBox("Ziplib Error in of_Compress2", &
				"Not enough memory!", StopSign!)
		Return False
	CASE Z_BUF_ERROR
		SetNull(ablob_dest)
		MessageBox("Ziplib Error in of_Compress2", &
				"Not enough room in the output buffer!", StopSign!)
		Return False
	CASE ELSE
		SetNull(ablob_dest)
		MessageBox("Ziplib Error in of_Compress2", &
				"Undefined error: " + String(li_rc), StopSign!)
		Return False
END CHOOSE

Return True

end function

public function integer of_unzopencurrentfile2 (unsignedlong aul_unzfile, ref integer ai_method, ref integer ai_level);// -----------------------------------------------------------------------------
// SCRIPT:     n_ziplib.of_unzOpenCurrentFile2
//
// PURPOSE:    This function opens the current file in the zip archive file.
//					This form is used when the raw compressed data is being extracted.
//
// ARGUMENTS:  aul_unzfile		-	Handle to open zip archive file.
//					ai_method		-	Output compression method
//					ai_level			-	Output compression level
//
// RETURN:     Integer			-	0 = Success
//										-	See instance constants for error codes
//
// DATE        PROG/ID 		DESCRIPTION OF CHANGE / REASON
// ----------  --------		-------------------------------------------------------
// 12/06/2001	RSmith		Initial creation
// -----------------------------------------------------------------------------

Integer li_rc

li_rc = unzOpenCurrentFile2(aul_unzfile, ai_method, ai_level, 1)

Return li_rc

end function

public function integer of_zipclosefileinzipraw (unsignedlong aul_zipfile, unsignedlong aul_origsize, unsignedlong aul_crc);// -----------------------------------------------------------------------------
// SCRIPT:     n_ziplib.of_zipCloseFileInZipRaw
//
// PURPOSE:    This function will close a new file in the zip archive file.
//
// ARGUMENTS:  aul_zipfile		-	Handle to open zip archive file.
//					aul_origsize	-	Uncompressed size
//					aul_crc			-	CRC
//
// RETURN:     Integer			-	0 = Success
//										-	See instance constants for error codes
//
// DATE        PROG/ID 		DESCRIPTION OF CHANGE / REASON
// ----------  --------		-------------------------------------------------------
// 08/11/2003	RSmith		Initial creation
// -----------------------------------------------------------------------------

Integer li_rc

li_rc = zipCloseFileInZipRaw(aul_zipfile, aul_origsize, aul_crc)

Return li_rc

end function

public function integer of_zipopennewfileinzip2 (unsignedlong aul_zipfile, string as_filename, integer ai_method, integer ai_level);// -----------------------------------------------------------------------------
// SCRIPT:     n_ziplib.of_zipOpenNewFileInZip2
//
// PURPOSE:    This function will add a new file to the zip archive file.
//					The input blob is already compressed.
//
// ARGUMENTS:  aul_zipfile		-	Handle to open zip archive file.
//					as_filename		-	Name of file being added to archive.
//					ai_method		-	Compression method
//					ai_level			-	Compression level
//
// RETURN:     Integer			-	0 = Success
//										-	See instance constants for error codes
//
// DATE        PROG/ID 		DESCRIPTION OF CHANGE / REASON
// ----------  --------		-------------------------------------------------------
// 08/11/2003	RSmith		Initial creation
// -----------------------------------------------------------------------------

Integer li_rc
ULong lul_Handle, lul_null
zip_fileinfo lstr_fileinfo
WIN32_FIND_DATA lstr_FD
FILETIME lstr_LT
SYSTEMTIME lstr_ST
String ls_cmt, ls_filename

SetNull(ls_cmt)
SetNull(lul_null)

// Get the file information

lul_Handle = FindFirstFile(as_filename, lstr_FD)
If lul_Handle <= 0 Then Return -1
FindClose(lul_Handle)

// Value of 1 indicates text file, 0 indicates binary
lstr_fileinfo.internal_fa = this.of_Internal_FA(Right(as_filename, 3))

// get file attributes (Read-Only, Hidden, System, Archive)
lstr_fileinfo.external_fa = lstr_FD.dwfileattributes

// convert create datetime to usable format
FileTimeToLocalFileTime(lstr_FD.ftlastwritetime, lstr_LT)
FileTimeToSystemTime(lstr_LT, lstr_ST)

// remove drive letter
If Pos(as_filename, ":\") = 2 Then
	as_filename = Mid(as_filename, 4)
End If

// .zip standard is to use forward slash
ls_filename = this.of_replaceall(as_filename, "\", "/")

// copy datetime to structure
lstr_fileinfo.tmz_date.tm_mon = lstr_ST.wMonth - 1
lstr_fileinfo.tmz_date.tm_mday = lstr_ST.wDay
lstr_fileinfo.tmz_date.tm_year = lstr_ST.wYear
lstr_fileinfo.tmz_date.tm_hour = lstr_ST.wHour
lstr_fileinfo.tmz_date.tm_min = lstr_ST.wMinute
lstr_fileinfo.tmz_date.tm_sec = lstr_ST.wSecond

li_rc = zipOpenNewFileInZip2(aul_zipfile, ls_filename, lstr_fileinfo, &
					lul_null, 0, lul_null, 0, ls_cmt, ai_method, ai_level, 1)

Return li_rc

end function

public function boolean of_extractblob (unsignedlong aul_unzfile, ref string as_filename, ref blob ablob_data);// -----------------------------------------------------------------------------
// SCRIPT:     n_ziplib.of_ExtractBlob
//
// PURPOSE:    This function will extract a file from the zip archive and
//             save it in a blob.  The zip archive must have already been
//					opened using the using the of_unzOpen function.
//
// ARGUMENTS:  aul_unzfile	-	Handle of currently open zip archive
//					as_filename	-	Name of file within the archive
//					ablob_dest	-	Output blob containing the file
//
// RETURN:     Boolean		-	True = Success, False = Error
//
// DATE        PROG/ID 		DESCRIPTION OF CHANGE / REASON
// ----------  --------		-----------------------------------------------------
// 01/28/2002	RSmith		Initial creation
// 08/12/2003	RSmith		If as_filename is blank, extract current file.
// -----------------------------------------------------------------------------

Integer li_rc
String ls_extra, ls_cmts
ULong lul_fname, lul_extra, lul_cmts
unz_fileinfo lstr_info
ULong lul_datalen

// locate the file within the archive
If Len(as_filename) > 0 Then
	li_rc = unzLocateFile(aul_unzfile, as_filename, 2)
	If li_rc < 0 Then
		MessageBox("Ziplib Error in of_ExtractBlob", &
				"Unable to locate file '" + as_filename + "' within archive!", StopSign!)
		Return False
	End If
End If

// initialize buffers
lul_fname = 256
as_filename = Space(lul_fname)
lul_extra = 0
ls_extra  = Space(lul_extra)
lul_cmts  = 500
ls_cmts   = Space(lul_cmts)

// get file info
li_rc = unzGetCurrentFileInfo(aul_unzfile, lstr_info, as_filename, lul_fname, ls_extra, lul_extra, ls_cmts, lul_cmts)

// open the current file
li_rc = unzOpenCurrentFile(aul_unzfile)

// extract the current file
lul_datalen = lstr_info.uncompressed_size
li_rc = this.of_unzReadCurrentFile(aul_unzfile, ablob_data, lul_datalen)

// close the current file
li_rc = unzCloseCurrentFile(aul_unzfile)

Return True

end function

public function boolean of_extractblobraw (unsignedlong aul_unzfile, ref string as_filename, ref blob ablob_data, ref unsignedlong aul_origsize, ref unsignedlong aul_crc, ref integer ai_method, ref integer ai_level);// -----------------------------------------------------------------------------
// SCRIPT:     n_ziplib.of_ExtractBlobRaw
//
// PURPOSE:    This function will extract a file from the zip archive and
//             save it in a blob.  The zip archive must have already been
//					opened using the using the of_unzOpen function.  The output
//					blob contains the compressed version of the file.
//
// ARGUMENTS:  aul_unzfile		-	Handle of currently open zip archive
//					as_filename		-	Name of file within the archive
//					ablob_dest		-	Output blob containing the file
//					aul_origsize	-	Output original size of the file
//					aul_crc			-	Output CRC value of the file
//					ai_method		-	Output compression method
//					ai_level			-	Output compression level
//
// RETURN:     Boolean		-	True = Success, False = Error
//
// DATE        PROG/ID 		DESCRIPTION OF CHANGE / REASON
// ----------  --------		-------------------------------------------------------
// 08/12/2003	RSmith		Initial creation
// -----------------------------------------------------------------------------

Integer li_rc
String ls_extra, ls_cmts
ULong lul_fname, lul_extra, lul_cmts
unz_fileinfo lstr_info
ULong lul_datalen

// locate the file within the archive
li_rc = unzLocateFile(aul_unzfile, as_filename, 2)
If Len(as_filename) > 0 Then
	li_rc = unzLocateFile(aul_unzfile, as_filename, 2)
	If li_rc < 0 Then
		MessageBox("Ziplib Error in of_ExtractBlobRaw", &
				"Unable to locate file '" + as_filename + "' within archive!", StopSign!)
		Return False
	End If
End If

// initialize buffers
lul_fname = 256
as_filename = Space(lul_fname)
lul_extra = 0
ls_extra  = Space(lul_extra)
lul_cmts  = 500
ls_cmts   = Space(lul_cmts)

// get file info
li_rc = unzGetCurrentFileInfo(aul_unzfile, lstr_info, as_filename, lul_fname, ls_extra, lul_extra, ls_cmts, lul_cmts)

// update output arguments
aul_origsize = lstr_info.uncompressed_size
aul_crc = lstr_info.crc

// open the current file
li_rc = unzOpenCurrentFile2(aul_unzfile, ai_method, ai_level, 1)

// extract the current file
lul_datalen = lstr_info.uncompressed_size
li_rc = this.of_unzReadCurrentFile(aul_unzfile, ablob_data, lul_datalen)

// close the current file
li_rc = unzCloseCurrentFile(aul_unzfile)

Return True

end function

public function boolean f_put_filefromblob (string ls_file, blob lblob_content);LONG    	ll_iterations		// Total de lecturas
LONG    	ll_blobpos			// Posición de inicio de lectura del blob para escribir
LONG		ll_reps				// Número de lectura  actual
LONG    	ll_length			// Largo del texto a escribir
LONG    	ll_rem            // Resto para escribir (  < 32765 )
INTEGER	li_fhandle			// Puntero a archivo
BLOB    	lblob_writeblob   // El trozo de blob que se escribe

ll_length = LEN( lblob_content ) 

// Elimino el archivo temporal
FileDelete( ls_file )

li_fhandle = FileOpen(ls_file,streammode!,write!,Shared!,replace!)
If li_fhandle=-1 Then
	Messagebox("Error al escribir archivo", ls_file )
	Return FALSE
End If


//
// filewrite escribe 32765 bytes por vez
// Ver cuantas iteraciones necesito...
//

Setpointer(HourGlass!)
		
ll_iterations = ll_length/32765
ll_rem        = MOD(ll_length,32765)
ll_blobpos    = 1

If ll_iterations >= 1 Then
	DO
		ll_reps++
		lblob_writeblob = BLOBMID(lblob_content, ll_blobpos, 32765)
		FileWrite(li_fhandle,lblob_writeblob)
		ll_blobpos+=32765
	LOOP UNTIL ll_reps=ll_iterations 

	lblob_writeblob = BLOBMID( lblob_content, ll_blobpos, ll_rem )
	Filewrite(li_fhandle,lblob_writeblob)
Else 
	FileWrite(li_fhandle,lblob_content) 
End If

FileClose(li_fhandle)

Setpointer(Arrow!)

Return TRUE

end function

public function string f_getfilefrompath (string cpath);String cFile , cChar
Int    nLenPath, i

cFile = ""
nLenPath = LEN( cPath )

For i = nLenPath To 1 STEP -1
	cChar = MID( cPath , i , 1 )
	If cChar = ":" OR cChar = "\" Then
		EXIT
	Else
		cFile = cChar + cFile 
	End If
Next

Return ( cFile )



end function

public function string f_getextfromfile (string ls_file);
String ls_ext , ls_char
Int    li_lenFile, li_i

ls_ext = ""
li_lenFile = LEN( ls_file )

For li_i = li_lenfile To 1 STEP -1
	ls_char = MID( ls_file , li_i , 1 )
	If ls_char = "." Then
		EXIT
	Else
		ls_ext = ls_char + ls_ext
	End If
Next

Return ( ls_ext )

end function

public function string f_get_tmp_windows ();constant long lcl_maxpath = 260 
string ls_tempdir 
ulong lul_retval 
 
// Windows requiere rellenar esta cadena 
ls_tempdir = Fill('0', lcl_maxpath) 
 
// Llamada a la función local externa 
lul_retval = GetTempPatha(lcl_maxpath, ls_tempdir) 
 
// Windows devuelve la longitud de la cadena. Si es mayor que cero es que ha encontrado el directorio temporal de Windows. 
IF lul_retval > 0 THEN 
	RETURN ls_tempdir
END IF 
 
// Si no lo encuentra o falla la llamada a la función 
RETURN ""

end function

public function blob f_get_file2blob (string ls_archivo);Blob lblob_contenido
 
Long    llFileHnd, llBytesRead, llLargo
Long    llTotalBytes 
Blob    tlBuffer

SetPointer(HourGlass!)

// Lectura
llLargo     = FileLength(ls_archivo)
llFileHnd   = FileOpen( ls_archivo, StreamMode!, Read!, LockRead!)
llBytesRead = FileRead(llFileHnd, tlBuffer)
llTotalBytes = llBytesRead
Do While llBytesRead > 0
   lblob_contenido = lblob_contenido + tlBuffer
   llBytesRead = FileRead(llFileHnd, tlBuffer)
   llTotalBytes = llTotalBytes + llBytesRead
Loop
FileClose(llFileHnd)

SetPointer(Arrow!)

Return( lblob_contenido )
end function

public subroutine of_graba (string as_archivo);Integer	nPos, li_existe, li_ret, li_tipo, li_cliente, li_planta
Blob		lblob_file, lblob_zipped
Long    	ll_len_ori, ll_len_packed, ll_numero
Date		ldt_fecha

li_cliente		=	Integer(Mid(as_Archivo, 4, 3))
li_planta		=	Integer(Mid(as_Archivo, 7, 4))
li_tipo			=	Integer(Mid(as_Archivo, 11, 1))
ll_numero	=	Long(Mid(as_Archivo, 12, 8))
ldt_fecha		=	Date(Today())

If ll_numero = 0 Then
	Messagebox("Atención","Debe Ingresar el Número de Orden de Compra")
	return
End If

is_file_full = as_archivo
is_file = f_getfilefrompath( is_file_full )

// Se quitan comillas que pudieran venir 
is_file_full = f_global_replace( is_file_full, "'", "" )
is_file      = f_global_replace( is_file, "'", "" )

// Compruebo existencia de archivo
If FileExists( is_file_full ) = FALSE Then
	MessageBox( "Atención", "Archivo '" + is_file_full + "', no existe." )
	Return
End If	

// Leo archivo a un blob
lblob_file = f_get_file2blob( is_file_full ) 
ll_len_ori = LEN( lblob_file )

// Lo zipeo
//of_compress(lblob_file, lblob_zipped)
//lblob_zipped= lblob_file
ll_len_packed = LEN( lblob_zipped )

itr_trans.AutoCommit = True

/*
Obtener Fecha de Sistema
Colocar función que captura fecha de sistema
*/
SELECT IsNull(Max(orpr_secuen), 0) + 1 
INTO :li_existe
FROM dba.ordenprocesopdf
WHERE orpr_numero = :ll_numero
  AND	nm_file = :is_file
  AND orpr_tipord = :li_tipo
  And clie_codigo = :li_cliente
  And plde_codigo = :li_planta
USING itr_trans;

	// No existe una instrucción INSERTBLOB...inserto y luego actualizo.
	INSERT INTO dba.ordenprocesopdf( nm_file, len_original, len_packed , orpr_tipord, orpr_numero, orpr_secuen, orpr_fecpro, clie_codigo, plde_codigo)
		VALUES( :is_file, :ll_len_ori, :ll_len_packed , :li_tipo, :ll_numero, :li_existe, :ldt_Fecha, :li_cliente, :li_planta)
	USING itr_trans;
	/**/
	UPDATEBLOB dba.ordenprocesopdf
	SET content_file = :lblob_file//lblob_zipped
	WHERE orpr_numero = :ll_numero
	  AND	orpr_secuen = :li_existe
	  AND	nm_file = :is_file
	  AND orpr_tipord = :li_tipo
	  And clie_codigo = :li_cliente
	  And plde_codigo = :li_planta
	USING itr_trans;		
	/**/
	If itr_trans.SqlCode = -1 Then
		MessageBox( "Atención", "Ha ocurrido el error: ~n~r" + itr_trans.SqlErrText ) 
   End If

	// Esto, porque UPDATEBLOB admite una sola variable
	UPDATE dba.ordenprocesopdf
	SET len_original = :ll_len_ori,
	    len_packed   = :ll_len_packed,
  	 	 nm_file      = :is_file
  WHERE orpr_numero = :ll_numero
	  AND	orpr_secuen = :li_existe
	  AND	nm_file = :is_file
	  AND orpr_tipord = :li_tipo
	  And clie_codigo = :li_cliente
	  And plde_codigo = :li_planta
	USING itr_trans;			 
//
If itr_trans.SqlCode = -1 Then
	MessageBox( "Atención", "Ha ocurrido el error: ~n~r" + itr_trans.SqlErrText ) 
End If
end subroutine

public subroutine of_lectura (datawindow adw);String 	ls_tmp_windows, ls_ext_file, ls_dir_app, ls_file, ls_directory, ls_program, ls_file_tmp
Long   	ll_row, ll_len_original, ll_tipo, ll_numero, ll_resul
Blob   	lblob_file, lblob_file_unzip
Date		ldt_fecha
Integer	li_Secuencia, li_cliente, li_planta

//Obtiene el nombre del archivo, en este se selecicona desde la datawindows
ll_row = adw.GetRow()
If ll_row < 1 Then Return

//// Nombre de archivo
ls_file 		=	adw.Object.nm_file[ll_row]
ldt_fecha 	=	adw.Object.orpr_fecpro[ll_row]
li_Secuencia	=	adw.Object.orpr_secuen[ll_row]
ll_tipo			=	adw.Object.orpr_tipord[ll_row]
ll_numero	=	adw.Object.orpr_numero[ll_row]
li_cliente		=	adw.Object.clie_codigo[ll_row]
li_planta		=	adw.Object.plde_codigo[ll_row]

// Extensión del archivo
ls_ext_file = f_getextfromfile( ls_file )

// Archivo temporal para leer
If is_tmp_windows = "" Then
	ls_file_tmp  = "c:\xyz_." + ls_ext_file
Else
	ls_file_tmp = is_tmp_windows + "c:\xyz_." + + ls_ext_file
End If

SELECTBLOB  content_file
INTO :lblob_file
FROM dba.ordenprocesopdf
WHERE orpr_numero =:ll_numero
  and orpr_fecpro = :ldt_fecha
  and orpr_secuen = :li_secuencia
  and orpr_tipord = :ll_tipo
  and clie_codigo = :li_cliente
  and plde_codigo = :li_planta
USING itr_trans;

If itr_trans.Sqlcode = -1 OR itr_trans.Sqlcode = 100 Then
	MessageBox( "Atención", "Ha ocurrido el error: ~n~r" + itr_trans.SqlErrText ) 
	Return
End If

// Esto, porque SELECTBLOB no permite más de una variable
SELECT len_original
INTO :ll_len_original
FROM dba.ordenprocesopdf
WHERE orpr_numero =:ll_numero
  and orpr_fecpro = :ldt_fecha
  and orpr_secuen = :li_secuencia
  and orpr_tipord = :ll_tipo
  and clie_codigo = :li_cliente
  and plde_codigo = :li_planta
USING itr_trans;

If itr_trans.Sqlcode = -1 OR itr_trans.Sqlcode = 100 Then
	MessageBox( "Atención", "Ha ocurrido el error: ~n~r" + itr_trans.SqlErrText ) 
	Return
End If

IF IsNull(lblob_file) THEN
	MessageBox( "Atención", "Archivo que está leyendo viene nulo" ) 
	Return
END IF

//of_uncompress(lblob_file, ll_len_original, lblob_file_unzip)
lblob_file_unzip = lblob_file

If f_put_filefromblob( ls_file_tmp, lblob_file_unzip ) Then
	ls_Directory = ""
	ls_Program = SPACE(144)

	ll_resul = FindExecutable( ls_file_tmp,ls_directory,ls_Program )	
	Run ('"C:\Archivos de programa\Adobe\Acrobat 7.0\Reader\AcroRd32.exe" ' + ls_file_tmp)
	/*
	If ll_resul = 32 And TRIM(ls_program) <> "" Then
      RUN( ls_program + " ~"" + ls_file_tmp + "~"")
	Else
		MessageBox( "Atención", "No existe un programa asociado a '" + ls_file + "'." )
	End If
	*/
	ls_dir_app = 'C:\Directorio Actual o variable global de Aplicación'
	SetCurrentDirectoryA ( ls_dir_app )
End If
end subroutine

on n_zipapi.create
call super::create
TriggerEvent( this, "constructor" )
end on

on n_zipapi.destroy
TriggerEvent( this, "destructor" )
call super::destroy
end on

event constructor;itr_trans	= SQLCA
ib_Autocommit = itr_trans.AutoCommit
end event

event destructor;itr_trans.AutoCommit = ib_Autocommit
end event

