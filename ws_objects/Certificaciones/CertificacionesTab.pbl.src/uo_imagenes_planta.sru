$PBExportHeader$uo_imagenes_planta.sru
$PBExportComments$Objecto Usuario que valida los reclamos
forward
global type uo_imagenes_planta from nonvisualobject
end type
end forward

global type uo_imagenes_planta from nonvisualobject
end type
global uo_imagenes_planta uo_imagenes_planta

type prototypes
Function ulong GetTempPath (long nBufferLength, ref string lpBuffer ) LIBRARY "KERNEL32.DLL" Alias For "GetTempPathA;Ansi"
end prototypes

type variables
CONSTANT Integer LARGO_ARCHIVO = 32765

Long			Numero, Recibidor, Nave, Puerto, Productor, Cajas
Integer		Cliente, Especie, Semana, Impresion, Variedad, Zona
Date			Fecha_Reclamo, Fecha_Zarpe, Fecha_Zonal
DateTime	Fecha_Referencia, Fecha_Embalaje
String			Tipo_Transporte, Referencia, Observacion, Motivo, Pallet, Calibre, Embalaje
end variables

forward prototypes
public function integer abrirdocumento (string as_fichero)
public function string temporalwindow ()
public function boolean archivoblob (string as_file, blob ablob_content)
public function boolean recuperaimagen (datawindow adw, transaction at_transaccion)
public function boolean obtienearchivo (ref string as_ruta, ref string as_fichero, string as_filtro, string as_extension)
public function boolean grabaimagen (string as_archivo, long al_planta, integer ai_especie, transaction at_transaccion)
public function string generadocumento (long al_planta, integer ai_especie, string as_archivo, transaction at_transaccion)
end prototypes

public function integer abrirdocumento (string as_fichero);String			ls_fichero
Integer		li_ret
OleObject	loo_Shell

ls_fichero = as_fichero

If Pos(ls_fichero,'"') = 0 Then ls_fichero = '"' + ls_fichero + '"'

If Not FileExists(as_fichero) Then Return -1

loo_Shell = Create OleObject

li_Ret = loo_Shell.ConnectToNewObject('WScript.Shell')

If li_ret < 0 Then
   Destroy loo_Shell
   Return -1
End if

loo_Shell.Run(ls_fichero, 1, False)

loo_Shell.DisconnectObject()
Destroy loo_Shell

Return 1
end function

public function string temporalwindow ();Constant long lcl_maxpath = 260 

String ls_tempdir, ls_Retorno
Ulong lul_retval
 
ls_tempdir = Fill('0', lcl_maxpath) 
 
lul_retval = GetTempPath(lcl_maxpath, ls_tempdir) 
 
IF lul_retval > 0 THEN 
	ls_Retorno = ls_tempdir
Else
	ls_Retorno = ''
END IF 
 
RETURN ls_Retorno

end function

public function boolean archivoblob (string as_file, blob ablob_content);/*
ll_iterations		:	Total de lecturas
ll_blobpos		:	Posición de inicio de lectura del blob para escribir
ll_reps			:	Número de lectura  actual
ll_length			:	Largo del texto a escribir
ll_rem         	:	Resto para escribir (  < 32765 )
li_fhandle		:	Puntero a archivo
lblob_writeblob	:	El trozo de blob que se escribe
*/

Long		ll_iterations, ll_blobpos, ll_reps, ll_length, ll_rem
Integer	li_fhandle
Blob		lblob_writeblob

ll_length = Len(ablob_content) 

// Elimino el archivo temporal
FileDelete(as_file)

li_fhandle = FileOpen(as_file,streammode!,write!,Shared!,replace!)
If li_fhandle=-1 Then
	Messagebox("Error al escribir archivo", as_file)
	Return FALSE
End If

Setpointer(HourGlass!)
		
ll_iterations = ll_length / LARGO_ARCHIVO
ll_rem        = MOD(ll_length, LARGO_ARCHIVO)
ll_blobpos    = 1

If ll_iterations >= 1 Then
	DO
		ll_reps++
		lblob_writeblob = BLOBMID(ablob_content, ll_blobpos, LARGO_ARCHIVO)
		FileWrite(li_fhandle, lblob_writeblob)
		ll_blobpos+= LARGO_ARCHIVO
	LOOP UNTIL ll_reps = ll_iterations 

	lblob_writeblob = BLOBMID(ablob_content, ll_blobpos, ll_rem)
	Filewrite(li_fhandle, lblob_writeblob)
Else 
	FileWrite(li_fhandle, ablob_content) 
End If

FileClose(li_fhandle)

Setpointer(Arrow!)

Return True
end function

public function boolean recuperaimagen (datawindow adw, transaction at_transaccion);String 	ls_tmp_windows, ls_file, ls_file_tmp
Long   	ll_row, ll_planta, ll_resul
Blob   	lblob_file
Integer	li_especie

//Obtiene el nombre del archivo, en este se selecicona desde la datawindows
ll_row = adw.GetRow()
If ll_row < 1 Then Return False

//// Nombre de archivo
ls_file 		=	adw.Object.cece_archiv[ll_row]
ll_planta		=	adw.Object.plde_codigo[ll_row]
li_especie	=	adw.Object.espe_codigo[ll_row]

ls_tmp_windows	= TemporalWindow()

// Archivo temporal para leer
If ls_tmp_windows = "" Then
	ls_file_tmp  = "c:\" + ls_file
Else
	ls_file_tmp = ls_tmp_windows + ls_File
End If

SELECTBLOB  cece_doctos
INTO :lblob_file
FROM dbo.cert_certificacion_plde
WHERE espe_codigo  = :li_especie
  and plde_codigo 	= :ll_planta
USING at_transaccion;

If at_transaccion.Sqlcode = -1 OR at_transaccion.Sqlcode = 100 Then
	MessageBox( "Atención", "Ha ocurrido el error: ~n~r" + at_transaccion.SqlErrText ) 
	Return False
End If

If IsNull(lblob_file) Then
	MessageBox( "Atención", "Archivo que está leyendo viene nulo" ) 
	Return False
End If

If ArchivoBlob(ls_file_tmp, lblob_file) Then
	AbrirDocumento(ls_file_tmp)
End If
end function

public function boolean obtienearchivo (ref string as_ruta, ref string as_fichero, string as_filtro, string as_extension);Boolean	lb_Retorno = True
String		ls_pathname, ls_filename, ls_filtro

If IsNull(as_Filtro) Then
	ls_filtro = "JPEG Files (*.jpg),*.jpg," + "GIFF Files (*.gif),*.gif," + "BMP Files (*.bmp),*.bmp,"
Else
	ls_Filtro = as_Filtro
End If

IF GetFileOpenName ( "Archivo de Imagen: ", as_Ruta, as_Fichero , as_extension, ls_filtro) = 0 THEN  lb_Retorno = False

Return lb_Retorno
end function

public function boolean grabaimagen (string as_archivo, long al_planta, integer ai_especie, transaction at_transaccion);Boolean	lb_Retorno = True
Long		ll_File
Blob 		lbl_data, lbl_temp

ll_File = FileOpen(as_archivo, StreamMode!)

Do While FileRead(ll_file, lbl_temp) > 0
	lbl_data += lbl_temp
Loop

FileClose(ll_file)

If ll_File = 1 Then
	FileRead(ll_file, lbl_data)
	FileClose(ll_file)
	at_Transaccion.AutoCommit = True
		
	UpDateBlob dbo.cert_certificacion_plde
			Set cece_doctos  = :lbl_data 
			Where plde_codigo = :al_planta
    				And espe_codigo = :ai_especie
			Using at_Transaccion;
			
	at_Transaccion.AutoCommit = False
ELSE
	lb_Retorno = False
END IF

IF at_Transaccion.SQLNRows > 0 THEN
	Commit;
ELSE
	lb_Retorno = False
END IF

FileClose(ll_file)

Return lb_Retorno = False
end function

public function string generadocumento (long al_planta, integer ai_especie, string as_archivo, transaction at_transaccion);String 	ls_tmp_windows, ls_file, ls_file_tmp
Blob   	lblob_file

// Nombre de archivo
ls_file 				=	as_archivo
ls_tmp_windows	= TemporalWindow()

// Archivo temporal para leer
If ls_tmp_windows = "" Then
	ls_file_tmp  = "c:\" + ls_file
Else
	ls_file_tmp = ls_tmp_windows + ls_File
End If

SELECTBLOB  dode_imagen 
INTO :lblob_file
FROM dbo.cert_certificacion_plde
WHERE prod_codigo =:al_planta
  and espe_codigo	= :ai_especie
USING at_transaccion;

If at_transaccion.Sqlcode = -1 Or at_transaccion.Sqlcode = 100 Then
	MessageBox( "Atención", "Ha ocurrido el error: ~n~r" + at_transaccion.SqlErrText ) 
	Return ''
End If

If IsNull(lblob_file) Then
	MessageBox( "Atención", "Archivo que está leyendo viene nulo" ) 
	Return ''
End If

If ArchivoBlob(ls_file_tmp, lblob_file) Then
	Return ls_file_tmp
End If
end function

on uo_imagenes_planta.create
call super::create
TriggerEvent( this, "constructor" )
end on

on uo_imagenes_planta.destroy
TriggerEvent( this, "destructor" )
call super::destroy
end on

