$PBExportHeader$uo_reclamos.sru
$PBExportComments$Objecto Usuario que valida los reclamos
forward
global type uo_reclamos from nonvisualobject
end type
end forward

global type uo_reclamos from nonvisualobject
end type
global uo_reclamos uo_reclamos

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
public function boolean existe (long al_numero, integer ai_especie, boolean ab_mensaje, transaction at_transaccion)
public function boolean grabaimagen (string as_archivo, long al_numero, integer ai_especie, long al_cliente, integer ai_codigo, transaction at_transaccion)
public function boolean obtienearchivo (ref string as_ruta, ref string as_fichero, string as_filtro)
public function integer abrirdocumento (string as_fichero)
public function long maximo (long al_numero, integer ai_especie, long al_cliente, transaction at_transaccion)
public function long maximo (integer ai_especie, long al_cliente, transaction at_transaccion)
public function string temporalwindow ()
public function boolean archivoblob (string as_file, blob ablob_content)
public function boolean recuperaimagen (datawindow adw, transaction at_transaccion)
public function string generadocumento (datawindow adw, long fila, transaction at_transaccion)
public function boolean existe (integer ai_cliente, integer ai_especie, string as_pallet, long al_productor, boolean ab_mensaje, transaction at_transaccion)
public function boolean existe (integer ai_cliente, integer ai_especie, integer ai_variedad, string as_pallet, integer ai_zona, long al_productor, string as_calibre, string as_embalaje, date ad_fecha, boolean ab_mensaje, transaction at_transaccion)
public function boolean grabapdf (string as_archivo, long al_numero, integer ai_especie, long al_cliente, integer ai_codigo, transaction at_transaccion)
public function boolean recuperapdf (datawindow adw, transaction at_transaccion)
public function string generapdf (datawindow adw, long fila, transaction at_transaccion)
public function long maxpdf (long al_numero, integer ai_especie, long al_cliente, transaction at_transaccion)
end prototypes

public function boolean existe (long al_numero, integer ai_especie, boolean ab_mensaje, transaction at_transaccion);SELECT reen_numero, clie_codigo, espe_codigo, reen_fecrec, reci_codigo, nave_tipotr,
		   nave_codigo, puer_codigo, reen_feczar, reen_semliq, reen_refere, reen_mailre,
		   reen_feczon, reen_observ, reen_impres, reen_motivo
	INTO	:Numero, :Cliente, :Especie, :Fecha_Reclamo, :Recibidor, :Tipo_Transporte, 
			:Nave, :Puerto, :Fecha_Zarpe, :Semana, :Referencia, :Fecha_Referencia,
			:Fecha_Zonal, :Observacion, :Impresion, :Motivo
	FROM	dbo.ctlcalreclamosenca
	WHERE	reen_numero	=	:al_Numero
		AND	espe_codigo		=	:ai_especie
	Using	at_transaccion;

IF At_Transaccion.SqlCode = -1 THEN
	F_ErrorBaseDatos(At_Transaccion,"Lectura de Tabla de Reclamos")
	
	RETURN False
ELSEIF sqlca.SQLCode = 100 THEN
	IF ab_Mensaje THEN
		MessageBox("Atención", "Código de Reclamo " + String(al_numero) + &
						", no ha sido~ringresado en tabla respectiva." + &
						"~r~rIngrese o seleccione otro Código.")
	END IF
	
	RETURN False
END IF

RETURN True
end function

public function boolean grabaimagen (string as_archivo, long al_numero, integer ai_especie, long al_cliente, integer ai_codigo, transaction at_transaccion);Boolean	lb_Retorno = True
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
	
	UpDateBlob dbo.ctlcalreclamosimagenesdeta 
			Set reid_imagen  = :lbl_data 
			Where reen_numero = :al_Numero
		    		And espe_codigo = :ai_especie
    				And clie_codigo = :al_Cliente
    				And reid_codigo = :ai_Codigo
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

public function boolean obtienearchivo (ref string as_ruta, ref string as_fichero, string as_filtro);Boolean	lb_Retorno = True
String		ls_pathname, ls_filename, ls_filtro

If IsNull(as_Filtro) Then
	ls_filtro = "JPEG Files (*.jpg),*.jpg," + "GIFF Files (*.gif),*.gif," + "BMP Files (*.bmp),*.bmp,"
Else
	ls_Filtro = as_Filtro
End If

IF GetFileOpenName ( "Archivo de Imagen: ", as_Ruta, as_Fichero , "jpg", ls_filtro) = 0 THEN  lb_Retorno = False

Return lb_Retorno
end function

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

public function long maximo (long al_numero, integer ai_especie, long al_cliente, transaction at_transaccion);Long	ll_Retorno

Select IsNull(Max(reid_codigo), 0)
	Into :ll_Retorno
    From dbo.ctlcalreclamosimagenesdeta
    Where reen_numero = :al_Numero
    And espe_codigo = :ai_especie
    And clie_codigo = :al_Cliente
Using	at_transaccion;

IF At_Transaccion.SqlCode = -1 THEN
	F_ErrorBaseDatos(At_Transaccion,"Lectura de Tabla de Imagenes de Reclamos")
	ll_Retorno = -1
ELSEIF sqlca.SQLCode = 100 THEN	
	ll_Retorno = -1
END IF

Return ll_Retorno
end function

public function long maximo (integer ai_especie, long al_cliente, transaction at_transaccion);Long	ll_Retorno

Select IsNull(Max(reen_numero), 0)
	Into :ll_Retorno
    From dbo.ctlcalreclamosenca
    Where espe_codigo = :ai_especie
    And clie_codigo = :al_Cliente
Using	at_transaccion;

IF At_Transaccion.SqlCode = -1 THEN
	F_ErrorBaseDatos(At_Transaccion,"Lectura de Tabla de Reclamos Encabezado")
	ll_Retorno = -1
ELSEIF sqlca.SQLCode = 100 THEN	
	ll_Retorno = -1
END IF

Return ll_Retorno
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
Long   	ll_row, ll_numero, ll_resul
Blob   	lblob_file
Integer	li_cliente, li_especie, li_Codigo

//Obtiene el nombre del archivo, en este se selecicona desde la datawindows
ll_row = adw.GetRow()
If ll_row < 1 Then Return False

//// Nombre de archivo
ls_file 		=	adw.Object.reid_archiv[ll_row]
ll_numero	=	adw.Object.reen_numero[ll_row]
li_cliente		=	adw.Object.clie_codigo[ll_row]
li_especie	=	adw.Object.espe_codigo[ll_row]
li_Codigo		=	adw.Object.reid_codigo[ll_row]

ls_tmp_windows	= TemporalWindow()

// Archivo temporal para leer
If ls_tmp_windows = "" Then
	ls_file_tmp  = "c:\" + ls_file
Else
	ls_file_tmp = ls_tmp_windows + ls_File
End If

SELECTBLOB  reid_imagen
INTO :lblob_file
FROM dbo.ctlcalreclamosimagenesdeta
WHERE reen_numero  =:ll_numero
  and espe_codigo  = :li_especie
  and clie_codigo 	= :li_cliente
  and reid_codigo  = :li_codigo
USING at_transaccion;

If at_transaccion.Sqlcode = -1 OR at_transaccion.Sqlcode = 100 Then
	MessageBox( "Atención", "Ha ocurrido el error: ~n~r" + at_transaccion.SqlErrText ) 
	Return False
End If

If IsNull(lblob_file) Then
	MessageBox( "Atención", "Archivo que está leyendo no fue cargado." ) 
	Return False
End If

If ArchivoBlob(ls_file_tmp, lblob_file) Then
	AbrirDocumento(ls_file_tmp)
End If
end function

public function string generadocumento (datawindow adw, long fila, transaction at_transaccion);String 	ls_tmp_windows, ls_file, ls_file_tmp
Long   	ll_row, ll_numero, ll_resul
Blob   	lblob_file
Integer	li_cliente, li_especie, li_Codigo

//Obtiene el nombre del archivo, en este se selecicona desde la datawindows
ll_row = adw.GetRow()
If ll_row < 1 Then Return ''

//// Nombre de archivo
ls_file 		=	adw.Object.reid_archiv[Fila]
ll_numero	=	adw.Object.reen_numero[Fila]
li_cliente		=	adw.Object.clie_codigo[Fila]
li_especie	=	adw.Object.espe_codigo[Fila]
li_Codigo		=	adw.Object.reid_codigo[Fila]

ls_tmp_windows	= TemporalWindow()

// Archivo temporal para leer
//If ls_tmp_windows = "" Then
//	ls_file_tmp  = "c:\" + ls_file
//Else
//	ls_file_tmp = ls_tmp_windows + ls_File
//	ls_file_tmp = "c:\" + ls_File
//End If

ls_file_tmp  = "c:\" + ls_file

SELECTBLOB  reid_imagen
INTO :lblob_file
FROM dbo.ctlcalreclamosimagenesdeta
WHERE reen_numero  =:ll_numero
  and espe_codigo  = :li_especie
  and clie_codigo 	= :li_cliente
  and reid_codigo  = :li_codigo
USING at_transaccion;

If at_transaccion.Sqlcode = -1 OR at_transaccion.Sqlcode = 100 Then
	MessageBox( "Atención", "Ha ocurrido el error: ~n~r" + at_transaccion.SqlErrText ) 
	Return ''
End If

If IsNull(lblob_file) Then
	MessageBox( "Atención", "Archivo que está leyendo no fue cargado." ) 
	Return ''
End If

If ArchivoBlob(ls_file_tmp, lblob_file) Then
	Return ls_file_tmp
End If
end function

public function boolean existe (integer ai_cliente, integer ai_especie, string as_pallet, long al_productor, boolean ab_mensaje, transaction at_transaccion);SELECT	reen_numero, espe_codigo, clie_codigo, paen_numero, vari_codigo,
			zona_codigo, prod_codigo, pafr_calibr, emba_codigo, emba_fecemb,
			reld_ccajas
	INTO	:Numero, :Especie, :Cliente, :Pallet, :Variedad,
			:Zona, :Productor, :Calibre, :Embalaje, :Fecha_Embalaje,
			:Cajas
	FROM	dbo.ctlcalreclamoslotedeta
	WHERE	clie_codigo		=	:ai_Cliente
		AND	espe_codigo		=	:ai_especie
		AND	paen_numero	=	:as_pallet
		AND  prod_codigo     =   :al_Productor
	Using	at_transaccion;

IF At_Transaccion.SqlCode = -1 THEN
	F_ErrorBaseDatos(At_Transaccion,"Lectura de Tabla de Reclamos")
	
	RETURN False
ELSEIF sqlca.SQLCode = 100 THEN
	IF ab_Mensaje THEN
		MessageBox("Atención", "Código de Reclamo para Pallet " + as_pallet + &
						", no ha sido~ringresado en tabla respectiva." + &
						"~r~rIngrese o seleccione otro Código.")
	END IF
	
	RETURN False
END IF

RETURN True
end function

public function boolean existe (integer ai_cliente, integer ai_especie, integer ai_variedad, string as_pallet, integer ai_zona, long al_productor, string as_calibre, string as_embalaje, date ad_fecha, boolean ab_mensaje, transaction at_transaccion);SELECT	reen_numero, espe_codigo, clie_codigo, paen_numero, vari_codigo,
			zona_codigo, prod_codigo, pafr_calibr, emba_codigo, emba_fecemb,
			reld_ccajas
	INTO	:Numero, :Especie, :Cliente, :Pallet, :Variedad,
			:Zona, :Productor, :Calibre, :Embalaje, :Fecha_Embalaje,
			:Cajas
	FROM	dbo.ctlcalreclamoslotedeta
	WHERE	clie_codigo		=	:ai_Cliente
		AND	espe_codigo		=	:ai_especie
		AND	paen_numero	=	:as_pallet
		AND  prod_codigo     =   :al_Productor
		And	vari_codigo		=	:ai_Variedad
		And	zona_codigo		=	:ai_zona
		And	emba_codigo	=	:as_embalaje
		And 	pafr_calibr		=	:as_calibre
		And	emba_fecemb	=	:ad_fecha
	Using	at_transaccion;

IF At_Transaccion.SqlCode = -1 THEN
	F_ErrorBaseDatos(At_Transaccion,"Lectura de Tabla de Reclamos")
	
	RETURN False
ELSEIF sqlca.SQLCode = 100 THEN
	IF ab_Mensaje THEN
		MessageBox("Atención", "Código de Reclamo para Pallet " + as_pallet + &
						", no ha sido~ringresado en tabla respectiva." + &
						"~r~rIngrese o seleccione otro Código.")
	END IF
	
	RETURN False
END IF

RETURN True
end function

public function boolean grabapdf (string as_archivo, long al_numero, integer ai_especie, long al_cliente, integer ai_codigo, transaction at_transaccion);Boolean	lb_Retorno = True
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
	
	UpDateBlob dbo.ctlcalreclamospdf
			Set repd_data  = :lbl_data 
			Where reen_numero = :al_Numero
		    		And espe_codigo = :ai_especie
    				And clie_codigo = :al_Cliente
    				And repd_codigo = :ai_Codigo
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

public function boolean recuperapdf (datawindow adw, transaction at_transaccion);String 	ls_tmp_windows, ls_file, ls_file_tmp
Long   	ll_row, ll_numero, ll_resul
Blob   	lblob_file
Integer	li_cliente, li_especie, li_Codigo

//Obtiene el nombre del archivo, en este se selecicona desde la datawindows
ll_row = adw.GetRow()
If ll_row < 1 Then Return False

//// Nombre de archivo
ls_file 		=	adw.Object.repd_archiv[ll_row]
ll_numero	=	adw.Object.reen_numero[ll_row]
li_cliente		=	adw.Object.clie_codigo[ll_row]
li_especie	=	adw.Object.espe_codigo[ll_row]
li_Codigo		=	adw.Object.repd_codigo[ll_row]

ls_tmp_windows	= TemporalWindow()

// Archivo temporal para leer
If ls_tmp_windows = "" Then
	ls_file_tmp  = "c:\" + ls_file
Else
	ls_file_tmp = ls_tmp_windows + ls_File
End If

SELECTBLOB  repd_data
INTO :lblob_file
FROM dbo.ctlcalreclamospdf
WHERE reen_numero  =:ll_numero
  and espe_codigo  = :li_especie
  and clie_codigo 	= :li_cliente
  and repd_codigo  = :li_codigo
USING at_transaccion;

If at_transaccion.Sqlcode = -1 OR at_transaccion.Sqlcode = 100 Then
	MessageBox( "Atención", "Ha ocurrido el error: ~n~r" + at_transaccion.SqlErrText ) 
	Return False
End If

If IsNull(lblob_file) Then
	MessageBox( "Atención", "Archivo que está leyendo no fue cargado." ) 
	Return False
End If

If ArchivoBlob(ls_file_tmp, lblob_file) Then
	AbrirDocumento(ls_file_tmp)
End If
end function

public function string generapdf (datawindow adw, long fila, transaction at_transaccion);String 	ls_tmp_windows, ls_file, ls_file_tmp
Long   	ll_row, ll_numero, ll_resul
Blob   	lblob_file
Integer	li_cliente, li_especie, li_Codigo

//Obtiene el nombre del archivo, en este se selecicona desde la datawindows
ll_row = adw.GetRow()
If ll_row < 1 Then Return ''

//// Nombre de archivo
ls_file 		=	adw.Object.repd_archiv[Fila]
ll_numero	=	adw.Object.reen_numero[Fila]
li_cliente		=	adw.Object.clie_codigo[Fila]
li_especie	=	adw.Object.espe_codigo[Fila]
li_Codigo		=	adw.Object.repd_codigo[Fila]

ls_tmp_windows	= TemporalWindow()

// Archivo temporal para leer
//If ls_tmp_windows = "" Then
//	ls_file_tmp  = "c:\" + ls_file
//Else
//	ls_file_tmp = ls_tmp_windows + ls_File
//	ls_file_tmp = "c:\" + ls_File
//End If

ls_file_tmp  = "c:\" + ls_file

SELECTBLOB  repd_data
INTO :lblob_file
FROM dbo.ctlcalreclamospdf
WHERE reen_numero  =:ll_numero
  and espe_codigo  = :li_especie
  and clie_codigo 	= :li_cliente
  and repd_codigo  = :li_codigo
USING at_transaccion;

If at_transaccion.Sqlcode = -1 OR at_transaccion.Sqlcode = 100 Then
	MessageBox( "Atención", "Ha ocurrido el error: ~n~r" + at_transaccion.SqlErrText ) 
	Return ''
End If

If IsNull(lblob_file) Then
	MessageBox( "Atención", "Archivo que está leyendo no fue cargado." ) 
	Return ''
End If

If ArchivoBlob(ls_file_tmp, lblob_file) Then
	Return ls_file_tmp
End If
end function

public function long maxpdf (long al_numero, integer ai_especie, long al_cliente, transaction at_transaccion);Long	ll_Retorno

Select IsNull(Max(repd_codigo), 0)
	Into :ll_Retorno
    From dbo.ctlcalreclamospdf
    Where reen_numero = :al_Numero
    And espe_codigo = :ai_especie
    And clie_codigo = :al_Cliente
Using	at_transaccion;

IF At_Transaccion.SqlCode = -1 THEN
	F_ErrorBaseDatos(At_Transaccion,"Lectura de Tabla de Imagenes de Reclamos")
	ll_Retorno = -1
ELSEIF sqlca.SQLCode = 100 THEN	
	ll_Retorno = -1
END IF

Return ll_Retorno
end function

on uo_reclamos.create
call super::create
TriggerEvent( this, "constructor" )
end on

on uo_reclamos.destroy
TriggerEvent( this, "destructor" )
call super::destroy
end on

