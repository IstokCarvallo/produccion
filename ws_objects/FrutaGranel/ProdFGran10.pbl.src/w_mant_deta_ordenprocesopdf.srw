$PBExportHeader$w_mant_deta_ordenprocesopdf.srw
forward
global type w_mant_deta_ordenprocesopdf from w_mant_detalle_csd
end type
end forward

global type w_mant_deta_ordenprocesopdf from w_mant_detalle_csd
integer width = 2350
integer height = 1328
end type
global w_mant_deta_ordenprocesopdf w_mant_deta_ordenprocesopdf

type variables
uo_productores				iuo_productor
uo_spro_ordenproceso	iuo_orden
uo_orderprocesopdf		iuo_Pdf
end variables

forward prototypes
public function boolean duplicado (string campo, integer tipo)
public function boolean wf_scanner ()
end prototypes

public function boolean duplicado (string campo, integer tipo);Long	ll_fila
String	ls_Tipo, ls_Orden, ls_Sec

ls_Tipo	= String(dw_1.Object.orpr_tipord[il_Fila])
ls_Orden	= String(dw_1.Object.orpr_numero[il_Fila])
ls_Sec	= String(dw_1.Object.orpr_secuen[il_Fila])

Choose Case Tipo
	CASE 1
		ls_Tipo	= campo
		
	Case 2
		ls_Orden	= campo
		
	Case 3
		ls_Sec	= campo
		
End Choose

ll_fila	= dw_1.Find('orpr_tipord = ' + ls_tipo + ' and orpr_numero = ' + ls_Orden + ' And orpr_secuen = ' + ls_Sec, 1, dw_1.RowCount())

If ll_fila > 0 and ll_fila <> il_fila Then
	MessageBox("Error","Registro ya fue ingresado anteriormente",Information!, Ok!)
	Return True
Else
	Return False
End If
end function

public function boolean wf_scanner ();Boolean	lb_Retorno = True
Long		ll_Imagen, ll_Resp
String		ls_Ruta, ls_File, ls_Directory

If TWAIN_SelectImageSource(0) = 0 Then
	MessageBox('','No hay dispositivos de captura seleccionados.')
	lb_Retorno = False
Else
	TWAIN_SetHideUI(1)
	ll_Resp	=	TWAIN_AcquireToFilename(ll_Imagen, ls_Ruta)
	TWAIN_FreeNative(ll_Imagen)
	
	ls_Directory = GetCurrentDirectory()

	GetFileSaveName("Seleccione Archivo", ls_Ruta, ls_File, "BMP", "BMP Files (*.bmp),*.bmp" , ls_Directory, 32770)

	ChangeDirectory(ls_Directory)
	
	If ll_Resp = 0 Then
		dw_1.Object.doct_rutas[il_Fila]		=	Mid(ls_Ruta, 1, LastPos(ls_Ruta, '\'))
		dw_1.Object.doct_archiv[il_Fila]	=	ls_File
		dw_1.Object.doct_estado[il_Fila]	=	2
		dw_1.Object.doct_fecdig[il_Fila]	=	Today()
		dw_1.Object.doct_hordig[il_Fila]	=	Now()
		dw_1.Object.doct_usuari[il_Fila]	=	gstr_us.Nombre
	Else
		 lb_Retorno = False
	End If
End If

Return lb_Retorno
end function

on w_mant_deta_ordenprocesopdf.create
call super::create
end on

on w_mant_deta_ordenprocesopdf.destroy
call super::destroy
end on

event ue_recuperadatos;call super::ue_recuperadatos;ias_campo[1] = String(dw_1.Object.clie_codigo[il_Fila])
ias_campo[2] = String(dw_1.Object.plde_codigo[il_Fila])
ias_campo[3] = String(dw_1.Object.orpr_tipord[il_Fila])
ias_campo[4] = String(dw_1.Object.orpr_numero[il_Fila])
ias_campo[5] = String(dw_1.Object.orpr_secuen[il_Fila])
ias_campo[6] = String(dw_1.Object.orpr_fecpro[il_Fila], 'dd/mm/yyyy')
ias_campo[7] = dw_1.Object.orpr_rutas[il_Fila]
ias_campo[8] = dw_1.Object.orpr_archiv[il_Fila]
ias_campo[9] = String(dw_1.Object.prod_codigo[il_Fila])
ias_campo[10] = dw_1.Object.prod_nombre[il_Fila]

If Not istr_mant.Agrega And Not istr_mant.Borra Then
	dw_1.Object.orpr_tipord.Protect = 1
	dw_1.Object.orpr_numero.Protect = 1
	dw_1.Object.orpr_secuen.Protect = 1

	dw_1.Object.orpr_tipord.BackGround.Color =  Rgb(192,192,192)
	dw_1.Object.orpr_numero.BackGround.Color =  Rgb(192,192,192)
	dw_1.Object.orpr_secuen.BackGround.Color =  Rgb(192,192,192)
End If

dw_1.Object.plde_codigo[il_Fila]	=	Long(istr_Mant.Argumento[1])
dw_1.Object.clie_codigo[il_Fila]	=	gi_CodExport
end event

event ue_deshace;call super::ue_deshace;dw_1.Object.clie_codigo[il_Fila]	=	Long(ias_campo[1])
dw_1.Object.plde_codigo[il_Fila]	=	Long(ias_campo[2])
dw_1.Object.orpr_tipord[il_Fila]		=	Long(ias_campo[3])
dw_1.Object.orpr_numero[il_Fila]	=	Long(ias_campo[4])
dw_1.Object.orpr_secuen[il_Fila]	=	Long(ias_campo[5])
dw_1.Object.orpr_fecpro[il_Fila]	=	Date(ias_campo[6])
dw_1.Object.orpr_rutas[il_Fila]		=	ias_campo[7]
dw_1.Object.orpr_archiv[il_Fila]	=	ias_campo[8]
dw_1.Object.prod_codigo[il_Fila]	=	Long(ias_campo[9])
dw_1.Object.prod_nombre[il_Fila]	=	ias_campo[10]
end event

event ue_antesguardar;Integer	li_cont
String	ls_mensaje, ls_colu[]

If IsNull(dw_1.Object.orpr_tipord[il_Fila]) Or dw_1.Object.orpr_tipord[il_Fila] = 0 Then
	li_cont ++
	ls_mensaje 			= ls_mensaje + "~nTipo de Orden"
	ls_colu[li_cont]	= "orpr_tipord"
End If

If IsNull(dw_1.Object.orpr_numero[il_Fila]) Or dw_1.Object.orpr_numero[il_Fila] = 0 Then
	li_cont ++
	ls_mensaje 			= ls_mensaje + "~nNumero de Orden"
	ls_colu[li_cont]	= "orpr_numero"
End If

If IsNull(dw_1.Object.prod_codigo[il_Fila]) Or dw_1.Object.prod_codigo[il_Fila] = 0 Then
	li_cont ++
	ls_mensaje 			= ls_mensaje + "~nProductor"
	ls_colu[li_cont]	= "prod_codigo"
End If

If li_cont > 0 Then
	MessageBox("Error de Consistencia", "Falta el ingreso de :" + ls_mensaje + ".", StopSign!, Ok!)
	dw_1.SetColumn(ls_colu[1])
	dw_1.SetFocus()
	Message.DoubleParm = -1
End If
end event

event ue_nuevo;call super::ue_nuevo;dw_1.Object.clie_codigo[il_Fila]	=	gi_CodExport
dw_1.Object.plde_codigo[il_Fila]	=	Long(istr_Mant.Argumento[1])
end event

event open;call super::open;iuo_productor	=	Create uo_productores
iuo_orden		=	Create uo_spro_ordenproceso
iuo_Pdf			=	Create uo_orderprocesopdf
end event

type pb_ultimo from w_mant_detalle_csd`pb_ultimo within w_mant_deta_ordenprocesopdf
end type

type pb_siguiente from w_mant_detalle_csd`pb_siguiente within w_mant_deta_ordenprocesopdf
end type

type pb_anterior from w_mant_detalle_csd`pb_anterior within w_mant_deta_ordenprocesopdf
end type

type pb_primero from w_mant_detalle_csd`pb_primero within w_mant_deta_ordenprocesopdf
end type

type pb_cancela from w_mant_detalle_csd`pb_cancela within w_mant_deta_ordenprocesopdf
integer x = 2034
integer y = 356
end type

type pb_acepta from w_mant_detalle_csd`pb_acepta within w_mant_deta_ordenprocesopdf
integer x = 2034
integer y = 176
end type

type pb_salir from w_mant_detalle_csd`pb_salir within w_mant_deta_ordenprocesopdf
integer x = 2034
integer y = 536
end type

type dw_1 from w_mant_detalle_csd`dw_1 within w_mant_deta_ordenprocesopdf
integer x = 101
integer y = 96
integer width = 1815
integer height = 1100
string dataobject = "dw_mant_ordenprocesopdf"
end type

event dw_1::itemchanged;call super::itemchanged;String		ls_Columna, ls_Null

ls_Columna = dwo.name
SetNull(ls_Null)

Choose Case ls_Columna
	Case	"orpr_tipord"
		If Duplicado(Data, 1) Then
			dw_1.SetItem(Row, ls_Columna, Long(ls_Null))
			Return 1
		End If
		
	Case	"orpr_numero"
		If Not iuo_Orden.Existe(This.Object.plde_codigo[row], This.Object.orpr_tipord[row], Long(Data), True, Sqlca, gi_CodExport) Or &
				Duplicado(Data, 2) Then
			dw_1.SetItem(Row, ls_Columna, Long(ls_Null))
			dw_1.SetItem(Row, 'orpr_fecpro', Date(ls_Null))
			dw_1.SetItem(Row, 'prod_codigo', Long(ls_Null))
			dw_1.SetItem(Row, 'prod_nombre', ls_Null)
			Return 1
		Else
			This.Object.orpr_fecpro[Row]	=	iuo_Orden.FechaOrden
			This.Object.prod_codigo[Row]	=	iuo_Orden.Productor
			If iuo_Productor.Existe(iuo_Orden.Productor, True, sqlca) Then 
				This.Object.prod_nombre[Row]	=	iuo_Productor.Nombre
			End If
		End If
		
	Case "orpr_secuen"
		If Duplicado(Data, 3) Then
			dw_1.SetItem(Row, ls_Columna, Long(ls_Null))
			Return 1
		End If
		
	Case 'prod_codigo'
		If Not iuo_Productor.Existe(Long(Data), True, Sqlca) Then
			dw_1.SetItem(Row, ls_Columna, Long(ls_Null))
			dw_1.SetItem(Row, 'prod_nombre', ls_Null)
			Return 1
		Else
			This.Object.prod_nombre[Row]	=	iuo_Productor.Nombre
		End If
End Choose 
end event

event dw_1::buttonclicked;call super::buttonclicked;String   			ls_Boton, ls_Ruta, ls_Archivo, ls_Filtro, ls_Null
str_Busqueda	lstr_Busq

SetNull(ls_Null)
ls_Boton = dwo.Name

Choose Case ls_Boton
	Case 'b_orden'
		If IsNull(This.Object.orpr_tipord[Row]) Then Return
		
		lstr_busq.argum[1] = String(This.Object.plde_codigo[Row])
		lstr_busq.argum[2] = "1"
		lstr_busq.argum[3] = String(This.Object.orpr_tipord[Row])
		lstr_busq.argum[4] = String(This.Object.clie_codigo[1])
		lstr_busq.argum[6] = ""
		
		OpenWithParm(w_busc_spro_ordenproceso, lstr_busq)
		
		lstr_busq	= Message.PowerObjectParm
		
		If lstr_busq.argum[6] <> "" Then
			This.Object.orpr_numero[Row]	= Long(lstr_Busq.Argum[6])
			If Duplicado('orpr_numero', 2) Then 
				This.SetItem(Row, 'orpr_numero', Long(ls_Null))
				This.SetItem(Row, 'orpr_fecpro', Date(ls_Null))
				This.SetItem(Row, 'prod_codigo', Long(ls_Null))
				This.SetItem(Row, 'prod_nombre', ls_Null)
			Else
				This.Object.orpr_fecpro[Row]	= Date(lstr_Busq.Argum[5])
				This.Object.prod_codigo[Row]	= Long(lstr_Busq.Argum[7])
				iuo_Productor.Existe(Long(lstr_Busq.Argum[7]), True, Sqlca)
				This.Object.prod_nombre[Row]	=  iuo_Productor.Nombre
			End If
		End If
		
	Case 'b_productor'
		OpenWithParm(w_busc_productores, lstr_busq)
		
		lstr_busq	= Message.PowerObjectParm
		
		If lstr_busq.argum[1] <> "" Then
			This.Object.prod_codigo[Row]	=	Long(lstr_busq.argum[1])
			This.Object.prod_nombre[Row]	=	lstr_busq.argum[2]
		End If

	Case 'b_scanner'
		wf_scanner()
		
	Case "b_carga"
		ls_Filtro	= 'Acrobat Files (*.pdf),*.pdf,JPGE Files (*.jpg),*.jpg,Map Bits Files (*.bmp),*.bmp'
		If iuo_Pdf.ObtieneArchivo(ls_Ruta, ls_Archivo, ls_Filtro) Then
			This.Object.orpr_rutas[Row]	=	Mid(ls_Ruta, 1, LastPos(ls_Ruta, '\'))
			This.Object.orpr_archiv[Row]	=	ls_Archivo
		Else
			MessageBox('Atencion', 'No se pudo recuperar archivo.')
			Return 1
		End If
		
	Case "b_visualiza"
		ls_Archivo = This.Object.orpr_rutas[Row] + This.Object.orpr_archiv[Row]
		
		If FileExists(ls_Archivo) Then
			iuo_Pdf.AbrirDocumento(ls_Archivo)
		Else
			iuo_Pdf.RecuperaImagen(dw_1, Row, sqlca)
		End If
End Choose 
end event

