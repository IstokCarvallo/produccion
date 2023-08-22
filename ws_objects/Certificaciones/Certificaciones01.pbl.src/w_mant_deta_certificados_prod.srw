$PBExportHeader$w_mant_deta_certificados_prod.srw
forward
global type w_mant_deta_certificados_prod from w_mant_detalle_csd
end type
type plb_1 from picturelistbox within w_mant_deta_certificados_prod
end type
type plb_documentos from picturelistbox within w_mant_deta_certificados_prod
end type
type st_1 from statictext within w_mant_deta_certificados_prod
end type
type pb_folder from picturebutton within w_mant_deta_certificados_prod
end type
type ddlb_filtro from dropdownlistbox within w_mant_deta_certificados_prod
end type
end forward

global type w_mant_deta_certificados_prod from w_mant_detalle_csd
integer width = 3835
integer height = 2080
string title = "Sucursal"
boolean controlmenu = true
plb_1 plb_1
plb_documentos plb_documentos
st_1 st_1
pb_folder pb_folder
ddlb_filtro ddlb_filtro
end type
global w_mant_deta_certificados_prod w_mant_deta_certificados_prod

type variables
DatawindowChild	idwc_Predio, idwc_Productor
String	is_Filtro, is_Ruta

uo_imagenes		iuo_Imagenes
uo_plantadesp		iuo_planta
end variables

forward prototypes
public function integer wf_inicializa ()
public subroutine wf_carga_imagenes ()
public function boolean wf_duplicado (string as_ruta, string as_archivo)
public function boolean wf_carga_documentos (string as_tipo)
public function boolean duplicado (string as_valor, string as_columna)
end prototypes

public function integer wf_inicializa ();Integer	li_Imagen

plb_1.Reset()
plb_documentos.Reset()
plb_documentos.PictureHeight	=	20
plb_documentos.PictureWidth	=	20
li_Imagen	=	plb_documentos.AddPicture("\Desarrollo\BMP\click.bmp")

Return	li_Imagen
end function

public subroutine wf_carga_imagenes ();Long	ll_Fila, ll_New

For ll_Fila = 1 To plb_Documentos.TotalItems()
	If plb_Documentos.State(ll_Fila) = 1 Then
//		If wf_Duplicado(is_Ruta + '\', plb_Documentos.Text(ll_Fila)) Then
			dw_1.Object.cece_rutas[il_Fila]		=	is_Ruta + '\'
			dw_1.Object.cece_archiv[il_Fila]		=	plb_Documentos.Text(ll_Fila)
//		Else
//			MessageBox('Carga de Imagenes...', 'Este Archivo :' + plb_Documentos.Text(ll_Fila) + ' en la Ruta: ' + is_Ruta + '\ ya fue cargado.',  Information!, Ok!)
//		End If
	End If
Next
end subroutine

public function boolean wf_duplicado (string as_ruta, string as_archivo);Boolean	lb_Retorno = True
Long		ll_Busca

ll_Busca = dw_1.Find('cece_rutas = "' + as_Ruta + '" And cece_archiv = "' + as_Archivo + '"', 1, dw_1.RowCount())

If ll_Busca > 0 Then Return lb_Retorno = False

Return lb_Retorno
end function

public function boolean wf_carga_documentos (string as_tipo);Boolean	lb_Retorno = True
Integer	li_Imagen, li_Fila

li_Imagen	=	wf_inicializa()

If plb_1.DirList(is_Ruta + '\' + as_Tipo, 32) Then
	For li_Fila = 1 To plb_1.TotalItems()
		plb_documentos.AddItem(plb_1.Text(li_Fila), li_Imagen)
	Next
Else
	MessageBox('Alerta', 'No se pudo cargar informacion en ruta seleccionada', Information!, Ok!)
	lb_Retorno = False
End If

Return	lb_Retorno
end function

public function boolean duplicado (string as_valor, string as_columna);Integer 	li_Contador
String		ls_Especie, ls_Protocolo, ls_Inspeccion

ls_Especie		= String(dw_1.Object.espe_codigo[il_Fila])
ls_Protocolo		= String(dw_1.Object.prot_codigo[il_Fila])
ls_Inspeccion	= String(dw_1.Object.cece_nroins[il_Fila])

Choose Case as_columna
	Case 'espe_codigo'
		ls_Especie		= as_Valor
	Case 'prot_codigo'
		ls_Protocolo 	= as_Valor
	Case 'cece_nroins'
		ls_Inspeccion	= as_Valor
End Choose

li_Contador	= 	dw_1.Find("espe_codigo = " + ls_Especie + ' And prot_codigo = ' + ls_Protocolo + ' And cece_nroins = "' + ls_Inspeccion + '"', 1, dw_1.RowCount())

IF li_Contador > 0 THEN
	MessageBox("Duplicidad de Datos","Código ya existe, Ingrese Otro")
	RETURN TRUE
END IF

RETURN FALSE
end function

on w_mant_deta_certificados_prod.create
int iCurrent
call super::create
this.plb_1=create plb_1
this.plb_documentos=create plb_documentos
this.st_1=create st_1
this.pb_folder=create pb_folder
this.ddlb_filtro=create ddlb_filtro
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.plb_1
this.Control[iCurrent+2]=this.plb_documentos
this.Control[iCurrent+3]=this.st_1
this.Control[iCurrent+4]=this.pb_folder
this.Control[iCurrent+5]=this.ddlb_filtro
end on

on w_mant_deta_certificados_prod.destroy
call super::destroy
destroy(this.plb_1)
destroy(this.plb_documentos)
destroy(this.st_1)
destroy(this.pb_folder)
destroy(this.ddlb_filtro)
end on

event ue_recuperadatos;call super::ue_recuperadatos;ias_campo[1] = String(dw_1.Object.espe_codigo[il_Fila])
ias_campo[2] = String(dw_1.Object.cert_codigo[il_Fila])
ias_campo[3] = String(dw_1.Object.prot_codigo[il_Fila])
ias_campo[4] = String(dw_1.Object.cace_codigo[il_Fila])
ias_campo[5] = String(dw_1.Object.prec_codigo[il_Fila])
ias_campo[6] = dw_1.Object.cece_nroins[il_Fila]
ias_campo[7] = dw_1.Object.cece_ggngap[il_Fila]
ias_campo[8] = String(dw_1.Object.cece_fecaud[il_Fila], 'dd/mm/yyyy')
ias_campo[9] = String(dw_1.Object.cece_fecexp[il_Fila], 'dd/mm/yyyy')
ias_campo[10] = dw_1.Object.cece_observ[il_Fila]
ias_campo[11] = dw_1.Object.cece_rutas[il_Fila]
ias_campo[12] = dw_1.Object.cece_archiv[il_Fila]
ias_campo[13] = String(dw_1.Object.cece_montos[il_Fila], '#,##0.00')
ias_campo[14] = String(dw_1.Object.cece_fecins[il_Fila], 'dd/mm/yyyy')
ias_campo[15] = String(dw_1.Object.cece_feesau[il_Fila], 'dd/mm/yyyy')


If Not istr_mant.agrega Then
	dw_1.Object.espe_codigo.Protect	= 1
	dw_1.Object.espe_codigo.BackGround.Color = 553648127
End If

dw_1.GetChild('prpr_codigo', idwc_Predio)
idwc_Predio.SetTransObject(Sqlca)
idwc_Predio.Retrieve(Long(istr_mant.Argumento[1]))

dw_1.Object.prod_codigo[il_Fila] = Long(istr_mant.Argumento[1])
dw_1.Object.prpr_codigo[il_Fila] = Long(istr_mant.Argumento[2])
wf_carga_documentos(is_Filtro)
dw_1.SetFocus()


end event

event ue_deshace;call super::ue_deshace;dw_1.SetItem(il_fila, "espe_codigo", Integer(ias_campo[1]))
dw_1.SetItem(il_fila, "cert_codigo", Integer(ias_campo[2]))
dw_1.SetItem(il_fila, "prot_codigo", Integer(ias_campo[3]))
dw_1.SetItem(il_fila, "cace_codigo", Integer(ias_campo[4]))
dw_1.SetItem(il_fila, "prec_codigo", Integer(ias_campo[5]))
dw_1.SetItem(il_fila, "cece_nroins", ias_campo[6])
dw_1.SetItem(il_fila, "cece_ggngap", ias_campo[7])
dw_1.SetItem(il_fila, "cece_fecaud", Date(ias_campo[8]))
dw_1.SetItem(il_fila, "cece_fecexp", Date(ias_campo[9]))
dw_1.SetItem(il_fila, "cece_observ", ias_campo[10])
dw_1.SetItem(il_fila, "cece_rutas", ias_campo[11])
dw_1.SetItem(il_fila, "cece_archiv", ias_campo[12])
dw_1.SetItem(il_fila, "cece_montos", Dec(ias_campo[13]))
dw_1.SetItem(il_fila, "cece_fecins", Date(ias_campo[14]))
dw_1.SetItem(il_fila, "cece_feesau", Date(ias_campo[15]))

end event

event ue_antesguardar;call super::ue_antesguardar;Integer	li_cont
String   ls_mensaje, ls_colu[]

If Isnull(dw_1.GetItemNumber(il_fila, "espe_codigo")) OR dw_1.GetItemNumber (il_fila, "espe_codigo") = 0 Then
	li_cont ++
	ls_mensaje			= ls_mensaje + "~nCódigo de Especie"
	ls_colu[li_cont]	= "espe_codigo"
End If

If Isnull(dw_1.GetItemNumber(il_fila, "cert_codigo")) OR dw_1.GetItemNumber (il_fila, "cert_codigo") = 0 Then
	li_cont ++
	ls_mensaje			= ls_mensaje + "~nCódigo de Empresa Certificadora"
	ls_colu[li_cont]	= "cert_codigo"
End If

If Isnull(dw_1.GetItemNumber(il_fila, "prot_codigo")) OR dw_1.GetItemNumber (il_fila, "prot_codigo") = 0 Then
	li_cont ++
	ls_mensaje			= ls_mensaje + "~nCódigo de Protocolo"
	ls_colu[li_cont]	= "prot_codigo"
End If

If Isnull(dw_1.GetItemNumber(il_fila, "prec_codigo")) OR dw_1.GetItemNumber (il_fila, "prec_codigo") = 0 Then
	li_cont ++
	ls_mensaje			= ls_mensaje + "~nCódigo de Estado"
	ls_colu[li_cont]	= "prec_codigo"
End If

If Isnull(dw_1.GetItemNumber(il_fila, "prec_codigo")) OR dw_1.GetItemNumber (il_fila, "prec_codigo") = 0 Then
	li_cont ++
	ls_mensaje			= ls_mensaje + "~nCódigo de Estado"
	ls_colu[li_cont]	= "prec_codigo"
End If

If Isnull(dw_1.GetItemString(il_fila, "cece_nroins")) OR dw_1.GetItemString(il_fila, "cece_nroins") = "" Then
	li_cont ++
	ls_mensaje			= ls_mensaje + "~nNumero de Inscripción"
	ls_colu[li_cont]	= "cece_nroins"
End If

If Isnull(dw_1.GetItemString(il_fila, "cece_ggngap")) OR dw_1.GetItemString(il_fila, "cece_ggngap") = "" Then
	li_cont ++
	ls_mensaje			= ls_mensaje + "~nGGN GlobalGAP"
	ls_colu[li_cont]	= "cece_ggngap"
End If

If li_cont > 0 Then
	MessageBox("Error de Consistencia", "Falta el Ingreso de :" + ls_mensaje + ".", StopSign!, Ok!)
	dw_1.SetColumn(ls_colu[1])
	dw_1.SetFocus()
	Message.DoubleParm = -1
End If	
end event

event ue_nuevo;call super::ue_nuevo;dw_1.GetChild('prpr_codigo', idwc_Predio)
idwc_Predio.SetTransObject(Sqlca)
idwc_Predio.Retrieve(Long(istr_mant.Argumento[1]))

dw_1.Object.prod_codigo[il_Fila] = Long(istr_mant.Argumento[1])
dw_1.Object.prpr_codigo[il_Fila] = Long(istr_mant.Argumento[2])
end event

event open;call super::open;is_Filtro = '*.pdf'
ddlb_filtro.SelectItem(5)

iuo_Imagenes	=	Create uo_imagenes
iuo_planta		=	Create uo_plantadesp

dw_1.Getchild('prod_codigo', idwc_Productor)
idwc_Productor.SetTransObject(Sqlca)
idwc_Productor.Retrieve(-1)

RegistryGet( "HKEY_CURRENT_USER\Software\Microsoft\Windows\CurrentVersion\Explorer\Shell Folders", "Personal", RegString!, is_Ruta)

end event

event resize;Integer		li_posic_x, li_posic_y, li_Ancho = 300, li_Alto = 245, li_Siguiente = 255

This.Height			=	2100
This.Width			=	3826

dw_1.x				=	78
dw_1.y				=	72


li_posic_x			=	This.WorkSpaceWidth() - 300
li_posic_y			=	108

pb_acepta.width	=	li_Ancho
pb_acepta.height	=	li_Alto
pb_acepta.x			=	li_posic_x
pb_acepta.y			=	li_posic_y

pb_cancela.x		=	pb_acepta.x
pb_cancela.y		=	pb_acepta.y + li_Siguiente
pb_cancela.width	=	li_Ancho
pb_cancela.height	=	li_Alto

pb_folder.x			=	pb_acepta.x
pb_folder.y			=	pb_cancela.y + li_Siguiente
pb_folder.width		=	li_Ancho
pb_folder.height	=	li_Alto

pb_salir.x			=	pb_acepta.x
pb_salir.y			=	pb_folder.y + li_Siguiente
pb_salir.width		=	li_Ancho
pb_salir.height		=	li_Alto
end event

type pb_ultimo from w_mant_detalle_csd`pb_ultimo within w_mant_deta_certificados_prod
end type

type pb_siguiente from w_mant_detalle_csd`pb_siguiente within w_mant_deta_certificados_prod
end type

type pb_anterior from w_mant_detalle_csd`pb_anterior within w_mant_deta_certificados_prod
end type

type pb_primero from w_mant_detalle_csd`pb_primero within w_mant_deta_certificados_prod
end type

type pb_cancela from w_mant_detalle_csd`pb_cancela within w_mant_deta_certificados_prod
integer x = 3575
integer y = 268
end type

type pb_acepta from w_mant_detalle_csd`pb_acepta within w_mant_deta_certificados_prod
integer x = 3575
integer y = 108
integer taborder = 10
boolean default = false
end type

type pb_salir from w_mant_detalle_csd`pb_salir within w_mant_deta_certificados_prod
integer x = 3575
integer y = 588
boolean cancel = false
end type

type dw_1 from w_mant_detalle_csd`dw_1 within w_mant_deta_certificados_prod
integer width = 2208
integer height = 1780
integer taborder = 20
string dataobject = "dw_mant_certificacion_prod"
string icon = "AppIcon!"
end type

event dw_1::itemchanged;call super::itemchanged;Integer	li_null
String	ls_Columna

SetNull(li_Null)

ls_Columna	=	dwo.Name

Choose Case ls_Columna
	Case "espe_codigo"
		If Duplicado(data, ls_Columna) Then
			dw_1.SetItem(row, ls_Columna, li_Null)
			Return 1
		End If

	Case "prot_codigo"
		If Duplicado(data, ls_Columna) Then
			dw_1.SetItem(row, ls_Columna, li_Null)
			Return 1
		End If
		
	Case 'cece_packin'
		If Not iuo_Planta.Existe(Long(Data), True, SQLCA) Then
			dw_1.SetItem(row, ls_Columna, String(li_Null))
			Return 1
		Else
			This.Object.plde_tippac[Row] = iuo_Planta.TipoPacking
		End If
	
	Case 'cece_nroins'
		If Duplicado(data, ls_Columna) Then
			dw_1.SetItem(row, ls_Columna, String(li_Null))
			Return 1
		End If

	Case "cace_codigo"
		If Data <> '1' Then dw_1.SetItem(row, 'cece_montos', Dec(li_Null))
		
End Choose
end event

event dw_1::buttonclicked;call super::buttonclicked;String	ls_Boton, ls_Archivo

ls_Boton = dwo.Name

Choose Case ls_Boton
	Case 'b_visualiza'
		ls_Archivo	=	This.Object.cece_rutas[Row]+ This.Object.cece_archiv[Row]
		
		If IsNull(ls_Archivo) Then Return
		
		If FileExists(ls_Archivo) Then 
			iuo_Imagenes.AbrirDocumento(ls_Archivo)
		Else
			iuo_Imagenes.RecuperaImagen(This, Sqlca)
		End If
		
End Choose
end event

type plb_1 from picturelistbox within w_mant_deta_certificados_prod
boolean visible = false
integer x = 3607
integer y = 1492
integer width = 169
integer height = 116
integer taborder = 40
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
borderstyle borderstyle = stylelowered!
long picturemaskcolor = 536870912
end type

type plb_documentos from picturelistbox within w_mant_deta_certificados_prod
integer x = 2359
integer y = 156
integer width = 1111
integer height = 1732
integer taborder = 50
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
boolean hscrollbar = true
boolean vscrollbar = true
borderstyle borderstyle = stylelowered!
long picturemaskcolor = 536870912
end type

event doubleclicked;wf_carga_imagenes()	
end event

type st_1 from statictext within w_mant_deta_certificados_prod
integer x = 2373
integer y = 76
integer width = 210
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 553648127
string text = "Filtros"
boolean focusrectangle = false
end type

type pb_folder from picturebutton within w_mant_deta_certificados_prod
integer x = 3575
integer y = 428
integer width = 233
integer height = 196
integer taborder = 50
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string picturename = "\Desarrollo 17\Imagenes\Botones\Abrir Carpeta.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\Abrir Carpeta-bn.png"
alignment htextalign = left!
end type

event clicked;GetFolder('Seleccion de Directorio', is_Ruta)
wf_carga_documentos(is_Filtro)
end event

type ddlb_filtro from dropdownlistbox within w_mant_deta_certificados_prod
integer x = 2583
integer y = 52
integer width = 887
integer height = 1568
integer taborder = 30
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
boolean sorted = false
boolean vscrollbar = true
string item[] = {"*.jpg (Archivos Imagen .JPEG) ","*.bmp (Archivos Mapa de bits)","*.gif (Archivos Imagen .GIF)","*.doc (Archivos Word .DOC)","*.pdf (Archivos .PDF)","*.xls (Archivos Excel .XLS)","*.* (Todos los Archivos)"}
borderstyle borderstyle = stylelowered!
end type

event selectionchanged;Choose Case Index
	Case 1
		is_Filtro = '*.jpg'

	Case 2
		is_Filtro = '*.bmp'

	Case 3
		is_Filtro = '*.bmp'
		
	Case 4
		is_Filtro = '*.doc'
		
	Case 5
		is_Filtro = '*.pdf'

	Case 6
		is_Filtro = '*.xls'
		
	Case 7
		is_Filtro = '*.*'

End Choose

wf_carga_documentos(is_Filtro)
end event

