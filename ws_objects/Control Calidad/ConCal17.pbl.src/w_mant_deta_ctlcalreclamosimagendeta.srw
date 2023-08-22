$PBExportHeader$w_mant_deta_ctlcalreclamosimagendeta.srw
$PBExportComments$Mantenedor de Planilla Cualitativa
forward
global type w_mant_deta_ctlcalreclamosimagendeta from w_mant_detalle_csd
end type
type plb_documentos from picturelistbox within w_mant_deta_ctlcalreclamosimagendeta
end type
type plb_1 from picturelistbox within w_mant_deta_ctlcalreclamosimagendeta
end type
type ddlb_filtro from dropdownlistbox within w_mant_deta_ctlcalreclamosimagendeta
end type
type st_1 from statictext within w_mant_deta_ctlcalreclamosimagendeta
end type
end forward

global type w_mant_deta_ctlcalreclamosimagendeta from w_mant_detalle_csd
integer x = 123
integer y = 96
integer width = 4475
integer height = 1888
string title = "Mantención de Imagenes"
plb_documentos plb_documentos
plb_1 plb_1
ddlb_filtro ddlb_filtro
st_1 st_1
end type
global w_mant_deta_ctlcalreclamosimagendeta w_mant_deta_ctlcalreclamosimagendeta

type variables
CONSTANT	Integer	MaximoImagenes = 15
String				is_Ruta, is_Filtro
end variables

forward prototypes
public function integer wf_inicializa ()
public function boolean wf_carga_documentos (string as_tipo)
public subroutine wf_carga_imagenes ()
public function boolean wf_duplicado (string as_ruta, string as_archivo)
end prototypes

public function integer wf_inicializa ();Integer	li_Imagen

plb_1.Reset()
plb_documentos.Reset()
plb_documentos.PictureHeight	=	20
plb_documentos.PictureWidth	=	20
li_Imagen	=	plb_documentos.AddPicture("\Desarrollo\BMP\click.bmp")

Return	li_Imagen
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

public subroutine wf_carga_imagenes ();Long	ll_Fila, ll_New

For ll_Fila = 1 To plb_Documentos.TotalItems()
	If plb_Documentos.State(ll_Fila) = 1 Then
		If wf_Duplicado(is_Ruta + '\', plb_Documentos.Text(ll_Fila)) Then
			ll_New	= dw_1.InsertRow(0)
			dw_1.Object.reen_numero[ll_New]	=	Long(istr_Mant.Argumento[1])
			dw_1.Object.espe_codigo[ll_New]		=	Integer(istr_Mant.Argumento[2])
			dw_1.Object.clie_codigo[ll_New]		=	gi_CodExport
			Choose case istr_mant.Respuesta
			Case 1
				dw_1.Object.reid_ruta[ll_New]			=	is_Ruta + '\'
				dw_1.Object.reid_archiv[ll_New]		=	plb_Documentos.Text(ll_Fila)
			Case 2
				dw_1.Object.repd_ruta[ll_New]			=	is_Ruta + '\'
				dw_1.Object.repd_archiv[ll_New]		=	plb_Documentos.Text(ll_Fila)
			End Choose
		Else
			MessageBox('Carga de Imagenes...', 'Este Archivo :' + plb_Documentos.Text(ll_Fila) + ' en la Ruta: ' + is_Ruta + '\ ya fue cargado.',  Information!, Ok!)
		End If
	End If
Next
end subroutine

public function boolean wf_duplicado (string as_ruta, string as_archivo);Boolean	lb_Retorno = True
Long		ll_Busca

Choose case istr_mant.Respuesta
	Case 1
		ll_Busca = dw_1.Find('reid_ruta = "' + as_Ruta + '" And reid_archiv = "' + as_Archivo + '"', 1, dw_1.RowCount())
	Case 2
		ll_Busca = dw_1.Find('repd_ruta = "' + as_Ruta + '" And repd_archiv = "' + as_Archivo + '"', 1, dw_1.RowCount())
End Choose

If ll_Busca > 0 Then Return lb_Retorno = False

Return lb_Retorno
end function

on w_mant_deta_ctlcalreclamosimagendeta.create
int iCurrent
call super::create
this.plb_documentos=create plb_documentos
this.plb_1=create plb_1
this.ddlb_filtro=create ddlb_filtro
this.st_1=create st_1
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.plb_documentos
this.Control[iCurrent+2]=this.plb_1
this.Control[iCurrent+3]=this.ddlb_filtro
this.Control[iCurrent+4]=this.st_1
end on

on w_mant_deta_ctlcalreclamosimagendeta.destroy
call super::destroy
destroy(this.plb_documentos)
destroy(this.plb_1)
destroy(this.ddlb_filtro)
destroy(this.st_1)
end on

event open;x	= 100
y	= 450
This.Icon	=	Gstr_apl.Icono

//PostEvent("ue_recuperadatos")
istr_mant = Message.PowerObjectParm
Choose case istr_mant.respuesta		
	case 1
		dw_1.DataObject ='dw_mues_ctlcalreclamosimagen'
		is_Filtro = '*.jpg'
	case 2
		dw_1.DataObject ='dw_mues_ctlcalreclamospdf'
		is_Filtro = '*.pdf'
		ddlb_filtro.enabled =False
End Choose

dw_1.SetTransObject(sqlca)
istr_mant.dw.ShareData(dw_1)
ddlb_filtro.SelectItem(1)

RegistryGet( "HKEY_CURRENT_USER\Software\Microsoft\Windows\CurrentVersion\Explorer\Shell Folders", "Personal", RegString!, is_Ruta)


GetFolder('Seleccion de Directorio', is_Ruta)
wf_carga_documentos(is_Filtro)
end event

event resize;//
end event

event ue_recuperadatos;wf_carga_documentos(is_Filtro)
end event

type pb_ultimo from w_mant_detalle_csd`pb_ultimo within w_mant_deta_ctlcalreclamosimagendeta
boolean visible = false
end type

type pb_siguiente from w_mant_detalle_csd`pb_siguiente within w_mant_deta_ctlcalreclamosimagendeta
boolean visible = false
end type

type pb_anterior from w_mant_detalle_csd`pb_anterior within w_mant_deta_ctlcalreclamosimagendeta
boolean visible = false
end type

type pb_primero from w_mant_detalle_csd`pb_primero within w_mant_deta_ctlcalreclamosimagendeta
boolean visible = false
end type

type pb_cancela from w_mant_detalle_csd`pb_cancela within w_mant_deta_ctlcalreclamosimagendeta
integer x = 4178
integer y = 420
string picturename = "\Desarrollo 12\Imagenes\Botones\BuscaArch.png"
string disabledname = "\Desarrollo 12\Imagenes\Botones\BuscaArchDisab.png"
end type

event pb_cancela::clicked;GetFolder('Seleccion de Directorio', is_Ruta)
wf_carga_documentos(is_Filtro)
end event

type pb_acepta from w_mant_detalle_csd`pb_acepta within w_mant_deta_ctlcalreclamosimagendeta
integer x = 4178
integer y = 200
end type

event pb_acepta::clicked;wf_carga_imagenes()
end event

type pb_salir from w_mant_detalle_csd`pb_salir within w_mant_deta_ctlcalreclamosimagendeta
integer x = 4174
integer y = 632
end type

event pb_salir::clicked;
CloseWithReturn(Parent, istr_mant)

end event

type dw_1 from w_mant_detalle_csd`dw_1 within w_mant_deta_ctlcalreclamosimagendeta
integer y = 76
integer width = 1911
integer height = 1636
string dataobject = "dw_mues_ctlcalreclamosimagen"
end type

type plb_documentos from picturelistbox within w_mant_deta_ctlcalreclamosimagendeta
integer x = 2080
integer y = 168
integer width = 1947
integer height = 1544
integer taborder = 30
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
boolean extendedselect = true
long picturemaskcolor = 536870912
end type

event doubleclicked;wf_carga_imagenes()
end event

type plb_1 from picturelistbox within w_mant_deta_ctlcalreclamosimagendeta
boolean visible = false
integer x = 3973
integer y = 1532
integer width = 407
integer height = 176
integer taborder = 50
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
boolean border = false
borderstyle borderstyle = stylelowered!
long picturemaskcolor = 536870912
end type

type ddlb_filtro from dropdownlistbox within w_mant_deta_ctlcalreclamosimagendeta
integer x = 2295
integer y = 60
integer width = 1733
integer height = 400
integer taborder = 20
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
boolean sorted = false
string item[] = {"*.jpg (Archivos Imagen .JPEG) ","*.bmp (Archivos Mapa de bits)","*.gif (Archivos Imagen .GIF)",""}
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
		is_Filtro = '*.*'

End Choose

wf_carga_documentos(is_Filtro)
end event

type st_1 from statictext within w_mant_deta_ctlcalreclamosimagendeta
integer x = 2085
integer y = 72
integer width = 197
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
string text = "Filtro"
boolean focusrectangle = false
end type

