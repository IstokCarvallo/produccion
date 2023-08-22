$PBExportHeader$w_busc_lotescomdeta_archivo.srw
$PBExportComments$busqueda de pallet por movimiento
forward
global type w_busc_lotescomdeta_archivo from w_busqueda
end type
type st_1 from statictext within tabpage_1
end type
type pb_cargar from picturebutton within tabpage_1
end type
type dw_archivos from datawindow within tabpage_1
end type
type pb_archivo from picturebutton within w_busc_lotescomdeta_archivo
end type
end forward

global type w_busc_lotescomdeta_archivo from w_busqueda
integer x = 123
integer y = 304
integer width = 3931
integer height = 1888
string title = "Búsqueda de Lotes Por Archivo"
pb_archivo pb_archivo
end type
global w_busc_lotescomdeta_archivo w_busc_lotescomdeta_archivo

type variables
String	is_archivos[]
Boolean	ib_todos
end variables

on w_busc_lotescomdeta_archivo.create
int iCurrent
call super::create
this.pb_archivo=create pb_archivo
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.pb_archivo
end on

on w_busc_lotescomdeta_archivo.destroy
call super::destroy
destroy(this.pb_archivo)
end on

event open;DataWindowChild		dwc1, ldwc_especie

istr_busq	= Message.PowerObjectParm

Call SUPER::Open

Tab_1.TabPage_1.dw_archivos.SetTransObject(SQLCa)

is_ordena	=	'Nro Lote:lofc_lotefc,' + &
					'Variedad:vari_codigo,Bultos:paen_ccajas'
					
Tab_1.TabPage_1.dw_archivos.SetReDraw(False)

IF Tab_1.TabPage_1.dw_archivos.Retrieve() > 0 THEN
	Tab_1.TabPage_1.dw_archivos.SetReDraw(TRUE)				  
	Tab_1.TabPage_1.dw_archivos.SetFocus()
	Tab_1.TabPage_1.dw_archivos.SelectRow(1, True)
	
ELSE
	MessageBox("Atención","No hay información para mostrar", Exclamation!, Ok!)
	istr_busq.Argum[1] = ""
	istr_busq.Argum[2] = ""
	istr_busq.Argum[3] = ""
	CloseWithReturn(This, istr_busq)
	
END IF
end event

event ue_asignacion;istr_busq.argum[1]	= String(dw_1.Object.lofc_pltcod[dw_1.GetRow()])
istr_busq.argum[2]	= String(dw_1.Object.lofc_espcod[dw_1.GetRow()])

IF ib_todos THEN
	istr_busq.argum[3]= '-1'
ELSE
	istr_busq.argum[3]= String(dw_1.Object.lofc_lotefc[dw_1.GetRow()])
END IF

istr_busq.argum[4]	= String(dw_1.Object.lfcd_archiv[dw_1.GetRow()])

CloseWithReturn(This, istr_busq)
end event

type pb_insertar from w_busqueda`pb_insertar within w_busc_lotescomdeta_archivo
boolean visible = false
integer x = 3570
integer y = 940
integer taborder = 50
boolean enabled = false
boolean cancel = false
end type

type dw_1 from w_busqueda`dw_1 within w_busc_lotescomdeta_archivo
integer x = 91
integer y = 748
integer width = 3438
integer height = 992
integer taborder = 60
boolean titlebar = true
string title = "Pallets en Archivo"
string dataobject = "dw_mues_spro_lotesfrutacomdeta_grupo_arc"
boolean hsplitscroll = true
end type

event dw_1::doubleclicked;//ib_todos	=	False
//
//IF row > 0 THEN
//	Parent.PostEvent("ue_asignacion")
//END IF

pb_archivo.TriggerEvent("Clicked")
end event

type pb_salir from w_busqueda`pb_salir within w_busc_lotescomdeta_archivo
integer x = 3630
integer y = 1340
end type

event pb_salir::clicked;istr_busq.Argum[1] = ""
istr_busq.Argum[2] = ""
istr_busq.Argum[3] = ""
istr_busq.Argum[4] = ""
CloseWithReturn(Parent,istr_busq)
end event

type tab_1 from w_busqueda`tab_1 within w_busc_lotescomdeta_archivo
integer x = 754
integer y = 64
integer width = 2331
boolean fixedwidth = true
end type

on tab_1.create
call super::create
this.Control[]={this.tabpage_1,&
this.tabpage_2,&
this.tabpage_3}
end on

on tab_1.destroy
call super::destroy
end on

type tabpage_1 from w_busqueda`tabpage_1 within tab_1
integer width = 2295
string text = "Archivos"
st_1 st_1
pb_cargar pb_cargar
dw_archivos dw_archivos
end type

on tabpage_1.create
this.st_1=create st_1
this.pb_cargar=create pb_cargar
this.dw_archivos=create dw_archivos
int iCurrent
call super::create
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_1
this.Control[iCurrent+2]=this.pb_cargar
this.Control[iCurrent+3]=this.dw_archivos
end on

on tabpage_1.destroy
call super::destroy
destroy(this.st_1)
destroy(this.pb_cargar)
destroy(this.dw_archivos)
end on

type pb_filtrar from w_busqueda`pb_filtrar within tabpage_1
integer x = 2747
integer y = 308
boolean default = false
end type

event pb_filtrar::clicked;call super::clicked;IF dw_1.Retrieve(Integer(istr_busq.argum[1])) > 0 THEN
	dw_1.SetFocus()
	dw_1.SelectRow(1,True)
ELSE
	MessageBox("Atención","No hay información para mostrar",Exclamation!,Ok!)
END IF
end event

type tabpage_2 from w_busqueda`tabpage_2 within tab_1
integer width = 2295
string text = "Ordenamiento    "
end type

type pb_acepta from w_busqueda`pb_acepta within tabpage_2
integer x = 1865
end type

type dw_3 from w_busqueda`dw_3 within tabpage_2
integer y = 40
end type

type dw_2 from w_busqueda`dw_2 within tabpage_2
integer y = 40
end type

type tabpage_3 from w_busqueda`tabpage_3 within tab_1
integer width = 2295
string text = "Búsqueda    "
end type

type sle_argumento2 from w_busqueda`sle_argumento2 within tabpage_3
integer x = 439
integer y = 244
integer width = 1243
end type

event sle_argumento2::getfocus;call super::getfocus;This.Text						= ""
This.BackColor					= RGB(255,255,255)
This.TabOrder					= 10
sle_argumento1.Text			= ""
sle_argumento1.BackColor	= RGB(192,192,192)
sle_argumento2.Text			= ""
sle_argumento2.BackColor	= RGB(192,192,192)
sle_argumento2.TabOrder		= 0
es_numero						= False
is_busca							= "vari_nombre"
end event

type st_argum2 from w_busqueda`st_argum2 within tabpage_3
integer x = 78
integer y = 252
integer width = 325
string text = "Variedad"
end type

type sle_argumento1 from w_busqueda`sle_argumento1 within tabpage_3
integer x = 439
integer y = 124
integer width = 210
end type

event sle_argumento1::getfocus;This.Text						= ""
This.BackColor					= RGB(255,255,255)
This.TabOrder					= 10
sle_argumento2.Text			= ""
sle_argumento2.BackColor	= RGB(192,192,192)
sle_argumento2.TabOrder		= 0
es_numero						= True
is_busca							= "paen_numero"
end event

type st_argum1 from w_busqueda`st_argum1 within tabpage_3
integer x = 73
integer y = 132
integer width = 302
string text = "Nro Pallet"
end type

type pb_buscar from w_busqueda`pb_buscar within tabpage_3
integer x = 1874
integer y = 304
end type

type st_1 from statictext within tabpage_1
integer x = 78
integer y = 60
integer width = 270
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 8388608
long backcolor = 553648127
string text = "Archivos"
boolean focusrectangle = false
end type

type pb_cargar from picturebutton within tabpage_1
integer x = 1870
integer y = 312
integer width = 155
integer height = 132
integer taborder = 61
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string picturename = "\Desarrollo 17\Imagenes\Botones\Aceptar base datos.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\Aceptar base datos-bn.png"
alignment htextalign = left!
end type

event clicked;Integer	li_filas
String	ls_archivo[]

is_archivos	=	ls_archivo

FOR li_filas = 1 TO dw_archivos.RowCount()
	IF dw_archivos.IsSelected(li_filas) THEN
		is_archivos[UpperBound(is_archivos) + 1]	=	dw_archivos.Object.lfcd_archiv[li_filas]
	END IF
NEXT

dw_1.SetReDraw(False)

IF UpperBound(is_archivos) > 0 THEN
	IF UpperBound(is_archivos) > 1 THEN
		MessageBox("Error", "Solo debe cargar un archivo por proceso", Exclamation!)
		Return
	ELSE
		IF dw_1.Retrieve(is_archivos) > 0 THEN
			dw_1.SetReDraw(TRUE)				  
			dw_1.SetFocus()
			dw_1.SelectRow(1,True)
		END IF
	END IF
ELSE
	MessageBox("Error", "Debe Seleccionar un archivo para cargar los datos")
END IF

ib_todos	=	False
end event

type dw_archivos from datawindow within tabpage_1
integer x = 553
integer y = 36
integer width = 905
integer height = 404
integer taborder = 21
boolean bringtotop = true
string title = "none"
string dataobject = "dw_listbox_archivos_lotescomdeta"
boolean hscrollbar = true
boolean vscrollbar = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event clicked;IF Row > 0 THEN
	This.SelectRow(Row, NOT This.IsSelected(Row))
	
END IF
end event

type pb_archivo from picturebutton within w_busc_lotescomdeta_archivo
string tag = "Todos los Pallets del Archivo"
integer x = 3630
integer y = 1524
integer width = 155
integer height = 132
integer taborder = 60
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string picturename = "\Desarrollo\Bmp\Apuntee.bmp"
string disabledname = "\Desarrollo\Bmp\Apunted.bmp"
alignment htextalign = left!
end type

event clicked;IF UpperBound(is_archivos) > 1 THEN
	MessageBox("Error", "Debe Seleccionar SOLO UN Archivo para cargar todos los pallets")
ELSE
	IF UpperBound(is_archivos) < 1 THEN
		MessageBox("Error", "Debe Seleccionar al MENOS UN Archivo para cargar todos los pallets")
	ELSE
		ib_todos	=	True
		Parent.TriggerEvent("ue_asignacion")
	END IF
END IF
end event

