$PBExportHeader$w_busc_movtocamaenca.srw
$PBExportComments$busqueda de movimiento de traspaso de camaras
forward
global type w_busc_movtocamaenca from w_busqueda
end type
type st_1 from statictext within tabpage_1
end type
type dw_planta from datawindow within tabpage_1
end type
type st_2 from statictext within tabpage_1
end type
type em_fecha from editmask within tabpage_1
end type
end forward

global type w_busc_movtocamaenca from w_busqueda
string tag = "Lotes Fruta Grnael en Existencia"
integer x = 123
integer y = 304
integer width = 3127
string title = "Búsqueda de Lotes Fruta Granel Existencia"
end type
global w_busc_movtocamaenca w_busc_movtocamaenca

type variables
Datawindowchild  idwc_planta, idwc_camara, idwc_especie

end variables

on w_busc_movtocamaenca.create
int iCurrent
call super::create
end on

on w_busc_movtocamaenca.destroy
call super::destroy
end on

event open;call super::open;istr_busq	=	Message.PowerObjectParm

is_ordena = 'Número Folio:mvce_numero,Cámara Origen:cama_nombre:Cámara Destino:camarasfrigo_cama_nombre,Fecha:mvce_fecmov'

tab_1.tabpage_1.dw_planta.GetChild("plde_codigo", idwc_planta)
idwc_planta.SetTransObject(SqlCa)
IF idwc_planta.Retrieve() = 0 THEN
	MessageBox("Atención","Falta Registrar Plantas")
	idwc_planta.InsertRow(0)
END IF

tab_1.tabpage_1.dw_planta.SetTransObject(SQLCA)

tab_1.tabpage_1.dw_planta.InsertRow(0)

IF istr_busq.Argum[1] <> "" THEN
	tab_1.tabpage_1.dw_planta.SetItem(1,"plde_codigo",Integer(istr_busq.Argum[1]))
	tab_1.tabpage_1.dw_planta.Object.plde_codigo.BackGround.Color	=	RGB(192,192,192)
	tab_1.tabpage_1.dw_planta.Enabled	=	False
END IF

IF istr_busq.Argum[2] <> "" THEN
	tab_1.tabpage_1.em_fecha.text=mid(istr_busq.argum[2],1,10)
END IF

tab_1.tabpage_1.pb_filtrar.SetFocus()


end event

event ue_asignacion();istr_Busq.Argum[1]	= String(dw_1.Object.plde_codigo[dw_1.GetRow()])
istr_Busq.Argum[2]	= ""
istr_Busq.Argum[3]	= String(dw_1.Object.mvce_numero[dw_1.GetRow()])


CloseWithReturn(This,istr_busq)
end event

event resize;call super::resize;Long		maximo

IF tab_1.Width > dw_1.Width THEN
	maximo = tab_1.Width
ELSE
	maximo = dw_1.Width
END IF

This.Width		= maximo + 540
//gb_1.x 			= This.WorkSpaceWidth() - 351
//gb_1.y			= This.WorkSpaceHeight() - 348
//gb_2.x			= gb_1.x
//gb_2.y			= gb_1.y - 304
pb_salir.x		= This.WorkSpaceWidth() - 292
pb_salir.y		= This.WorkSpaceHeight() - 264
pb_insertar.x	= This.WorkSpaceWidth() - 292
pb_insertar.y	= pb_salir.y - 304
tab_1.x			= 78
tab_1.y			= 69
dw_1.x			= 78
dw_1.y			= tab_1.Height + 136

end event

type pb_insertar from w_busqueda`pb_insertar within w_busc_movtocamaenca
boolean visible = false
integer x = 2601
integer y = 1068
integer taborder = 50
end type

type dw_1 from w_busqueda`dw_1 within w_busc_movtocamaenca
integer x = 110
integer y = 712
integer width = 2071
integer taborder = 60
string dataobject = "dw_mant_mues_spro_movtocamarafgenca"
end type

type pb_salir from w_busqueda`pb_salir within w_busc_movtocamaenca
integer y = 1384
end type

event pb_salir::clicked;istr_busq.argum[1] = ""
istr_busq.argum[2] = ""
istr_busq.argum[3] = ""
istr_busq.argum[4] = ""
istr_busq.argum[5] = ""
istr_busq.argum[6] = ""

CloseWithReturn(Parent,istr_busq)
end event

type tab_1 from w_busqueda`tab_1 within w_busc_movtocamaenca
integer x = 105
integer y = 80
integer width = 2066
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
integer width = 2030
string text = "Filtros              "
st_1 st_1
dw_planta dw_planta
st_2 st_2
em_fecha em_fecha
end type

on tabpage_1.create
this.st_1=create st_1
this.dw_planta=create dw_planta
this.st_2=create st_2
this.em_fecha=create em_fecha
int iCurrent
call super::create
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_1
this.Control[iCurrent+2]=this.dw_planta
this.Control[iCurrent+3]=this.st_2
this.Control[iCurrent+4]=this.em_fecha
end on

on tabpage_1.destroy
call super::destroy
destroy(this.st_1)
destroy(this.dw_planta)
destroy(this.st_2)
destroy(this.em_fecha)
end on

type pb_filtrar from w_busqueda`pb_filtrar within tabpage_1
integer x = 1605
integer y = 296
integer taborder = 40
boolean default = false
end type

event pb_filtrar::clicked;call super::clicked;
Date  ld_fecha
ld_Fecha = Date('01/01/1900')
Istr_busq.argum[2] = em_fecha.Text

IF (istr_busq.argum[1] <> "") THEN
	IF Date(istr_busq.argum[2])	> ld_Fecha   THEN
	  IF dw_1.Retrieve(Integer(istr_busq.argum[1]),Date(istr_busq.argum[2])) > 0 THEN
			dw_1.SetFocus()
			dw_1.SelectRow(1,True)
		ELSE
			MessageBox("Atención","No hay información para mostrar",Exclamation!,Ok!)
		END IF
	ELSE
			MessageBox("Atención","Fecha invalida")
			em_fecha.Text = String(Today())
			dw_1.Reset()
	END IF
END IF	
end event

type tabpage_2 from w_busqueda`tabpage_2 within tab_1
integer width = 2030
string text = "Ordenamiento  "
end type

type pb_acepta from w_busqueda`pb_acepta within tabpage_2
integer x = 1760
integer y = 312
end type

type dw_3 from w_busqueda`dw_3 within tabpage_2
integer x = 695
integer width = 914
end type

type dw_2 from w_busqueda`dw_2 within tabpage_2
integer width = 658
end type

type tabpage_3 from w_busqueda`tabpage_3 within tab_1
integer width = 2030
string text = "Búsqueda          "
end type

type sle_argumento2 from w_busqueda`sle_argumento2 within tabpage_3
integer x = 645
integer y = 256
end type

event sle_argumento2::getfocus;This.SelectText(1, Len(This.Text))

This.BackColor					= RGB(255,255,255)
This.TabOrder					= 10
sle_argumento1.Text			= ""
sle_argumento1.BackColor	= RGB(192,192,192)
sle_argumento1.TabOrder		= 0
es_numero						= False
is_busca							= "cama_nombre"
end event

type st_argum2 from w_busqueda`st_argum2 within tabpage_3
integer x = 169
integer y = 264
string text = "Camara Or."
end type

type sle_argumento1 from w_busqueda`sle_argumento1 within tabpage_3
integer x = 645
integer y = 136
integer width = 306
end type

event sle_argumento1::getfocus;This.SelectText(1, Len(This.Text))

This.BackColor					= RGB(255,255,255)
This.TabOrder					= 10
sle_argumento2.Text			= ""
sle_argumento2.BackColor	= RGB(192,192,192)
sle_argumento2.TabOrder		= 0
es_numero						= True
is_busca							= "mvce_numero"
end event

type st_argum1 from w_busqueda`st_argum1 within tabpage_3
integer x = 169
integer y = 144
integer width = 407
string text = "Número Folio"
end type

type pb_buscar from w_busqueda`pb_buscar within tabpage_3
integer x = 1760
integer y = 300
end type

type st_1 from statictext within tabpage_1
integer x = 91
integer y = 112
integer width = 288
integer height = 84
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 8388608
long backcolor = 553648127
string text = "Planta"
boolean focusrectangle = false
end type

type dw_planta from datawindow within tabpage_1
integer x = 421
integer y = 92
integer width = 873
integer height = 96
integer taborder = 10
boolean bringtotop = true
string dataobject = "dddw_planta"
boolean border = false
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event itemchanged;istr_busq.Argum[1]	=	data
		

end event

type st_2 from statictext within tabpage_1
integer x = 96
integer y = 264
integer width = 288
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 8388608
long backcolor = 553648127
string text = "Fecha"
boolean focusrectangle = false
end type

type em_fecha from editmask within tabpage_1
string tag = "Fecha Mínima de Búsqueda"
integer x = 425
integer y = 248
integer width = 494
integer height = 92
integer taborder = 21
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long backcolor = 1090519039
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
end type

event modified;istr_busq.argum[2] = this.text
end event

