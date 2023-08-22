$PBExportHeader$w_gestionpatio.srw
forward
global type w_gestionpatio from w_systray
end type
type dw_planta from uo_dw within w_gestionpatio
end type
type dw_3 from uo_dw within w_gestionpatio
end type
type dw_4 from uo_dw within w_gestionpatio
end type
type dw_5 from uo_dw within w_gestionpatio
end type
type dw_6 from uo_dw within w_gestionpatio
end type
type st_1 from statictext within w_gestionpatio
end type
type em_tiempo from editmask within w_gestionpatio
end type
end forward

global type w_gestionpatio from w_systray
integer width = 4731
integer height = 2112
string title = "Consulta Folios"
boolean minbox = true
boolean maxbox = true
boolean resizable = true
windowstate windowstate = maximized!
long backcolor = 553648127
dw_planta dw_planta
dw_3 dw_3
dw_4 dw_4
dw_5 dw_5
dw_6 dw_6
st_1 st_1
em_tiempo em_tiempo
end type
global w_gestionpatio w_gestionpatio

type variables
Integer	ii_Tiempo = 10
end variables

on w_gestionpatio.create
int iCurrent
call super::create
this.dw_planta=create dw_planta
this.dw_3=create dw_3
this.dw_4=create dw_4
this.dw_5=create dw_5
this.dw_6=create dw_6
this.st_1=create st_1
this.em_tiempo=create em_tiempo
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_planta
this.Control[iCurrent+2]=this.dw_3
this.Control[iCurrent+3]=this.dw_4
this.Control[iCurrent+4]=this.dw_5
this.Control[iCurrent+5]=this.dw_6
this.Control[iCurrent+6]=this.st_1
this.Control[iCurrent+7]=this.em_tiempo
end on

on w_gestionpatio.destroy
call super::destroy
destroy(this.dw_planta)
destroy(this.dw_3)
destroy(this.dw_4)
destroy(this.dw_5)
destroy(this.dw_6)
destroy(this.st_1)
destroy(this.em_tiempo)
end on

event open;call super::open;dw_1.SetTransObject(Sqlca)
dw_2.SetTransObject(Sqlca)
dw_3.SetTransObject(Sqlca)
dw_4.SetTransObject(Sqlca)
dw_5.SetTransObject(Sqlca)
dw_6.SetTransObject(Sqlca)
dw_Planta.SetTransObject(Sqlca)

dw_1.Retrieve(1)
dw_2.Retrieve(2)
dw_3.Retrieve(3)
dw_4.Retrieve(4)
dw_5.Retrieve(5)
dw_6.Retrieve(6)
dw_Planta.Retrieve(gstr_paramplanta.CodigoPlanta)

dw_1.Object.gr_1.Title = 'Folios por Especie / Envases'
dw_2.Object.gr_1.Title = 'Folios por Especie / Altura'
dw_3.Object.gr_1.Title = 'Folios por Especie / Envoltorios'
dw_4.Object.gr_1.Title = 'Folios por Especie / Peso'
dw_5.Object.gr_1.Title = 'Folios por Especie'

em_tiempo.Text = '10'

in_tray.of_delete_icon(This, True)

Timer(ii_Tiempo)
end event

event timer;Timer(0)

dw_1.Retrieve(1)
dw_2.Retrieve(2)
dw_3.Retrieve(3)
dw_4.Retrieve(4)
dw_5.Retrieve(5)
dw_6.Retrieve(6)

Timer(ii_Tiempo)
end event

event resize;st_1.x							= 40
st_1.y							= 40
em_tiempo.x				=	st_1.x +  st_1.Width + 30

st_encabezado.x 			= st_1.x
st_encabezado.y 			= st_1.y + st_1.Height + 25
st_encabezado.Width		= This.WorkSpaceWidth() - 400

dw_Planta.x 			= st_encabezado.x
dw_Planta.y 			= st_encabezado.y 
dw_Planta.Width		= st_encabezado.Width

dw_1.x		=	st_encabezado.x
dw_1.y		=	st_encabezado.y + st_encabezado.Height + 25
dw_1.Height	=	(This.WorkSpaceHeight() - (dw_1.y + 60)) / 2
dw_1.Width	=	(This.WorkSpaceWidth() - (dw_1.x + 60)) / 3

dw_2.x		=	dw_1.x + dw_1.Width + 10
dw_2.y		=	dw_1.y
dw_2.Height	=	dw_1.Height	
dw_2.Width	=	dw_1.Width	

dw_3.x		=	dw_2.x + dw_2.Width + 10
dw_3.y		=	dw_1.y
dw_3.Height	=	dw_1.Height	
dw_3.Width	=	dw_1.Width	

dw_4.x		=	dw_1.x
dw_4.y		=	dw_1.y + dw_1.Height + 25
dw_4.Height	=	dw_1.Height
dw_4.Width	=	dw_1.Width	

dw_5.x		=	dw_2.x
dw_5.y		=	dw_4.y
dw_5.Height	=	dw_4.Height
dw_5.Width	=	dw_4.Width	

dw_6.x		=	dw_3.x
dw_6.y		=	dw_4.y
dw_6.Height	=	dw_4.Height
dw_6.Width	=	dw_4.Width	

pb_salir.x			= This.WorkSpaceWidth() - 292
pb_salir.y			= dw_Planta.y
pb_salir.Width		= 300
pb_salir.Height		= 245
end event

type st_encabezado from w_systray`st_encabezado within w_gestionpatio
integer x = 41
integer y = 40
integer width = 4087
long backcolor = 8388608
end type

type dw_2 from w_systray`dw_2 within w_gestionpatio
integer x = 1499
integer y = 416
integer width = 841
integer height = 464
string title = "2"
string dataobject = "dw_graf_embalajes"
boolean vscrollbar = false
end type

type dw_1 from w_systray`dw_1 within w_gestionpatio
integer x = 265
integer y = 396
integer width = 1111
integer height = 464
string title = "1"
string dataobject = "dw_graf_embalajes"
boolean vscrollbar = false
end type

type pb_salir from w_systray`pb_salir within w_gestionpatio
integer x = 4384
integer y = 1636
integer width = 302
integer height = 244
boolean cancel = true
boolean flatstyle = true
string picturename = "\Desarrollo 17\Imagenes\Botones\Apagar.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\Apagar-bn.png"
string powertiptext = "Salir"
end type

type dw_planta from uo_dw within w_gestionpatio
integer x = 41
integer y = 56
integer width = 3707
integer height = 292
integer taborder = 11
boolean bringtotop = true
string dataobject = "dw_mues_plantadesp"
boolean vscrollbar = false
boolean border = false
borderstyle borderstyle = styleshadowbox!
end type

type dw_3 from uo_dw within w_gestionpatio
integer x = 3104
integer y = 440
integer taborder = 11
boolean bringtotop = true
string title = "3"
string dataobject = "dw_graf_embalajes"
boolean vscrollbar = false
end type

type dw_4 from uo_dw within w_gestionpatio
integer x = 846
integer y = 1232
integer taborder = 21
boolean bringtotop = true
string title = "4"
string dataobject = "dw_graf_embalajes"
boolean vscrollbar = false
end type

type dw_5 from uo_dw within w_gestionpatio
integer x = 1737
integer y = 1216
integer taborder = 21
boolean bringtotop = true
string title = "5"
string dataobject = "dw_graf_embalajes"
boolean vscrollbar = false
end type

type dw_6 from uo_dw within w_gestionpatio
integer x = 2523
integer y = 1200
integer taborder = 21
boolean bringtotop = true
string title = "6"
string dataobject = "dw_mues_tiemporecepcion"
boolean vscrollbar = false
end type

type st_1 from statictext within w_gestionpatio
integer x = 2642
integer y = 52
integer width = 526
integer height = 60
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Tiempo Refresco"
boolean focusrectangle = false
end type

type em_tiempo from editmask within w_gestionpatio
string tag = "Tiempo Refresco"
integer x = 3214
integer y = 48
integer width = 251
integer height = 84
integer taborder = 21
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
boolean border = false
alignment alignment = center!
string mask = "0000"
double increment = 1
string minmax = "1~~9999"
end type

event modified;If IsNull(This.Text) Or INteger(This.Text) = 0 Then Return

ii_Tiempo = Integer(This.Text)
end event

