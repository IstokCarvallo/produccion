$PBExportHeader$w_info_compactorepa.srw
forward
global type w_info_compactorepa from w_para_informes
end type
type dw_1 from datawindow within w_info_compactorepa
end type
type gb_3 from groupbox within w_info_compactorepa
end type
end forward

global type w_info_compactorepa from w_para_informes
integer x = 521
integer y = 656
integer width = 2446
integer height = 1064
boolean controlmenu = false
boolean minbox = false
boolean maxbox = false
boolean resizable = false
windowtype windowtype = response!
dw_1 dw_1
gb_3 gb_3
end type
global w_info_compactorepa w_info_compactorepa

type variables
Str_mant	istr_mant

Integer	ii_planta

DataWindowchild idwc_mercado
end variables

on w_info_compactorepa.create
int iCurrent
call super::create
this.dw_1=create dw_1
this.gb_3=create gb_3
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_1
this.Control[iCurrent+2]=this.gb_3
end on

on w_info_compactorepa.destroy
call super::destroy
destroy(this.dw_1)
destroy(this.gb_3)
end on

event open;call super::open;istr_mant = Message.PowerObjectParm

dw_1.GetChild("merc_codigo", idwc_mercado)
idwc_mercado.SetTransObject(SQLCA)
idwc_mercado.Retrieve()
dw_1.InsertRow(0)


end event

type pb_excel from w_para_informes`pb_excel within w_info_compactorepa
end type

type st_computador from w_para_informes`st_computador within w_info_compactorepa
end type

type st_usuario from w_para_informes`st_usuario within w_info_compactorepa
end type

type st_temporada from w_para_informes`st_temporada within w_info_compactorepa
end type

type p_logo from w_para_informes`p_logo within w_info_compactorepa
end type

type st_titulo from w_para_informes`st_titulo within w_info_compactorepa
integer width = 1655
string text = "Seleccion Mercado"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_compactorepa
string tag = "Imprimir Reporte"
integer x = 1998
integer y = 404
integer taborder = 30
boolean enabled = false
end type

event pb_acepta::clicked;Integer	li_cliente, li_planta, li_Reimprime

istr_mant.argumento[17]	= String(dw_1.Object.merc_codigo[1])

CloseWithReturn(Parent,istr_mant)




end event

type pb_salir from w_para_informes`pb_salir within w_info_compactorepa
string tag = "Salir [Cerrar Ventana Activa]"
integer x = 2002
integer y = 652
integer taborder = 40
string powertiptext = "Salir [Cerrar Ventana Activa]"
end type

event pb_salir::clicked;istr_mant.argumento[17]	= '-1'


CloseWithReturn(Parent,istr_mant)
end event

type dw_1 from datawindow within w_info_compactorepa
integer x = 539
integer y = 608
integer width = 1006
integer height = 120
integer taborder = 50
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_mercado"
boolean border = false
boolean livescroll = true
end type

event itemchanged;pb_acepta.Enabled = True
end event

type gb_3 from groupbox within w_info_compactorepa
integer x = 233
integer y = 464
integer width = 1655
integer height = 400
integer taborder = 10
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 16711680
string text = "Selectivos"
end type

