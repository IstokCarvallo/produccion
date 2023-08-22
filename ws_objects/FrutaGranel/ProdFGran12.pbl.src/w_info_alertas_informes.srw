$PBExportHeader$w_info_alertas_informes.srw
$PBExportComments$Ventana de Informe de Control Calidad de Daños y Defectos
forward
global type w_info_alertas_informes from w_para_informes
end type
type st_3 from statictext within w_info_alertas_informes
end type
type st_1 from statictext within w_info_alertas_informes
end type
type st_6 from statictext within w_info_alertas_informes
end type
type tab_1 from tab within w_info_alertas_informes
end type
type tabpage_1 from userobject within tab_1
end type
type dw_existencia from datawindow within tabpage_1
end type
type tabpage_1 from userobject within tab_1
dw_existencia dw_existencia
end type
type tabpage_2 from userobject within tab_1
end type
type tabpage_2 from userobject within tab_1
end type
type tab_1 from tab within w_info_alertas_informes
tabpage_1 tabpage_1
tabpage_2 tabpage_2
end type
type uo_selcliente from uo_seleccion_clientesprod within w_info_alertas_informes
end type
type uo_selplantas from uo_seleccion_plantas within w_info_alertas_informes
end type
end forward

global type w_info_alertas_informes from w_para_informes
integer width = 5230
integer height = 2256
string title = "CONSULTA EN LINEA"
boolean controlmenu = false
boolean minbox = false
boolean maxbox = false
event ue_recuperadatos ( )
event ue_imprimir ( )
st_3 st_3
st_1 st_1
st_6 st_6
tab_1 tab_1
uo_selcliente uo_selcliente
uo_selplantas uo_selplantas
end type
global w_info_alertas_informes w_info_alertas_informes

type variables

end variables

on w_info_alertas_informes.create
int iCurrent
call super::create
this.st_3=create st_3
this.st_1=create st_1
this.st_6=create st_6
this.tab_1=create tab_1
this.uo_selcliente=create uo_selcliente
this.uo_selplantas=create uo_selplantas
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_3
this.Control[iCurrent+2]=this.st_1
this.Control[iCurrent+3]=this.st_6
this.Control[iCurrent+4]=this.tab_1
this.Control[iCurrent+5]=this.uo_selcliente
this.Control[iCurrent+6]=this.uo_selplantas
end on

on w_info_alertas_informes.destroy
call super::destroy
destroy(this.st_3)
destroy(this.st_1)
destroy(this.st_6)
destroy(this.tab_1)
destroy(this.uo_selcliente)
destroy(this.uo_selplantas)
end on

event open;Boolean	lb_Cerrar = False

If IsNull(uo_SelCliente.Codigo) Then lb_Cerrar = True
If IsNull(uo_SelPlantas.Codigo) Then lb_Cerrar = True

If lb_Cerrar Then
	Close(This)
Else
	Tab_1.TabPage_1.dw_existencia.SetTransObject(sqlca)
	
	uo_SelCliente.Seleccion(False, False)
	uo_SelPlantas.Seleccion(False, False)
	uo_SelCliente.Inicia(81)
	uo_SelPlantas.Inicia(gstr_ParamPlanta.CodigoPlanta)
		
End If
end event

type pb_excel from w_para_informes`pb_excel within w_info_alertas_informes
integer x = 3744
integer y = 28
end type

type st_computador from w_para_informes`st_computador within w_info_alertas_informes
end type

type st_usuario from w_para_informes`st_usuario within w_info_alertas_informes
end type

type st_temporada from w_para_informes`st_temporada within w_info_alertas_informes
end type

type p_logo from w_para_informes`p_logo within w_info_alertas_informes
end type

type st_titulo from w_para_informes`st_titulo within w_info_alertas_informes
integer width = 4425
integer height = 96
string text = "Alertas "
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_alertas_informes
integer x = 4763
integer y = 892
integer taborder = 80
string picturename = "\Desarrollo 12\Imagenes\Botones\BuscaArch.png"
string disabledname = "\Desarrollo 12\Imagenes\Botones\BuscaArchDisab.png"
end type

event pb_acepta::clicked;Long		ll_fila, respuesta
	
ll_fila = tab_1.tabpage_1.dw_existencia.Retrieve(uo_SelPlantas.Codigo, uo_SelCliente.Codigo)

IF ll_fila = -1 THEN
	respuesta = MessageBox("Error en Base de Datos", "No es posible conectar la Base de Datos.", Information!, RetryCancel!)
ELSEIF ll_fila = 0 THEN
	Messagebox("Error","No Existe Existencia para Datos seleccionados")
END IF					
end event

type pb_salir from w_para_informes`pb_salir within w_info_alertas_informes
integer x = 4768
integer y = 1212
integer taborder = 90
end type

type st_3 from statictext within w_info_alertas_informes
integer x = 2889
integer y = 516
integer width = 215
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "Planta"
boolean focusrectangle = false
end type

type st_1 from statictext within w_info_alertas_informes
integer x = 251
integer y = 440
integer width = 4425
integer height = 208
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16711680
alignment alignment = center!
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_6 from statictext within w_info_alertas_informes
integer x = 357
integer y = 516
integer width = 251
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "Cliente"
boolean focusrectangle = false
end type

type tab_1 from tab within w_info_alertas_informes
integer x = 251
integer y = 716
integer width = 4425
integer height = 1360
integer taborder = 100
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16777215
boolean raggedright = true
boolean focusonbuttondown = true
integer selectedtab = 1
tabpage_1 tabpage_1
tabpage_2 tabpage_2
end type

on tab_1.create
this.tabpage_1=create tabpage_1
this.tabpage_2=create tabpage_2
this.Control[]={this.tabpage_1,&
this.tabpage_2}
end on

on tab_1.destroy
destroy(this.tabpage_1)
destroy(this.tabpage_2)
end on

type tabpage_1 from userobject within tab_1
integer x = 18
integer y = 112
integer width = 4389
integer height = 1232
long backcolor = 16711680
string text = "Existencia"
long tabtextcolor = 33554432
long tabbackcolor = 12632256
long picturemaskcolor = 536870912
dw_existencia dw_existencia
end type

on tabpage_1.create
this.dw_existencia=create dw_existencia
this.Control[]={this.dw_existencia}
end on

on tabpage_1.destroy
destroy(this.dw_existencia)
end on

type dw_existencia from datawindow within tabpage_1
integer x = 27
integer y = 24
integer width = 4325
integer height = 1160
integer taborder = 30
string title = "none"
string dataobject = "dw_info_existencia_alerta"
boolean hscrollbar = true
boolean vscrollbar = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type tabpage_2 from userobject within tab_1
boolean visible = false
integer x = 18
integer y = 112
integer width = 4389
integer height = 1232
boolean enabled = false
long backcolor = 33543637
string text = "T° de Recepción a Loteo"
long tabtextcolor = 33554432
long tabbackcolor = 12632256
long picturemaskcolor = 536870912
end type

type uo_selcliente from uo_seleccion_clientesprod within w_info_alertas_informes
integer x = 599
integer y = 504
integer height = 92
integer taborder = 30
boolean bringtotop = true
end type

on uo_selcliente.destroy
call uo_seleccion_clientesprod::destroy
end on

type uo_selplantas from uo_seleccion_plantas within w_info_alertas_informes
integer x = 3122
integer y = 500
integer height = 96
integer taborder = 110
boolean bringtotop = true
end type

on uo_selplantas.destroy
call uo_seleccion_plantas::destroy
end on

