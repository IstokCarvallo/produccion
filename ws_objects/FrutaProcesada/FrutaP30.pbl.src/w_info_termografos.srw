$PBExportHeader$w_info_termografos.srw
forward
global type w_info_termografos from w_para_informes
end type
type st_1 from statictext within w_info_termografos
end type
type st_2 from statictext within w_info_termografos
end type
type em_desde from editmask within w_info_termografos
end type
type st_6 from statictext within w_info_termografos
end type
type st_7 from statictext within w_info_termografos
end type
type em_hasta from editmask within w_info_termografos
end type
type gb_3 from groupbox within w_info_termografos
end type
type st_4 from statictext within w_info_termografos
end type
type cbx_transito from checkbox within w_info_termografos
end type
type uo_selcliente from uo_seleccion_clientesprod within w_info_termografos
end type
type uo_selplanta from uo_seleccion_plantas within w_info_termografos
end type
end forward

global type w_info_termografos from w_para_informes
integer x = 14
integer y = 32
integer width = 2665
integer height = 1464
string title = "Termógrafos Por Rango De Fechas"
boolean minbox = false
boolean maxbox = false
boolean resizable = false
windowtype windowtype = response!
string icon = "F:\Desarrollo\Producción\FrutaProcesada\Producc.ico"
st_1 st_1
st_2 st_2
em_desde em_desde
st_6 st_6
st_7 st_7
em_hasta em_hasta
gb_3 gb_3
st_4 st_4
cbx_transito cbx_transito
uo_selcliente uo_selcliente
uo_selplanta uo_selplanta
end type
global w_info_termografos w_info_termografos

type variables
str_busqueda istr_busq
str_mant istr_mant

DataWindowChild	idwc_cliente, idwc_planta
String is_NomPlanta
end variables

on w_info_termografos.create
int iCurrent
call super::create
this.st_1=create st_1
this.st_2=create st_2
this.em_desde=create em_desde
this.st_6=create st_6
this.st_7=create st_7
this.em_hasta=create em_hasta
this.gb_3=create gb_3
this.st_4=create st_4
this.cbx_transito=create cbx_transito
this.uo_selcliente=create uo_selcliente
this.uo_selplanta=create uo_selplanta
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_1
this.Control[iCurrent+2]=this.st_2
this.Control[iCurrent+3]=this.em_desde
this.Control[iCurrent+4]=this.st_6
this.Control[iCurrent+5]=this.st_7
this.Control[iCurrent+6]=this.em_hasta
this.Control[iCurrent+7]=this.gb_3
this.Control[iCurrent+8]=this.st_4
this.Control[iCurrent+9]=this.cbx_transito
this.Control[iCurrent+10]=this.uo_selcliente
this.Control[iCurrent+11]=this.uo_selplanta
end on

on w_info_termografos.destroy
call super::destroy
destroy(this.st_1)
destroy(this.st_2)
destroy(this.em_desde)
destroy(this.st_6)
destroy(this.st_7)
destroy(this.em_hasta)
destroy(this.gb_3)
destroy(this.st_4)
destroy(this.cbx_transito)
destroy(this.uo_selcliente)
destroy(this.uo_selplanta)
end on

event open;Boolean lb_Cerrar

If IsNull(uo_SelCliente.Codigo) Then lb_Cerrar = True
If IsNull(uo_SelPlanta.Codigo) Then lb_Cerrar = True

If lb_Cerrar Then 
	Close(This)
Else
	uo_SelCliente.Seleccion(False, False)
	uo_SelPlanta.Seleccion(True, False)
	uo_SelCliente.Inicia(gi_CodExport)
	uo_SelPlanta.Filtra(1)
	
	em_Desde.Text				=	String(RelativeDate(Today(), -365))
	em_Hasta.Text				=	String(Today())
End If
end event

type pb_excel from w_para_informes`pb_excel within w_info_termografos
end type

type st_computador from w_para_informes`st_computador within w_info_termografos
end type

type st_usuario from w_para_informes`st_usuario within w_info_termografos
end type

type st_temporada from w_para_informes`st_temporada within w_info_termografos
end type

type p_logo from w_para_informes`p_logo within w_info_termografos
end type

type st_titulo from w_para_informes`st_titulo within w_info_termografos
integer width = 1847
string text = "Informe Termógrafos Despachados"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_termografos
string tag = "Imprimir Reporte"
integer x = 2245
integer y = 728
integer taborder = 150
end type

event pb_acepta::clicked;SetPointer(Arrow!)

Integer	fila, li_transito

istr_info.titulo	= 'INFORME DE TERMOGRAFOS DESPACHADOS'

OpenWithParm(vinf, istr_info)
vinf.dw_1.DataObject = "dw_info_rango_termografos"

IF cbx_transito.Checked THEN
	li_transito = 1
ELSE
	li_transito = 2
END IF	

vinf.dw_1.SetTransObject(sqlca)

fila	=	vinf.dw_1.Retrieve(uo_SelCliente.Codigo,uo_SelPlanta.Codigo,Date(em_Desde.Text),Date(em_Hasta.Text),li_transito)

IF fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
			   	"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
ELSE
	F_Membrete(vinf.dw_1)
	vinf.dw_1.Modify("fechas.text = '" + "Desde El :  " + f_fecha_texto(em_Desde.Text, 1) + &
						"   Hasta El :  " + f_fecha_texto(em_Hasta.Text, 1) + "'")
	IF li_transito = 1 THEN
		vinf.dw_1.Modify("t_transito.text = '" + 'Solo En Tránsito' + "'")
	ELSE
		vinf.dw_1.Modify("t_transito.text = '" + 'Todos los Termógrafos' + "'")
	END IF	
	
	IF gs_Ambiente <> 'Windows' THEN F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
	vinf.Visible	= True
	vinf.Enabled	= True
END IF
SetPointer(Arrow!)				
end event

type pb_salir from w_para_informes`pb_salir within w_info_termografos
string tag = "Salir [Cerrar Ventana Activa]"
integer x = 2245
integer y = 1016
integer taborder = 170
string powertiptext = "Salir [Cerrar Ventana Activa]"
end type

type st_1 from statictext within w_info_termografos
integer x = 343
integer y = 696
integer width = 462
integer height = 76
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
boolean enabled = false
string text = "Planta"
boolean focusrectangle = false
end type

type st_2 from statictext within w_info_termografos
integer x = 357
integer y = 908
integer width = 425
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Fecha Inicio"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type em_desde from editmask within w_info_termografos
integer x = 786
integer y = 892
integer width = 416
integer height = 96
integer taborder = 130
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
long backcolor = 16777215
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
end type

event modified;istr_mant.argumento[3]	=	This.Text
end event

type st_6 from statictext within w_info_termografos
integer x = 343
integer y = 508
integer width = 233
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Cliente"
boolean focusrectangle = false
end type

type st_7 from statictext within w_info_termografos
integer x = 1234
integer y = 908
integer width = 297
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Término"
boolean focusrectangle = false
end type

type em_hasta from editmask within w_info_termografos
integer x = 1568
integer y = 892
integer width = 416
integer height = 96
integer taborder = 140
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
long backcolor = 16777215
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
end type

event modified;istr_mant.argumento[4]	=	This.Text
end event

type gb_3 from groupbox within w_info_termografos
integer x = 279
integer y = 792
integer width = 1783
integer height = 252
integer taborder = 140
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Despacho"
end type

type st_4 from statictext within w_info_termografos
integer x = 247
integer y = 412
integer width = 1847
integer height = 780
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16711680
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type cbx_transito from checkbox within w_info_termografos
integer x = 782
integer y = 1080
integer width = 718
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Solo en Tránsito"
boolean checked = true
end type

type uo_selcliente from uo_seleccion_clientesprod within w_info_termografos
integer x = 786
integer y = 496
integer height = 92
integer taborder = 20
boolean bringtotop = true
end type

on uo_selcliente.destroy
call uo_seleccion_clientesprod::destroy
end on

type uo_selplanta from uo_seleccion_plantas within w_info_termografos
event destroy ( )
integer x = 786
integer y = 600
integer taborder = 20
boolean bringtotop = true
end type

on uo_selplanta.destroy
call uo_seleccion_plantas::destroy
end on

