$PBExportHeader$w_recep_pallet_pallet_temperatura.srw
$PBExportComments$Muestra los pallet, con tipo de entrasa desde Packing,  que no han sido asimilados a ninún encabezado de recepción.
forward
global type w_recep_pallet_pallet_temperatura from w_para_informes
end type
type st_2 from statictext within w_recep_pallet_pallet_temperatura
end type
type st_3 from statictext within w_recep_pallet_pallet_temperatura
end type
type em_desde from editmask within w_recep_pallet_pallet_temperatura
end type
type em_hasta from editmask within w_recep_pallet_pallet_temperatura
end type
type gb_3 from groupbox within w_recep_pallet_pallet_temperatura
end type
type st_1 from statictext within w_recep_pallet_pallet_temperatura
end type
type st_4 from statictext within w_recep_pallet_pallet_temperatura
end type
type st_5 from statictext within w_recep_pallet_pallet_temperatura
end type
type uo_selcliente from uo_seleccion_clientesprod within w_recep_pallet_pallet_temperatura
end type
type uo_selplantas from uo_seleccion_plantas within w_recep_pallet_pallet_temperatura
end type
end forward

global type w_recep_pallet_pallet_temperatura from w_para_informes
integer width = 2866
integer height = 1488
string title = "Recepciones de Pallets con Temperaturas"
st_2 st_2
st_3 st_3
em_desde em_desde
em_hasta em_hasta
gb_3 gb_3
st_1 st_1
st_4 st_4
st_5 st_5
uo_selcliente uo_selcliente
uo_selplantas uo_selplantas
end type
global w_recep_pallet_pallet_temperatura w_recep_pallet_pallet_temperatura

type variables

end variables

on w_recep_pallet_pallet_temperatura.create
int iCurrent
call super::create
this.st_2=create st_2
this.st_3=create st_3
this.em_desde=create em_desde
this.em_hasta=create em_hasta
this.gb_3=create gb_3
this.st_1=create st_1
this.st_4=create st_4
this.st_5=create st_5
this.uo_selcliente=create uo_selcliente
this.uo_selplantas=create uo_selplantas
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_2
this.Control[iCurrent+2]=this.st_3
this.Control[iCurrent+3]=this.em_desde
this.Control[iCurrent+4]=this.em_hasta
this.Control[iCurrent+5]=this.gb_3
this.Control[iCurrent+6]=this.st_1
this.Control[iCurrent+7]=this.st_4
this.Control[iCurrent+8]=this.st_5
this.Control[iCurrent+9]=this.uo_selcliente
this.Control[iCurrent+10]=this.uo_selplantas
end on

on w_recep_pallet_pallet_temperatura.destroy
call super::destroy
destroy(this.st_2)
destroy(this.st_3)
destroy(this.em_desde)
destroy(this.em_hasta)
destroy(this.gb_3)
destroy(this.st_1)
destroy(this.st_4)
destroy(this.st_5)
destroy(this.uo_selcliente)
destroy(this.uo_selplantas)
end on

event open;call super::open;Boolean	lb_Cerrar 

If IsNull(uo_SelCliente.Codigo) Then lb_Cerrar = True
If IsNull(uo_SelPlantas.Codigo) Then lb_Cerrar = True

If lb_Cerrar Then
	Close(This)
Else
	uo_SelCliente.Seleccion(False, False)
	uo_SelPlantas.Seleccion(True, False)
	uo_SelCliente.Inicia(gi_CodExport)
	
	em_desde.Text = String(RelativeDate(Today(), -365))
	em_hasta.Text = String(Today())
End If
end event

type pb_excel from w_para_informes`pb_excel within w_recep_pallet_pallet_temperatura
end type

type st_computador from w_para_informes`st_computador within w_recep_pallet_pallet_temperatura
end type

type st_usuario from w_para_informes`st_usuario within w_recep_pallet_pallet_temperatura
end type

type st_temporada from w_para_informes`st_temporada within w_recep_pallet_pallet_temperatura
end type

type p_logo from w_para_informes`p_logo within w_recep_pallet_pallet_temperatura
string picturename = "\Desarrollo 17\Imagenes\Logos\RBlanco.jpg"
end type

type st_titulo from w_para_informes`st_titulo within w_recep_pallet_pallet_temperatura
string text = "Informe de Temperaturas con Recepciones"
end type

type pb_acepta from w_para_informes`pb_acepta within w_recep_pallet_pallet_temperatura
integer x = 2459
integer y = 448
integer taborder = 40
end type

event pb_acepta::clicked;SetPointer(Arrow!)

Integer	fila
Date		ld_desde, ld_hasta

istr_info.titulo	= 'INFORME DE TEMPERATURAS CON RECEPCIONES'
OpenWithParm(vinf, istr_info)
vinf.dw_1.DataObject = 'dw_informe_recepciones_temperaturas'

vinf.dw_1.SetTransObject(sqlca)
ld_desde = Date(em_desde.text)
ld_hasta = Date(em_hasta.text)

fila	=	vinf.dw_1.Retrieve(uo_SelCliente.Codigo, uo_SelPlantas.Codigo,ld_desde,ld_hasta)

If fila = -1 Then
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
			   	"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)

ElseIf fila = 0 Then
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
Else
	F_Membrete(vinf.dw_1)
	If gs_Ambiente <> 'Windows' Then F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
End If

SetPointer(Arrow!)
end event

type pb_salir from w_para_informes`pb_salir within w_recep_pallet_pallet_temperatura
integer x = 2459
integer y = 744
integer taborder = 50
end type

type st_2 from statictext within w_recep_pallet_pallet_temperatura
integer x = 389
integer y = 524
integer width = 215
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

type st_3 from statictext within w_recep_pallet_pallet_temperatura
integer x = 384
integer y = 740
integer width = 197
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
string text = "Planta"
boolean focusrectangle = false
end type

type em_desde from editmask within w_recep_pallet_pallet_temperatura
integer x = 713
integer y = 952
integer width = 526
integer height = 96
integer taborder = 60
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long backcolor = 16777215
string text = "none"
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
boolean dropdowncalendar = true
end type

type em_hasta from editmask within w_recep_pallet_pallet_temperatura
integer x = 1563
integer y = 952
integer width = 526
integer height = 96
integer taborder = 70
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long backcolor = 16777215
string text = "none"
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
boolean dropdowncalendar = true
end type

type gb_3 from groupbox within w_recep_pallet_pallet_temperatura
integer x = 361
integer y = 844
integer width = 1783
integer height = 248
integer taborder = 80
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Fecha Recepción"
end type

type st_1 from statictext within w_recep_pallet_pallet_temperatura
integer x = 251
integer y = 444
integer width = 2034
integer height = 708
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

type st_4 from statictext within w_recep_pallet_pallet_temperatura
integer x = 471
integer y = 964
integer width = 224
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
string text = "Desde"
boolean focusrectangle = false
end type

type st_5 from statictext within w_recep_pallet_pallet_temperatura
integer x = 1335
integer y = 964
integer width = 210
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
string text = "Hasta"
boolean focusrectangle = false
end type

type uo_selcliente from uo_seleccion_clientesprod within w_recep_pallet_pallet_temperatura
event destroy ( )
integer x = 663
integer y = 512
integer height = 92
integer taborder = 20
boolean bringtotop = true
end type

on uo_selcliente.destroy
call uo_seleccion_clientesprod::destroy
end on

type uo_selplantas from uo_seleccion_plantas within w_recep_pallet_pallet_temperatura
event destroy ( )
integer x = 663
integer y = 640
integer taborder = 30
boolean bringtotop = true
end type

on uo_selplantas.destroy
call uo_seleccion_plantas::destroy
end on

