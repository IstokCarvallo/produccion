$PBExportHeader$w_info_cajas_embaladas.srw
forward
global type w_info_cajas_embaladas from w_para_informes
end type
type st_1 from statictext within w_info_cajas_embaladas
end type
type st_2 from statictext within w_info_cajas_embaladas
end type
type uo_selembalaje from uo_seleccion_embalajeprod within w_info_cajas_embaladas
end type
type uo_selcliente from uo_seleccion_clientesprod within w_info_cajas_embaladas
end type
type uo_selplanta from uo_seleccion_plantas within w_info_cajas_embaladas
end type
type st_3 from statictext within w_info_cajas_embaladas
end type
type cbx_folio from checkbox within w_info_cajas_embaladas
end type
type em_desde from editmask within w_info_cajas_embaladas
end type
type em_hasta from editmask within w_info_cajas_embaladas
end type
type st_4 from statictext within w_info_cajas_embaladas
end type
type st_5 from statictext within w_info_cajas_embaladas
end type
type rb_diario from radiobutton within w_info_cajas_embaladas
end type
type rb_semanal from radiobutton within w_info_cajas_embaladas
end type
type rb_mensual from radiobutton within w_info_cajas_embaladas
end type
type uo_selentidades from uo_seleccion_entidades within w_info_cajas_embaladas
end type
type st_6 from statictext within w_info_cajas_embaladas
end type
type gb_3 from groupbox within w_info_cajas_embaladas
end type
type gb_4 from groupbox within w_info_cajas_embaladas
end type
type gb_5 from groupbox within w_info_cajas_embaladas
end type
end forward

global type w_info_cajas_embaladas from w_para_informes
integer x = 14
integer y = 32
integer width = 2153
integer height = 2000
string title = "CONTROL CAJAS EMBALADAS"
boolean minbox = false
boolean maxbox = false
boolean resizable = false
windowtype windowtype = response!
string icon = "\Desarrollo\Productiva\FrutaGranelPackingTerceros\ProdFGranelPP.ico"
st_1 st_1
st_2 st_2
uo_selembalaje uo_selembalaje
uo_selcliente uo_selcliente
uo_selplanta uo_selplanta
st_3 st_3
cbx_folio cbx_folio
em_desde em_desde
em_hasta em_hasta
st_4 st_4
st_5 st_5
rb_diario rb_diario
rb_semanal rb_semanal
rb_mensual rb_mensual
uo_selentidades uo_selentidades
st_6 st_6
gb_3 gb_3
gb_4 gb_4
gb_5 gb_5
end type
global w_info_cajas_embaladas w_info_cajas_embaladas

type variables

end variables

on w_info_cajas_embaladas.create
int iCurrent
call super::create
this.st_1=create st_1
this.st_2=create st_2
this.uo_selembalaje=create uo_selembalaje
this.uo_selcliente=create uo_selcliente
this.uo_selplanta=create uo_selplanta
this.st_3=create st_3
this.cbx_folio=create cbx_folio
this.em_desde=create em_desde
this.em_hasta=create em_hasta
this.st_4=create st_4
this.st_5=create st_5
this.rb_diario=create rb_diario
this.rb_semanal=create rb_semanal
this.rb_mensual=create rb_mensual
this.uo_selentidades=create uo_selentidades
this.st_6=create st_6
this.gb_3=create gb_3
this.gb_4=create gb_4
this.gb_5=create gb_5
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_1
this.Control[iCurrent+2]=this.st_2
this.Control[iCurrent+3]=this.uo_selembalaje
this.Control[iCurrent+4]=this.uo_selcliente
this.Control[iCurrent+5]=this.uo_selplanta
this.Control[iCurrent+6]=this.st_3
this.Control[iCurrent+7]=this.cbx_folio
this.Control[iCurrent+8]=this.em_desde
this.Control[iCurrent+9]=this.em_hasta
this.Control[iCurrent+10]=this.st_4
this.Control[iCurrent+11]=this.st_5
this.Control[iCurrent+12]=this.rb_diario
this.Control[iCurrent+13]=this.rb_semanal
this.Control[iCurrent+14]=this.rb_mensual
this.Control[iCurrent+15]=this.uo_selentidades
this.Control[iCurrent+16]=this.st_6
this.Control[iCurrent+17]=this.gb_3
this.Control[iCurrent+18]=this.gb_4
this.Control[iCurrent+19]=this.gb_5
end on

on w_info_cajas_embaladas.destroy
call super::destroy
destroy(this.st_1)
destroy(this.st_2)
destroy(this.uo_selembalaje)
destroy(this.uo_selcliente)
destroy(this.uo_selplanta)
destroy(this.st_3)
destroy(this.cbx_folio)
destroy(this.em_desde)
destroy(this.em_hasta)
destroy(this.st_4)
destroy(this.st_5)
destroy(this.rb_diario)
destroy(this.rb_semanal)
destroy(this.rb_mensual)
destroy(this.uo_selentidades)
destroy(this.st_6)
destroy(this.gb_3)
destroy(this.gb_4)
destroy(this.gb_5)
end on

event open;call super::open;Boolean	lb_Cerrar

IF IsNull(uo_SelCliente.Codigo) THEN lb_Cerrar	=	True
IF IsNull(uo_SelEmbalaje.Codigo) THEN lb_Cerrar	=	True
IF IsNull(uo_SelPlanta.Codigo) THEN lb_Cerrar	=	True
IF IsNull(uo_SelEntidades.Codigo) THEN lb_Cerrar	=	True

IF lb_Cerrar THEN
	Close(This)
ELSE
	uo_SelCliente.Seleccion(True,True)
	uo_SelEmbalaje.Seleccion(True,True)
	uo_SelPlanta.Seleccion(True,True)
	uo_SelEntidades.Seleccion(True,True)

	em_desde.Text	=	String (today(), 'dd/mm/yyyy')
	em_hasta.Text	=	String (today(), 'dd/mm/yyyy')
End If
end event

type pb_excel from w_para_informes`pb_excel within w_info_cajas_embaladas
end type

type st_computador from w_para_informes`st_computador within w_info_cajas_embaladas
end type

type st_usuario from w_para_informes`st_usuario within w_info_cajas_embaladas
end type

type st_temporada from w_para_informes`st_temporada within w_info_cajas_embaladas
end type

type p_logo from w_para_informes`p_logo within w_info_cajas_embaladas
end type

type st_titulo from w_para_informes`st_titulo within w_info_cajas_embaladas
integer width = 1454
string text = "Informe Control Cajas Embaladas"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_cajas_embaladas
string tag = "Imprimir Reporte"
integer x = 1806
integer y = 452
integer taborder = 100
long backcolor = 553648127
end type

event pb_acepta::clicked;SetPointer(HourGlass!)

Long		Fila
Date		ld_desde, ld_hasta
Integer	li_Tipo, li_Folio

istr_info.titulo	= "INFORME CAJAS EMBALADAS"
istr_info.copias	= 1

ld_desde	=	Date(em_desde.Text)
ld_hasta	=	Date(em_hasta.Text)

If cbx_folio.Checked Then
	li_Folio = -9
Else
	li_Folio = -1
End If

If rb_Diario.Checked Then
	li_Tipo = 1
ElseIf rb_Semanal.Checked Then
	li_Tipo = 2
ElseIf rb_Mensual.Checked Then
	li_Tipo = 3
End If

OpenWithParm(vinf, istr_info)
vinf.dw_1.DataObject = "dw_info_cajasembadetalle"
vinf.dw_1.SetTransObject(sqlca)

fila = vinf.dw_1.Retrieve(uo_SelPlanta.Codigo, uo_SelCliente.codigo, uo_SelEmbalaje.Codigo, uo_SelEntidades.Codigo, ld_desde, ld_hasta, li_Folio, li_tipo)

IF fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
ELSE
	F_Membrete(vinf.dw_1)
	IF gs_Ambiente <> 'Windows' THEN F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
END IF

SetPointer(Arrow!)
end event

type pb_salir from w_para_informes`pb_salir within w_info_cajas_embaladas
string tag = "Salir [Cerrar Ventana Activa]"
integer x = 1801
integer y = 712
integer taborder = 110
string disabledname = "\desarrollo\bmp\exitd.bmp"
string powertiptext = "Salir [Cerrar Ventana Activa]"
long backcolor = 553648127
end type

type st_1 from statictext within w_info_cajas_embaladas
integer x = 315
integer y = 736
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
string text = "Planta"
boolean focusrectangle = false
end type

type st_2 from statictext within w_info_cajas_embaladas
integer x = 1010
integer y = 1356
integer width = 187
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
string text = "Hasta"
boolean focusrectangle = false
end type

type uo_selembalaje from uo_seleccion_embalajeprod within w_info_cajas_embaladas
event destroy ( )
integer x = 699
integer y = 824
integer taborder = 110
boolean bringtotop = true
end type

on uo_selembalaje.destroy
call uo_seleccion_embalajeprod::destroy
end on

type uo_selcliente from uo_seleccion_clientesprod within w_info_cajas_embaladas
event destroy ( )
integer x = 699
integer y = 456
integer taborder = 110
boolean bringtotop = true
end type

on uo_selcliente.destroy
call uo_seleccion_clientesprod::destroy
end on

event ue_cambio;call super::ue_cambio;If IsNull(This.codigo)  Then Return

Choose Case This.codigo
	Case -1, -9
		
	Case This.codigo
		uo_selembalaje.Filtra(This.codigo)
		uo_SelEmbalaje.LimpiaDatos()
		
End Choose
end event

type uo_selplanta from uo_seleccion_plantas within w_info_cajas_embaladas
integer x = 699
integer y = 640
integer taborder = 120
boolean bringtotop = true
end type

on uo_selplanta.destroy
call uo_seleccion_plantas::destroy
end on

type st_3 from statictext within w_info_cajas_embaladas
integer x = 315
integer y = 552
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

type cbx_folio from checkbox within w_info_cajas_embaladas
integer x = 599
integer y = 1468
integer width = 777
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
string text = "Consolida Nro. Folio"
boolean checked = true
end type

type em_desde from editmask within w_info_cajas_embaladas
integer x = 498
integer y = 1340
integer width = 503
integer height = 88
integer taborder = 120
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
boolean dropdowncalendar = true
end type

type em_hasta from editmask within w_info_cajas_embaladas
integer x = 1189
integer y = 1340
integer width = 503
integer height = 88
integer taborder = 130
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
boolean dropdowncalendar = true
end type

type st_4 from statictext within w_info_cajas_embaladas
integer x = 311
integer y = 1100
integer width = 288
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
string text = "Personal"
boolean focusrectangle = false
end type

type st_5 from statictext within w_info_cajas_embaladas
integer x = 311
integer y = 1356
integer width = 206
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
string text = "Desde"
boolean focusrectangle = false
end type

type rb_diario from radiobutton within w_info_cajas_embaladas
integer x = 293
integer y = 1748
integer width = 402
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
string text = "Diario"
boolean checked = true
end type

event clicked;em_desde.Text	=	String (today(), 'dd/mm/yyyy')
em_hasta.Text	=	String (today(), 'dd/mm/yyyy')
end event

type rb_semanal from radiobutton within w_info_cajas_embaladas
integer x = 777
integer y = 1744
integer width = 402
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
string text = "Semanal"
end type

event clicked;em_desde.Text	=	String (today(), 'dd/mm/yyyy')
em_hasta.Text	=	String (today(), 'dd/mm/yyyy')
end event

type rb_mensual from radiobutton within w_info_cajas_embaladas
integer x = 1257
integer y = 1748
integer width = 402
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
string text = "Mensual"
end type

event clicked;em_desde.Text	=	'01/' + String (Today(), 'mm/yyyy')
em_hasta.Text	=	String(RelativeDate(Date('01/' + String(RelativeDate(Today(), 31), 'mm/yyyy')), -1), 'dd/mm/yyyy')
end event

type uo_selentidades from uo_seleccion_entidades within w_info_cajas_embaladas
integer x = 699
integer y = 1008
integer taborder = 120
boolean bringtotop = true
long backcolor = 553648127
end type

on uo_selentidades.destroy
call uo_seleccion_entidades::destroy
end on

type st_6 from statictext within w_info_cajas_embaladas
integer x = 311
integer y = 912
integer width = 288
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
string text = "Embalaje"
boolean focusrectangle = false
end type

type gb_3 from groupbox within w_info_cajas_embaladas
integer x = 251
integer y = 392
integer width = 1449
integer height = 832
integer taborder = 40
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 16711680
end type

type gb_4 from groupbox within w_info_cajas_embaladas
integer x = 251
integer y = 1220
integer width = 1449
integer height = 400
integer taborder = 120
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 16711680
end type

type gb_5 from groupbox within w_info_cajas_embaladas
integer x = 251
integer y = 1636
integer width = 1449
integer height = 248
integer taborder = 130
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 16711680
string text = " Tipo de Informe "
end type

