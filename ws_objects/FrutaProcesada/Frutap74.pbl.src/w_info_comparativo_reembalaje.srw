$PBExportHeader$w_info_comparativo_reembalaje.srw
forward
global type w_info_comparativo_reembalaje from w_para_informes
end type
type st_4 from statictext within w_info_comparativo_reembalaje
end type
type st_1 from statictext within w_info_comparativo_reembalaje
end type
type st_2 from statictext within w_info_comparativo_reembalaje
end type
type em_desde from editmask within w_info_comparativo_reembalaje
end type
type st_6 from statictext within w_info_comparativo_reembalaje
end type
type st_7 from statictext within w_info_comparativo_reembalaje
end type
type em_hasta from editmask within w_info_comparativo_reembalaje
end type
type st_8 from statictext within w_info_comparativo_reembalaje
end type
type st_15 from statictext within w_info_comparativo_reembalaje
end type
type cbx_varirotula from checkbox within w_info_comparativo_reembalaje
end type
type st_5 from statictext within w_info_comparativo_reembalaje
end type
type cbx_prodrotula from checkbox within w_info_comparativo_reembalaje
end type
type cbx_calrotula from checkbox within w_info_comparativo_reembalaje
end type
type uo_selcliente from uo_seleccion_clientesprod within w_info_comparativo_reembalaje
end type
type uo_selplanta from uo_seleccion_plantas within w_info_comparativo_reembalaje
end type
type uo_selproductor from uo_seleccion_productor within w_info_comparativo_reembalaje
end type
end forward

global type w_info_comparativo_reembalaje from w_para_informes
integer x = 14
integer y = 32
integer width = 2560
integer height = 1992
string title = "INFORME COMPARATIVO REEMBALAJE"
boolean minbox = false
boolean maxbox = false
boolean resizable = false
windowtype windowtype = response!
string icon = "F:\Desarrollo\Producción\FrutaProcesada\Producc.ico"
st_4 st_4
st_1 st_1
st_2 st_2
em_desde em_desde
st_6 st_6
st_7 st_7
em_hasta em_hasta
st_8 st_8
st_15 st_15
cbx_varirotula cbx_varirotula
st_5 st_5
cbx_prodrotula cbx_prodrotula
cbx_calrotula cbx_calrotula
uo_selcliente uo_selcliente
uo_selplanta uo_selplanta
uo_selproductor uo_selproductor
end type
global w_info_comparativo_reembalaje w_info_comparativo_reembalaje

type variables

end variables

on w_info_comparativo_reembalaje.create
int iCurrent
call super::create
this.st_4=create st_4
this.st_1=create st_1
this.st_2=create st_2
this.em_desde=create em_desde
this.st_6=create st_6
this.st_7=create st_7
this.em_hasta=create em_hasta
this.st_8=create st_8
this.st_15=create st_15
this.cbx_varirotula=create cbx_varirotula
this.st_5=create st_5
this.cbx_prodrotula=create cbx_prodrotula
this.cbx_calrotula=create cbx_calrotula
this.uo_selcliente=create uo_selcliente
this.uo_selplanta=create uo_selplanta
this.uo_selproductor=create uo_selproductor
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_4
this.Control[iCurrent+2]=this.st_1
this.Control[iCurrent+3]=this.st_2
this.Control[iCurrent+4]=this.em_desde
this.Control[iCurrent+5]=this.st_6
this.Control[iCurrent+6]=this.st_7
this.Control[iCurrent+7]=this.em_hasta
this.Control[iCurrent+8]=this.st_8
this.Control[iCurrent+9]=this.st_15
this.Control[iCurrent+10]=this.cbx_varirotula
this.Control[iCurrent+11]=this.st_5
this.Control[iCurrent+12]=this.cbx_prodrotula
this.Control[iCurrent+13]=this.cbx_calrotula
this.Control[iCurrent+14]=this.uo_selcliente
this.Control[iCurrent+15]=this.uo_selplanta
this.Control[iCurrent+16]=this.uo_selproductor
end on

on w_info_comparativo_reembalaje.destroy
call super::destroy
destroy(this.st_4)
destroy(this.st_1)
destroy(this.st_2)
destroy(this.em_desde)
destroy(this.st_6)
destroy(this.st_7)
destroy(this.em_hasta)
destroy(this.st_8)
destroy(this.st_15)
destroy(this.cbx_varirotula)
destroy(this.st_5)
destroy(this.cbx_prodrotula)
destroy(this.cbx_calrotula)
destroy(this.uo_selcliente)
destroy(this.uo_selplanta)
destroy(this.uo_selproductor)
end on

event open;call super::open;Boolean lb_Cerrar

If IsNull(uo_SelCliente.Codigo) Then lb_Cerrar = True
If IsNull(uo_SelPlanta.Codigo) Then lb_Cerrar = True
If IsNull(uo_SelProductor.Codigo) Then lb_Cerrar = True

If lb_Cerrar Then 
	Close(This)
Else
	uo_SelCliente.Seleccion(False, False)
	uo_SelPlanta.Seleccion(True, False)
	uo_SelProductor.Seleccion(True, False)
	uo_SelPlanta.Filtra(1)
	uo_SelProductor.Filtra(-1)
	uo_SelCliente.Inicia(gi_CodExport)
		
	IF gi_vari_rotulada = 1 THEN
		cbx_varirotula.Checked	= True
		cbx_varirotula.Enabled	= False
	ELSE
		cbx_varirotula.Checked	= False
		cbx_varirotula.Enabled	= True
	END IF	
	
	IF gi_prod_rotulado = 1 THEN
		cbx_prodrotula.Checked	= True
		cbx_prodrotula.Enabled	= False
	ELSE
		cbx_prodrotula.Checked	= False
		cbx_prodrotula.Enabled	= True
	END IF	
	
	IF gi_cali_rotulado = 1 THEN
		cbx_calrotula.Checked	= True
		cbx_calrotula.Enabled	= False
	ELSE
		cbx_calrotula.Checked	= False
		cbx_calrotula.Enabled	= True
	END IF	
	
	em_desde.Text				=	String(RelativeDate(Today(), -365))
	em_hasta.Text				=	String(Today())
End If
end event

event resize;//
end event

type pb_excel from w_para_informes`pb_excel within w_info_comparativo_reembalaje
end type

type st_computador from w_para_informes`st_computador within w_info_comparativo_reembalaje
end type

type st_usuario from w_para_informes`st_usuario within w_info_comparativo_reembalaje
end type

type st_temporada from w_para_informes`st_temporada within w_info_comparativo_reembalaje
end type

type p_logo from w_para_informes`p_logo within w_info_comparativo_reembalaje
end type

type st_titulo from w_para_informes`st_titulo within w_info_comparativo_reembalaje
integer width = 1719
string text = "Informe Reembalaje"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_comparativo_reembalaje
integer x = 2085
integer y = 1096
integer taborder = 150
end type

event pb_acepta::clicked;SetPointer(Arrow!)

Integer 	fila, li_varirotula, li_calrotula, li_prodrotula

istr_info.titulo	= 'INFORME COMPARATIVO REEMBALAJE'
OpenWithParm(vinf, istr_info)
vinf.dw_1.DataObject = "dw_info_repa_reembalaje"

IF cbx_varirotula.Checked THEN
	li_varirotula = 1
ELSE
	li_varirotula = 0
END IF

IF cbx_prodrotula.Checked THEN
	li_prodrotula = 1
ELSE
	li_prodrotula = 0
END IF

IF cbx_calrotula.Checked THEN
	li_calrotula = 1
ELSE
	li_calrotula = 0
END IF	

vinf.dw_1.SetTransObject(sqlca)

fila	=	vinf.dw_1.Retrieve(uo_SelCliente.Codigo, uo_SelPlanta.Codigo, uo_SelProductor.Codigo, &
				Date(em_Desde.Text),Date(em_Hasta.Text),li_prodrotula,li_varirotula,li_calrotula)

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

type pb_salir from w_para_informes`pb_salir within w_info_comparativo_reembalaje
integer x = 2089
integer y = 1384
integer taborder = 170
end type

type st_4 from statictext within w_info_comparativo_reembalaje
integer x = 247
integer y = 440
integer width = 1719
integer height = 612
boolean bringtotop = true
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

type st_1 from statictext within w_info_comparativo_reembalaje
integer x = 311
integer y = 688
integer width = 293
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

type st_2 from statictext within w_info_comparativo_reembalaje
integer x = 283
integer y = 1516
integer width = 384
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
string text = "Desde Desp."
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type em_desde from editmask within w_info_comparativo_reembalaje
integer x = 672
integer y = 1500
integer width = 485
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
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
boolean dropdowncalendar = true
end type

type st_6 from statictext within w_info_comparativo_reembalaje
integer x = 311
integer y = 488
integer width = 306
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

type st_7 from statictext within w_info_comparativo_reembalaje
integer x = 1184
integer y = 1516
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
string text = "Hasta"
boolean focusrectangle = false
end type

type em_hasta from editmask within w_info_comparativo_reembalaje
integer x = 1408
integer y = 1500
integer width = 485
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
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
boolean dropdowncalendar = true
end type

type st_8 from statictext within w_info_comparativo_reembalaje
integer x = 311
integer y = 896
integer width = 306
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
string text = "Productor"
boolean focusrectangle = false
end type

type st_15 from statictext within w_info_comparativo_reembalaje
integer x = 247
integer y = 1424
integer width = 1719
integer height = 264
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

type cbx_varirotula from checkbox within w_info_comparativo_reembalaje
integer x = 731
integer y = 1092
integer width = 626
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
string text = "Variedad Rotulada"
end type

type st_5 from statictext within w_info_comparativo_reembalaje
integer x = 247
integer y = 1052
integer width = 1719
integer height = 368
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

type cbx_prodrotula from checkbox within w_info_comparativo_reembalaje
integer x = 731
integer y = 1200
integer width = 645
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
string text = "Productor Rotulado"
end type

type cbx_calrotula from checkbox within w_info_comparativo_reembalaje
integer x = 731
integer y = 1308
integer width = 626
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
string text = "Calibre Rotulado"
end type

type uo_selcliente from uo_seleccion_clientesprod within w_info_comparativo_reembalaje
integer x = 722
integer y = 472
integer height = 96
integer taborder = 20
boolean bringtotop = true
end type

on uo_selcliente.destroy
call uo_seleccion_clientesprod::destroy
end on

type uo_selplanta from uo_seleccion_plantas within w_info_comparativo_reembalaje
integer x = 722
integer y = 600
integer taborder = 50
boolean bringtotop = true
end type

on uo_selplanta.destroy
call uo_seleccion_plantas::destroy
end on

type uo_selproductor from uo_seleccion_productor within w_info_comparativo_reembalaje
integer x = 722
integer y = 800
integer taborder = 20
boolean bringtotop = true
end type

on uo_selproductor.destroy
call uo_seleccion_productor::destroy
end on

