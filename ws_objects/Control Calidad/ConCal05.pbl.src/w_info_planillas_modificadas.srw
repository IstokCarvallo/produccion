$PBExportHeader$w_info_planillas_modificadas.srw
forward
global type w_info_planillas_modificadas from w_para_informes
end type
type st_1 from statictext within w_info_planillas_modificadas
end type
type st_6 from statictext within w_info_planillas_modificadas
end type
type dw_pesoneto from datawindow within w_info_planillas_modificadas
end type
type tit_peso from statictext within w_info_planillas_modificadas
end type
type gb_3 from groupbox within w_info_planillas_modificadas
end type
type cbx_1 from checkbox within w_info_planillas_modificadas
end type
type st_4 from statictext within w_info_planillas_modificadas
end type
type uo_selcliente from uo_seleccion_clientesprod within w_info_planillas_modificadas
end type
type uo_selplanta from uo_seleccion_plantas within w_info_planillas_modificadas
end type
end forward

global type w_info_planillas_modificadas from w_para_informes
integer x = 14
integer y = 32
integer width = 2528
integer height = 1196
string title = "Pallets Reclasificados"
boolean minbox = false
boolean maxbox = false
boolean resizable = false
windowtype windowtype = response!
string icon = "F:\Desarrollo\Productiva\FrutaProcesada\FrutaPro.ico"
st_1 st_1
st_6 st_6
dw_pesoneto dw_pesoneto
tit_peso tit_peso
gb_3 gb_3
cbx_1 cbx_1
st_4 st_4
uo_selcliente uo_selcliente
uo_selplanta uo_selplanta
end type
global w_info_planillas_modificadas w_info_planillas_modificadas

type variables

end variables

on w_info_planillas_modificadas.create
int iCurrent
call super::create
this.st_1=create st_1
this.st_6=create st_6
this.dw_pesoneto=create dw_pesoneto
this.tit_peso=create tit_peso
this.gb_3=create gb_3
this.cbx_1=create cbx_1
this.st_4=create st_4
this.uo_selcliente=create uo_selcliente
this.uo_selplanta=create uo_selplanta
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_1
this.Control[iCurrent+2]=this.st_6
this.Control[iCurrent+3]=this.dw_pesoneto
this.Control[iCurrent+4]=this.tit_peso
this.Control[iCurrent+5]=this.gb_3
this.Control[iCurrent+6]=this.cbx_1
this.Control[iCurrent+7]=this.st_4
this.Control[iCurrent+8]=this.uo_selcliente
this.Control[iCurrent+9]=this.uo_selplanta
end on

on w_info_planillas_modificadas.destroy
call super::destroy
destroy(this.st_1)
destroy(this.st_6)
destroy(this.dw_pesoneto)
destroy(this.tit_peso)
destroy(this.gb_3)
destroy(this.cbx_1)
destroy(this.st_4)
destroy(this.uo_selcliente)
destroy(this.uo_selplanta)
end on

event open;call super::open;Boolean	lb_Cerrar

IF IsNull(uo_SelCliente.Codigo) THEN lb_Cerrar	=	True
IF IsNull(uo_SelPlanta.Codigo) THEN lb_Cerrar	=	True

IF lb_Cerrar THEN
	Close(This)
ELSE
	uo_SelCliente.Seleccion(False, False)
	uo_SelPlanta.Seleccion(False, False)

	uo_SelCliente.Inicia(gi_CodExport)
	uo_SelPlanta.Inicia(gi_CodPlanta)
END IF






end event

type pb_excel from w_para_informes`pb_excel within w_info_planillas_modificadas
end type

type st_computador from w_para_informes`st_computador within w_info_planillas_modificadas
integer x = 1595
end type

type st_usuario from w_para_informes`st_usuario within w_info_planillas_modificadas
integer x = 1595
end type

type st_temporada from w_para_informes`st_temporada within w_info_planillas_modificadas
integer x = 1595
end type

type p_logo from w_para_informes`p_logo within w_info_planillas_modificadas
end type

type st_titulo from w_para_informes`st_titulo within w_info_planillas_modificadas
integer width = 1847
string text = "Listado de Planillas Modificadas"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_planillas_modificadas
string tag = "Imprimir Reporte"
integer x = 2213
integer y = 460
integer taborder = 170
end type

event pb_acepta::clicked;SetPointer(HourGlass!)

Long    ll_fila

istr_info.titulo	= 'INFORME PLANILLAS ELIMINADAS'
OpenWithParm(vinf,istr_info)
vinf.dw_1.DataObject = "dw_info_planillasmodificadas"
vinf.dw_1.SetTransObject(sqlca)
ll_fila = vinf.dw_1.Retrieve(uo_SelCliente.Codigo, uo_SelPlanta.Codigo)

IF ll_fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF ll_fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
ELSE
	F_Membrete(vinf.dw_1)
	IF gs_Ambiente <> 'Windows' THEN F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
END IF

SetPointer(Arrow!)

end event

type pb_salir from w_para_informes`pb_salir within w_info_planillas_modificadas
string tag = "Salir [Cerrar Ventana Activa]"
integer x = 2213
integer y = 748
integer taborder = 180
string powertiptext = "Salir [Cerrar Ventana Activa]"
end type

type st_1 from statictext within w_info_planillas_modificadas
integer x = 334
integer y = 728
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

type st_6 from statictext within w_info_planillas_modificadas
integer x = 334
integer y = 516
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

type dw_pesoneto from datawindow within w_info_planillas_modificadas
boolean visible = false
integer x = 759
integer y = 1728
integer width = 544
integer height = 84
integer taborder = 20
boolean bringtotop = true
string dataobject = "dddw_pesoneto"
boolean border = false
boolean livescroll = true
end type

event itemerror;RETURN 1
end event

type tit_peso from statictext within w_info_planillas_modificadas
boolean visible = false
integer x = 567
integer y = 1744
integer width = 183
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Peso"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type gb_3 from groupbox within w_info_planillas_modificadas
boolean visible = false
integer x = 174
integer y = 1412
integer width = 1614
integer height = 280
integer taborder = 190
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
end type

type cbx_1 from checkbox within w_info_planillas_modificadas
boolean visible = false
integer x = 617
integer y = 1468
integer width = 631
integer height = 80
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Cajas Equivalentes"
end type

event clicked;IF This.Checked THEN
	tit_peso.Enabled		=	True
	dw_pesoneto.Enabled	=	True
	dw_pesoneto.Modify("enva_pesone.BackGround.Color = " + String(rgb(255,255,255)))

ELSE
	tit_peso.Enabled		=	False
	dw_pesoneto.Enabled	=	False
	dw_pesoneto.Modify("enva_pesone.BackGround.Color = " + String(rgb(192,192,192)))
END IF

end event

type st_4 from statictext within w_info_planillas_modificadas
integer x = 238
integer y = 416
integer width = 1847
integer height = 512
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long backcolor = 16711680
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type uo_selcliente from uo_seleccion_clientesprod within w_info_planillas_modificadas
integer x = 722
integer y = 500
integer height = 100
integer taborder = 180
boolean bringtotop = true
end type

on uo_selcliente.destroy
call uo_seleccion_clientesprod::destroy
end on

event ue_cambio;call super::ue_cambio;//IF IsNull(This.Codigo) THEN RETURN
//
//uo_selplanta.Filtra(This.Codigo)
//		
//uo_selplanta.cbx_Todos.Enabled	=	True
//
end event

type uo_selplanta from uo_seleccion_plantas within w_info_planillas_modificadas
integer x = 731
integer y = 716
integer height = 100
integer taborder = 190
boolean bringtotop = true
end type

on uo_selplanta.destroy
call uo_seleccion_plantas::destroy
end on

