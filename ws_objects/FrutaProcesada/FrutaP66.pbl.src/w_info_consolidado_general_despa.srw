$PBExportHeader$w_info_consolidado_general_despa.srw
$PBExportComments$Informe Consolidado General de Embarques.
forward
global type w_info_consolidado_general_despa from w_para_informes
end type
type st_1 from statictext within w_info_consolidado_general_despa
end type
type st_2 from statictext within w_info_consolidado_general_despa
end type
type st_3 from statictext within w_info_consolidado_general_despa
end type
type st_8 from statictext within w_info_consolidado_general_despa
end type
type st_9 from statictext within w_info_consolidado_general_despa
end type
type st_10 from statictext within w_info_consolidado_general_despa
end type
type st_11 from statictext within w_info_consolidado_general_despa
end type
type dw_tiposalida from datawindow within w_info_consolidado_general_despa
end type
type st_plantadestino from statictext within w_info_consolidado_general_despa
end type
type st_22 from statictext within w_info_consolidado_general_despa
end type
type em_desde from editmask within w_info_consolidado_general_despa
end type
type st_23 from statictext within w_info_consolidado_general_despa
end type
type em_hasta from editmask within w_info_consolidado_general_despa
end type
type st_21 from statictext within w_info_consolidado_general_despa
end type
type st_5 from statictext within w_info_consolidado_general_despa
end type
type dw_transp from datawindow within w_info_consolidado_general_despa
end type
type cbx_trans from checkbox within w_info_consolidado_general_despa
end type
type st_4 from statictext within w_info_consolidado_general_despa
end type
type st_6 from statictext within w_info_consolidado_general_despa
end type
type st_7 from statictext within w_info_consolidado_general_despa
end type
type dw_status from datawindow within w_info_consolidado_general_despa
end type
type cbx_status from checkbox within w_info_consolidado_general_despa
end type
type cbx_constatus from checkbox within w_info_consolidado_general_despa
end type
type uo_selespecie from uo_seleccion_especie within w_info_consolidado_general_despa
end type
type cbx_varirotula from checkbox within w_info_consolidado_general_despa
end type
type st_12 from statictext within w_info_consolidado_general_despa
end type
type uo_selproductor from uo_seleccion_varios_productores_clientes within w_info_consolidado_general_despa
end type
type cbx_excel from checkbox within w_info_consolidado_general_despa
end type
type dw_1 from datawindow within w_info_consolidado_general_despa
end type
type uo_selcliente from uo_seleccion_clientesprod within w_info_consolidado_general_despa
end type
type uo_selplanta from uo_seleccion_plantas within w_info_consolidado_general_despa
end type
type uo_selplantadestino from uo_seleccion_plantas within w_info_consolidado_general_despa
end type
type uo_seltipocamion from uo_seleccion_tipocamion within w_info_consolidado_general_despa
end type
end forward

global type w_info_consolidado_general_despa from w_para_informes
integer width = 3470
integer height = 2208
st_1 st_1
st_2 st_2
st_3 st_3
st_8 st_8
st_9 st_9
st_10 st_10
st_11 st_11
dw_tiposalida dw_tiposalida
st_plantadestino st_plantadestino
st_22 st_22
em_desde em_desde
st_23 st_23
em_hasta em_hasta
st_21 st_21
st_5 st_5
dw_transp dw_transp
cbx_trans cbx_trans
st_4 st_4
st_6 st_6
st_7 st_7
dw_status dw_status
cbx_status cbx_status
cbx_constatus cbx_constatus
uo_selespecie uo_selespecie
cbx_varirotula cbx_varirotula
st_12 st_12
uo_selproductor uo_selproductor
cbx_excel cbx_excel
dw_1 dw_1
uo_selcliente uo_selcliente
uo_selplanta uo_selplanta
uo_selplantadestino uo_selplantadestino
uo_seltipocamion uo_seltipocamion
end type
global w_info_consolidado_general_despa w_info_consolidado_general_despa

type variables
str_mant istr_mant

DataWindowChild 	idwc_transp, idwc_status, idwc_tiposalida

String		is_NomEmbarque

uo_transportista	iuo_transportista
uo_status			iuo_status

end variables

on w_info_consolidado_general_despa.create
int iCurrent
call super::create
this.st_1=create st_1
this.st_2=create st_2
this.st_3=create st_3
this.st_8=create st_8
this.st_9=create st_9
this.st_10=create st_10
this.st_11=create st_11
this.dw_tiposalida=create dw_tiposalida
this.st_plantadestino=create st_plantadestino
this.st_22=create st_22
this.em_desde=create em_desde
this.st_23=create st_23
this.em_hasta=create em_hasta
this.st_21=create st_21
this.st_5=create st_5
this.dw_transp=create dw_transp
this.cbx_trans=create cbx_trans
this.st_4=create st_4
this.st_6=create st_6
this.st_7=create st_7
this.dw_status=create dw_status
this.cbx_status=create cbx_status
this.cbx_constatus=create cbx_constatus
this.uo_selespecie=create uo_selespecie
this.cbx_varirotula=create cbx_varirotula
this.st_12=create st_12
this.uo_selproductor=create uo_selproductor
this.cbx_excel=create cbx_excel
this.dw_1=create dw_1
this.uo_selcliente=create uo_selcliente
this.uo_selplanta=create uo_selplanta
this.uo_selplantadestino=create uo_selplantadestino
this.uo_seltipocamion=create uo_seltipocamion
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_1
this.Control[iCurrent+2]=this.st_2
this.Control[iCurrent+3]=this.st_3
this.Control[iCurrent+4]=this.st_8
this.Control[iCurrent+5]=this.st_9
this.Control[iCurrent+6]=this.st_10
this.Control[iCurrent+7]=this.st_11
this.Control[iCurrent+8]=this.dw_tiposalida
this.Control[iCurrent+9]=this.st_plantadestino
this.Control[iCurrent+10]=this.st_22
this.Control[iCurrent+11]=this.em_desde
this.Control[iCurrent+12]=this.st_23
this.Control[iCurrent+13]=this.em_hasta
this.Control[iCurrent+14]=this.st_21
this.Control[iCurrent+15]=this.st_5
this.Control[iCurrent+16]=this.dw_transp
this.Control[iCurrent+17]=this.cbx_trans
this.Control[iCurrent+18]=this.st_4
this.Control[iCurrent+19]=this.st_6
this.Control[iCurrent+20]=this.st_7
this.Control[iCurrent+21]=this.dw_status
this.Control[iCurrent+22]=this.cbx_status
this.Control[iCurrent+23]=this.cbx_constatus
this.Control[iCurrent+24]=this.uo_selespecie
this.Control[iCurrent+25]=this.cbx_varirotula
this.Control[iCurrent+26]=this.st_12
this.Control[iCurrent+27]=this.uo_selproductor
this.Control[iCurrent+28]=this.cbx_excel
this.Control[iCurrent+29]=this.dw_1
this.Control[iCurrent+30]=this.uo_selcliente
this.Control[iCurrent+31]=this.uo_selplanta
this.Control[iCurrent+32]=this.uo_selplantadestino
this.Control[iCurrent+33]=this.uo_seltipocamion
end on

on w_info_consolidado_general_despa.destroy
call super::destroy
destroy(this.st_1)
destroy(this.st_2)
destroy(this.st_3)
destroy(this.st_8)
destroy(this.st_9)
destroy(this.st_10)
destroy(this.st_11)
destroy(this.dw_tiposalida)
destroy(this.st_plantadestino)
destroy(this.st_22)
destroy(this.em_desde)
destroy(this.st_23)
destroy(this.em_hasta)
destroy(this.st_21)
destroy(this.st_5)
destroy(this.dw_transp)
destroy(this.cbx_trans)
destroy(this.st_4)
destroy(this.st_6)
destroy(this.st_7)
destroy(this.dw_status)
destroy(this.cbx_status)
destroy(this.cbx_constatus)
destroy(this.uo_selespecie)
destroy(this.cbx_varirotula)
destroy(this.st_12)
destroy(this.uo_selproductor)
destroy(this.cbx_excel)
destroy(this.dw_1)
destroy(this.uo_selcliente)
destroy(this.uo_selplanta)
destroy(this.uo_selplantadestino)
destroy(this.uo_seltipocamion)
end on

event open;call super::open;String	ls_Columna[]
Boolean	lb_Cerrar

IF IsNull(uo_SelEspecie.Codigo) THEN lb_Cerrar = True
IF IsNull(uo_SelCliente.Codigo) THEN lb_Cerrar = True
IF IsNull(uo_SelProductor.Codigo) THEN lb_Cerrar = True
IF IsNull(uo_SelPlanta.Codigo) THEN lb_Cerrar = True
IF IsNull(uo_SelPlantaDestino.Codigo) THEN lb_Cerrar = True
IF IsNull(uo_SelTipoCamion.Codigo) THEN lb_Cerrar = True

IF lb_Cerrar THEN
	Close(This)
ELSE
	uo_SelEspecie.Seleccion(True,True)
	uo_SelCliente.Seleccion(False, False)
	uo_SelProductor.Seleccion(True, True)
	uo_SelPlanta.Seleccion(True, False)
	uo_SelPlantaDestino.Seleccion(False, False)
	uo_SelTipoCamion.Seleccion(True, False)
	
	uo_SelCliente.Inicia(gi_CodExport)
	
	iuo_transportista	=	CREATE	uo_transportista	
	iuo_status			=	CREATE	uo_status
	
	dw_tiposalida.GetChild("defe_tiposa", idwc_tiposalida)
	idwc_tiposalida.SetTransObject(SQLCA)
	idwc_tiposalida.Retrieve()
	dw_tiposalida.InsertRow(0)
	
	dw_transp.GetChild("tran_codigo", idwc_transp)
	idwc_transp.SetTransObject(SQLCA)
	idwc_transp.Retrieve()
	dw_transp.InsertRow(0)
		
	dw_status.GetChild("stat_codigo",idwc_status)
	idwc_status.SetTransObject(SQLCA)
	idwc_status.Retrieve()
	dw_status.InsertRow(0)
	
	em_desde.Text		=	String(RelativeDate(Today(), -365))
	em_hasta.Text		=	String(Today())
	
	istr_mant.argumento[7] 		= 	String(Today())			// Fecha
	istr_mant.argumento[10] 	= 	'-1'							// Transportista
	
	istr_mant.argumento[20]	=	"11"
	istr_mant.argumento[21]	=	"-9"
	is_NomEmbarque				=	"Todos"
END IF

end event

type pb_excel from w_para_informes`pb_excel within w_info_consolidado_general_despa
integer x = 2962
integer y = 648
end type

type st_computador from w_para_informes`st_computador within w_info_consolidado_general_despa
end type

type st_usuario from w_para_informes`st_usuario within w_info_consolidado_general_despa
end type

type st_temporada from w_para_informes`st_temporada within w_info_consolidado_general_despa
end type

type p_logo from w_para_informes`p_logo within w_info_consolidado_general_despa
end type

type st_titulo from w_para_informes`st_titulo within w_info_consolidado_general_despa
integer x = 247
integer width = 2720
string text = "Informe Consolidado Despachos Especiales"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_consolidado_general_despa
integer x = 3063
integer y = 1396
integer taborder = 110
end type

event pb_acepta::clicked;Long	  	ll_Fila, li_varirotula = 0, ll_cont
String		texto_fecha,ls_transpor, ls_nomtipo,  ls_Archivo

SetPointer(HourGlass!)

istr_info.titulo	= 'INFORME DESPACHOS GENERALES ESPECIALES'

OpenWithParm(vinf, istr_info)
vinf.dw_1.DataObject = "dw_info_consolidado_general_despastatus"
vinf.dw_1.SetTransObject(SQLCA)

If cbx_varirotula.Checked Then li_varirotula = 1

texto_fecha	=	"Desde El : " + f_fecha_texto(String(em_Desde.Text), 1) + " Hasta El : " + f_fecha_texto(String(em_Hasta.Text), 1)

If cbx_trans.Checked  Then
	ls_transpor = 'Todos'
Else
	ls_transpor = iuo_transportista.nombre
End If

ll_Fila	=	vinf.dw_1.Retrieve(uo_SelCliente.Codigo, Integer(istr_mant.argumento[20]), uo_SelEspecie.Codigo, uo_SelPlanta.Codigo, uo_SelPlantaDestino.Codigo, &
										 Date(em_Desde.Text), Date(em_Hasta.Text), Integer(istr_mant.argumento[10]), &
										 uo_SelTipoCamion.Codigo,Integer(istr_mant.argumento[21]),&
										 li_varirotula, uo_SelProductor.Lista)

If ll_Fila = -1 Then
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ElseIf ll_Fila = 0 Then
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
Else	
	Choose Case is_NomEmbarque
		Case "2"
			ls_nomtipo = 'Reproceso'
		Case "3"
			ls_nomtipo = 'Reembalaje'
		Case "5"
			ls_nomtipo = 'Reproceso Serv. 3º'
		Case "6"
			ls_nomtipo = 'Reembalaje Serv. 3º'
		Case "7"
			ls_nomtipo = 'Embarque Maritimo'
		Case "8"
			ls_nomtipo = 'Embarque Aereo'
		Case "9"
			ls_nomtipo = 'Embarque Terrestre'
		Case "10"
			ls_nomtipo = 'Devolución al Productor'
		Case "11"
			ls_nomtipo = 'Traspaso Inter-Planta'
		Case "12"
			ls_nomtipo = 'M/I Cta. Propia'
		Case "13"
			ls_nomtipo = 'M/I Cta. Productor'
		Case "14"
			ls_nomtipo = 'Muestra Ensayo'
		Case "15"
			ls_nomtipo = 'Botadero'	
		Case "16"
			ls_nomtipo = 'Ventas a Terceros'
		Case "17"
			ls_nomtipo = 'Desp. Servicios 3º'
		Case "20"
			ls_nomtipo = 'Packing Externo'
		Case "21"
			ls_nomtipo = 'Venta Exp. País'	
		Case "31"
			ls_nomtipo = 'Despacho por Cajas'	
	End Choose
		
	F_Membrete(vinf.dw_1)
	
	vinf.dw_1.ModIfy("planta.text = '" + uo_SelPlanta.Nombre + "'")
	vinf.dw_1.ModIfy("especie.text = '" + uo_SelEspecie.Nombre + "'")
	vinf.dw_1.ModIfy("tiposa.text = '" + ls_nomtipo + "'")
	vinf.dw_1.ModIfy("plantadestino.text = '" + uo_SelPlantaDestino.Nombre + "'")
	vinf.dw_1.ModIfy("fechas.text = '" + Texto_Fecha + "'")
	vinf.dw_1.ModIfy("transportista.text = '" + ls_transpor+ "'")
	vinf.dw_1.ModIfy("tipocamion.text = '" + uo_SelTipoCamion.Nombre + "'")
	
	If gs_Ambiente <> 'Windows' Then F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
	
	If cbx_excel.Checked Then	
		ls_Archivo	=	"\despachosespeciales.xls"
//		RegistryGet( "HKEY_CURRENT_USER\Software\Microsoft\Windows\CurrentVersion\Explorer\Shell Folders", "Personal", RegString!, ls_Ruta)
//		GetFolder ( "selecccione carpeta", ls_Ruta)
										 
		dw_1.SetTransObject(sqlca)
		ll_cont = dw_1.Retrieve(uo_SelCliente.Codigo, Integer(istr_mant.argumento[20]), uo_SelEspecie.Codigo, &
										 uo_SelPlanta.Codigo, uo_SelPlantaDestino.Codigo,  &
										 Date(em_Desde.Text), Date(em_Hasta.Text), Integer(istr_mant.argumento[10]), &
										 uo_SelTipoCamion.Codigo,Integer(istr_mant.argumento[21]),&
										 li_varirotula, uo_SelProductor.Lista)	
		If ll_cont > 0 Then
			dw_1.SaveAs(/*ls_Ruta + ls_Archivo*/'' ,Excel5!, True)	
		End If
	End If
End If
end event

type pb_salir from w_para_informes`pb_salir within w_info_consolidado_general_despa
integer x = 3063
integer y = 1748
integer taborder = 130
end type

type st_1 from statictext within w_info_consolidado_general_despa
integer x = 247
integer y = 440
integer width = 2720
integer height = 680
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 16711680
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_2 from statictext within w_info_consolidado_general_despa
integer x = 370
integer y = 508
integer width = 270
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

type st_3 from statictext within w_info_consolidado_general_despa
integer x = 370
integer y = 724
integer width = 270
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
string text = "Especie"
boolean focusrectangle = false
end type

type st_8 from statictext within w_info_consolidado_general_despa
integer x = 375
integer y = 936
integer width = 270
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

type st_9 from statictext within w_info_consolidado_general_despa
integer x = 247
integer y = 1420
integer width = 2720
integer height = 284
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 16711680
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_10 from statictext within w_info_consolidado_general_despa
integer x = 315
integer y = 1460
integer width = 347
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
string text = "Tipo Salida"
boolean focusrectangle = false
end type

type st_11 from statictext within w_info_consolidado_general_despa
integer x = 247
integer y = 1120
integer width = 2720
integer height = 296
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 16711680
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type dw_tiposalida from datawindow within w_info_consolidado_general_despa
integer x = 1042
integer y = 1456
integer width = 1079
integer height = 92
integer taborder = 140
boolean bringtotop = true
string title = "none"
string dataobject = "ddlbdw_tiposalida"
boolean border = false
boolean livescroll = true
end type

event itemchanged;If Not IsNull(data) And Integer(data) <> 0  Then
	
	is_NomEmbarque	=	String(data)
	istr_mant.argumento[20]	=	data

	If Integer(istr_mant.argumento[20]) <> 11 Then
		uo_SelPlantaDestino.Visible		=	False
		uo_SelPlantaDestino.Enabled	=	False
		st_plantadestino.visible			=	False
	Else
		uo_SelPlantaDestino.visible		=	True
		uo_SelPlantaDestino.Enabled 	=	True
		uo_SelPlantaDestino.SetFocus()
		st_plantadestino.visible			=	True
	End If
Else
	Return 1
End If



end event

type st_plantadestino from statictext within w_info_consolidado_general_despa
integer x = 311
integer y = 1580
integer width = 434
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
string text = "Planta Destino"
boolean focusrectangle = false
end type

type st_22 from statictext within w_info_consolidado_general_despa
integer x = 750
integer y = 1892
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

type em_desde from editmask within w_info_consolidado_general_despa
integer x = 1179
integer y = 1876
integer width = 393
integer height = 96
integer taborder = 80
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

type st_23 from statictext within w_info_consolidado_general_despa
integer x = 1627
integer y = 1892
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

type em_hasta from editmask within w_info_consolidado_general_despa
integer x = 1966
integer y = 1876
integer width = 393
integer height = 96
integer taborder = 100
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

type st_21 from statictext within w_info_consolidado_general_despa
integer x = 247
integer y = 1856
integer width = 2720
integer height = 148
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 16711680
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_5 from statictext within w_info_consolidado_general_despa
integer x = 261
integer y = 1264
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
string text = "Transportista"
boolean focusrectangle = false
end type

type dw_transp from datawindow within w_info_consolidado_general_despa
integer x = 686
integer y = 1256
integer width = 896
integer height = 92
integer taborder = 170
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_transportista"
boolean border = false
boolean livescroll = true
end type

event itemchanged;Integer li_nula
SetNull(li_nula)

IF iuo_transportista.existe(Integer(data),True,sqlca) THEN
	istr_mant.argumento[10] = data
ELSE
	This.SetItem(1, "tran_codigo", li_nula)
	RETURN 1
END IF
end event

event itemerror;RETURN 1
end event

type cbx_trans from checkbox within w_info_consolidado_general_despa
integer x = 704
integer y = 1148
integer width = 256
integer height = 72
integer taborder = 150
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Todos"
boolean checked = true
end type

event clicked;IF This.Checked THEN
	istr_mant.argumento[10]	=	'-1'
	dw_transp.Enabled	=	False
	dw_transp.Reset()
	dw_transp.InsertRow(0)
ELSE
	dw_transp.Enabled	=	True
	dw_transp.SetFocus()
END IF
end event

type st_4 from statictext within w_info_consolidado_general_despa
integer x = 1641
integer y = 1264
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
string text = "Tipo Camión"
boolean focusrectangle = false
end type

type st_6 from statictext within w_info_consolidado_general_despa
integer x = 247
integer y = 1708
integer width = 2720
integer height = 148
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 16711680
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_7 from statictext within w_info_consolidado_general_despa
integer x = 311
integer y = 1748
integer width = 402
integer height = 72
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Status"
boolean focusrectangle = false
end type

type dw_status from datawindow within w_info_consolidado_general_despa
integer x = 741
integer y = 1732
integer width = 1010
integer height = 108
integer taborder = 70
boolean bringtotop = true
boolean enabled = false
string title = "none"
string dataobject = "dddw_status1"
boolean border = false
boolean livescroll = true
end type

event itemchanged;Integer li_nula
SetNull(li_nula)

If iuo_status.existe(Integer(data),True,sqlca) Then
	istr_mant.argumento[21] = data
Else
	This.SetItem(1, "stat_codigo", li_nula)
	Return 1
End If
end event

event itemerror;RETURN 1
end event

type cbx_status from checkbox within w_info_consolidado_general_despa
integer x = 1755
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
string text = "Todos"
boolean checked = true
end type

event clicked;IF This.Checked THEN
	dw_status.Enabled		=	False
	cbx_constatus.Enabled=	False
	istr_mant.argumento[21]	=	'-1'
ELSE
	dw_status.Enabled		=	True
	dw_status.Reset()
	dw_status.InsertRow(0)
	dw_status.SetFocus()
	cbx_constatus.Enabled=	True
END IF	

end event

type cbx_constatus from checkbox within w_info_consolidado_general_despa
integer x = 2085
integer y = 1744
integer width = 443
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
string text = "Consolidado"
boolean checked = true
end type

event clicked;IF This.checked=True THEN
	dw_status.Enabled=False
	istr_mant.argumento[21]	=	'-9'
ELSE
	istr_mant.argumento[21]	=	'-1'
	dw_status.Enabled=True
	dw_status.Reset()
	dw_status.InsertRow(0)
	dw_status.SetFocus()
END IF
	
end event

type uo_selespecie from uo_seleccion_especie within w_info_consolidado_general_despa
event destroy ( )
integer x = 640
integer y = 628
integer height = 180
integer taborder = 90
boolean bringtotop = true
end type

on uo_selespecie.destroy
call uo_seleccion_especie::destroy
end on

type cbx_varirotula from checkbox within w_info_consolidado_general_despa
integer x = 2016
integer y = 592
integer width = 873
integer height = 84
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
boolean lefttext = true
end type

type st_12 from statictext within w_info_consolidado_general_despa
integer x = 1659
integer y = 936
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
string text = "Productor"
boolean focusrectangle = false
end type

type uo_selproductor from uo_seleccion_varios_productores_clientes within w_info_consolidado_general_despa
integer x = 2016
integer y = 760
integer taborder = 120
boolean bringtotop = true
end type

on uo_selproductor.destroy
call uo_seleccion_varios_productores_clientes::destroy
end on

event ue_cambio;call super::ue_cambio;uo_selproductor.Filtra(-1,-1, uo_SelCliente.Codigo)
end event

type cbx_excel from checkbox within w_info_consolidado_general_despa
integer x = 2016
integer y = 484
integer width = 873
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
string text = "Archivo Excel"
boolean lefttext = true
end type

type dw_1 from datawindow within w_info_consolidado_general_despa
boolean visible = false
integer x = 3013
integer y = 344
integer width = 261
integer height = 208
integer taborder = 130
boolean bringtotop = true
string title = "none"
string dataobject = "dw_archivodespacho_especiales"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type uo_selcliente from uo_seleccion_clientesprod within w_info_consolidado_general_despa
event destroy ( )
integer x = 635
integer y = 496
integer height = 88
integer taborder = 130
boolean bringtotop = true
end type

on uo_selcliente.destroy
call uo_seleccion_clientesprod::destroy
end on

type uo_selplanta from uo_seleccion_plantas within w_info_consolidado_general_despa
event destroy ( )
integer x = 635
integer y = 852
integer taborder = 130
boolean bringtotop = true
end type

on uo_selplanta.destroy
call uo_seleccion_plantas::destroy
end on

type uo_selplantadestino from uo_seleccion_plantas within w_info_consolidado_general_despa
event destroy ( )
integer x = 1042
integer y = 1568
integer height = 88
integer taborder = 140
boolean bringtotop = true
end type

on uo_selplantadestino.destroy
call uo_seleccion_plantas::destroy
end on

type uo_seltipocamion from uo_seleccion_tipocamion within w_info_consolidado_general_despa
event destroy ( )
integer x = 2016
integer y = 1172
integer taborder = 170
boolean bringtotop = true
end type

on uo_seltipocamion.destroy
call uo_seleccion_tipocamion::destroy
end on

