$PBExportHeader$w_info_traslados_camiones.srw
forward
global type w_info_traslados_camiones from w_para_informes
end type
type st_4 from statictext within w_info_traslados_camiones
end type
type st_1 from statictext within w_info_traslados_camiones
end type
type st_2 from statictext within w_info_traslados_camiones
end type
type em_desde from editmask within w_info_traslados_camiones
end type
type st_6 from statictext within w_info_traslados_camiones
end type
type st_7 from statictext within w_info_traslados_camiones
end type
type em_hasta from editmask within w_info_traslados_camiones
end type
type dw_pesoneto from datawindow within w_info_traslados_camiones
end type
type tit_peso from statictext within w_info_traslados_camiones
end type
type st_15 from statictext within w_info_traslados_camiones
end type
type st_16 from statictext within w_info_traslados_camiones
end type
type st_10 from statictext within w_info_traslados_camiones
end type
type cbx_operacion from checkbox within w_info_traslados_camiones
end type
type dw_operaciones from datawindow within w_info_traslados_camiones
end type
type st_14 from statictext within w_info_traslados_camiones
end type
type cbx_embarque from checkbox within w_info_traslados_camiones
end type
type dw_embarques from datawindow within w_info_traslados_camiones
end type
type cbx_1 from checkbox within w_info_traslados_camiones
end type
type cbx_operacioncons from checkbox within w_info_traslados_camiones
end type
type st_3 from statictext within w_info_traslados_camiones
end type
type st_5 from statictext within w_info_traslados_camiones
end type
type cbx_solotermografos from checkbox within w_info_traslados_camiones
end type
type st_8 from statictext within w_info_traslados_camiones
end type
type uo_selespecie from uo_seleccion_especie within w_info_traslados_camiones
end type
type uo_selvariedad from uo_seleccion_variedad within w_info_traslados_camiones
end type
type st_12 from statictext within w_info_traslados_camiones
end type
type st_13 from statictext within w_info_traslados_camiones
end type
type st_17 from statictext within w_info_traslados_camiones
end type
type st_18 from statictext within w_info_traslados_camiones
end type
type st_19 from statictext within w_info_traslados_camiones
end type
type dw_puertos from datawindow within w_info_traslados_camiones
end type
type cbx_puertos from checkbox within w_info_traslados_camiones
end type
type cbx_puertoscons from checkbox within w_info_traslados_camiones
end type
type st_22 from statictext within w_info_traslados_camiones
end type
type cbx_4 from checkbox within w_info_traslados_camiones
end type
type dw_tiposalida from datawindow within w_info_traslados_camiones
end type
type st_20 from statictext within w_info_traslados_camiones
end type
type st_calidad from statictext within w_info_traslados_camiones
end type
type em_calidad from editmask within w_info_traslados_camiones
end type
type cbx_calidad from checkbox within w_info_traslados_camiones
end type
type st_21 from statictext within w_info_traslados_camiones
end type
type em_norden from editmask within w_info_traslados_camiones
end type
type cbx_5 from checkbox within w_info_traslados_camiones
end type
type cbx_6 from checkbox within w_info_traslados_camiones
end type
type st_11 from statictext within w_info_traslados_camiones
end type
type uo_selplanta from uo_seleccion_plantas within w_info_traslados_camiones
end type
type st_23 from statictext within w_info_traslados_camiones
end type
type uo_selzona from uo_seleccion_zonas within w_info_traslados_camiones
end type
type st_24 from statictext within w_info_traslados_camiones
end type
type uo_selproductor from uo_seleccion_varios_productores_clientes within w_info_traslados_camiones
end type
type uo_seldestino from uo_seleccion_destinos within w_info_traslados_camiones
end type
type st_25 from statictext within w_info_traslados_camiones
end type
type uo_selcliente from uo_seleccion_clientesprod within w_info_traslados_camiones
end type
type uo_selplantaorigen from uo_seleccion_plantas within w_info_traslados_camiones
end type
type uo_seltransportista from uo_seleccion_transportista within w_info_traslados_camiones
end type
type uo_seltipocamion from uo_seleccion_tipocamion within w_info_traslados_camiones
end type
end forward

global type w_info_traslados_camiones from w_para_informes
integer x = 14
integer y = 32
integer width = 4233
integer height = 2560
string title = "Listado de Traslados"
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
dw_pesoneto dw_pesoneto
tit_peso tit_peso
st_15 st_15
st_16 st_16
st_10 st_10
cbx_operacion cbx_operacion
dw_operaciones dw_operaciones
st_14 st_14
cbx_embarque cbx_embarque
dw_embarques dw_embarques
cbx_1 cbx_1
cbx_operacioncons cbx_operacioncons
st_3 st_3
st_5 st_5
cbx_solotermografos cbx_solotermografos
st_8 st_8
uo_selespecie uo_selespecie
uo_selvariedad uo_selvariedad
st_12 st_12
st_13 st_13
st_17 st_17
st_18 st_18
st_19 st_19
dw_puertos dw_puertos
cbx_puertos cbx_puertos
cbx_puertoscons cbx_puertoscons
st_22 st_22
cbx_4 cbx_4
dw_tiposalida dw_tiposalida
st_20 st_20
st_calidad st_calidad
em_calidad em_calidad
cbx_calidad cbx_calidad
st_21 st_21
em_norden em_norden
cbx_5 cbx_5
cbx_6 cbx_6
st_11 st_11
uo_selplanta uo_selplanta
st_23 st_23
uo_selzona uo_selzona
st_24 st_24
uo_selproductor uo_selproductor
uo_seldestino uo_seldestino
st_25 st_25
uo_selcliente uo_selcliente
uo_selplantaorigen uo_selplantaorigen
uo_seltransportista uo_seltransportista
uo_seltipocamion uo_seltipocamion
end type
global w_info_traslados_camiones w_info_traslados_camiones

type variables
str_busqueda istr_busq
str_mant istr_mant

DataWindowChild	idwc_tipopro, idwc_productor, idwc_pesoneto,&
						idwc_operaciones,idwc_embarque, idwc_tipocamion, idwc_puertos, idwc_tiposalida

String 	is_NomPlanta, is_Embarque, is_Operacion, is_NomEmbarque
Integer	ii_Operacion
Long		ll_norden

uo_calibre								iuo_calibre


end variables

on w_info_traslados_camiones.create
int iCurrent
call super::create
this.st_4=create st_4
this.st_1=create st_1
this.st_2=create st_2
this.em_desde=create em_desde
this.st_6=create st_6
this.st_7=create st_7
this.em_hasta=create em_hasta
this.dw_pesoneto=create dw_pesoneto
this.tit_peso=create tit_peso
this.st_15=create st_15
this.st_16=create st_16
this.st_10=create st_10
this.cbx_operacion=create cbx_operacion
this.dw_operaciones=create dw_operaciones
this.st_14=create st_14
this.cbx_embarque=create cbx_embarque
this.dw_embarques=create dw_embarques
this.cbx_1=create cbx_1
this.cbx_operacioncons=create cbx_operacioncons
this.st_3=create st_3
this.st_5=create st_5
this.cbx_solotermografos=create cbx_solotermografos
this.st_8=create st_8
this.uo_selespecie=create uo_selespecie
this.uo_selvariedad=create uo_selvariedad
this.st_12=create st_12
this.st_13=create st_13
this.st_17=create st_17
this.st_18=create st_18
this.st_19=create st_19
this.dw_puertos=create dw_puertos
this.cbx_puertos=create cbx_puertos
this.cbx_puertoscons=create cbx_puertoscons
this.st_22=create st_22
this.cbx_4=create cbx_4
this.dw_tiposalida=create dw_tiposalida
this.st_20=create st_20
this.st_calidad=create st_calidad
this.em_calidad=create em_calidad
this.cbx_calidad=create cbx_calidad
this.st_21=create st_21
this.em_norden=create em_norden
this.cbx_5=create cbx_5
this.cbx_6=create cbx_6
this.st_11=create st_11
this.uo_selplanta=create uo_selplanta
this.st_23=create st_23
this.uo_selzona=create uo_selzona
this.st_24=create st_24
this.uo_selproductor=create uo_selproductor
this.uo_seldestino=create uo_seldestino
this.st_25=create st_25
this.uo_selcliente=create uo_selcliente
this.uo_selplantaorigen=create uo_selplantaorigen
this.uo_seltransportista=create uo_seltransportista
this.uo_seltipocamion=create uo_seltipocamion
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_4
this.Control[iCurrent+2]=this.st_1
this.Control[iCurrent+3]=this.st_2
this.Control[iCurrent+4]=this.em_desde
this.Control[iCurrent+5]=this.st_6
this.Control[iCurrent+6]=this.st_7
this.Control[iCurrent+7]=this.em_hasta
this.Control[iCurrent+8]=this.dw_pesoneto
this.Control[iCurrent+9]=this.tit_peso
this.Control[iCurrent+10]=this.st_15
this.Control[iCurrent+11]=this.st_16
this.Control[iCurrent+12]=this.st_10
this.Control[iCurrent+13]=this.cbx_operacion
this.Control[iCurrent+14]=this.dw_operaciones
this.Control[iCurrent+15]=this.st_14
this.Control[iCurrent+16]=this.cbx_embarque
this.Control[iCurrent+17]=this.dw_embarques
this.Control[iCurrent+18]=this.cbx_1
this.Control[iCurrent+19]=this.cbx_operacioncons
this.Control[iCurrent+20]=this.st_3
this.Control[iCurrent+21]=this.st_5
this.Control[iCurrent+22]=this.cbx_solotermografos
this.Control[iCurrent+23]=this.st_8
this.Control[iCurrent+24]=this.uo_selespecie
this.Control[iCurrent+25]=this.uo_selvariedad
this.Control[iCurrent+26]=this.st_12
this.Control[iCurrent+27]=this.st_13
this.Control[iCurrent+28]=this.st_17
this.Control[iCurrent+29]=this.st_18
this.Control[iCurrent+30]=this.st_19
this.Control[iCurrent+31]=this.dw_puertos
this.Control[iCurrent+32]=this.cbx_puertos
this.Control[iCurrent+33]=this.cbx_puertoscons
this.Control[iCurrent+34]=this.st_22
this.Control[iCurrent+35]=this.cbx_4
this.Control[iCurrent+36]=this.dw_tiposalida
this.Control[iCurrent+37]=this.st_20
this.Control[iCurrent+38]=this.st_calidad
this.Control[iCurrent+39]=this.em_calidad
this.Control[iCurrent+40]=this.cbx_calidad
this.Control[iCurrent+41]=this.st_21
this.Control[iCurrent+42]=this.em_norden
this.Control[iCurrent+43]=this.cbx_5
this.Control[iCurrent+44]=this.cbx_6
this.Control[iCurrent+45]=this.st_11
this.Control[iCurrent+46]=this.uo_selplanta
this.Control[iCurrent+47]=this.st_23
this.Control[iCurrent+48]=this.uo_selzona
this.Control[iCurrent+49]=this.st_24
this.Control[iCurrent+50]=this.uo_selproductor
this.Control[iCurrent+51]=this.uo_seldestino
this.Control[iCurrent+52]=this.st_25
this.Control[iCurrent+53]=this.uo_selcliente
this.Control[iCurrent+54]=this.uo_selplantaorigen
this.Control[iCurrent+55]=this.uo_seltransportista
this.Control[iCurrent+56]=this.uo_seltipocamion
end on

on w_info_traslados_camiones.destroy
call super::destroy
destroy(this.st_4)
destroy(this.st_1)
destroy(this.st_2)
destroy(this.em_desde)
destroy(this.st_6)
destroy(this.st_7)
destroy(this.em_hasta)
destroy(this.dw_pesoneto)
destroy(this.tit_peso)
destroy(this.st_15)
destroy(this.st_16)
destroy(this.st_10)
destroy(this.cbx_operacion)
destroy(this.dw_operaciones)
destroy(this.st_14)
destroy(this.cbx_embarque)
destroy(this.dw_embarques)
destroy(this.cbx_1)
destroy(this.cbx_operacioncons)
destroy(this.st_3)
destroy(this.st_5)
destroy(this.cbx_solotermografos)
destroy(this.st_8)
destroy(this.uo_selespecie)
destroy(this.uo_selvariedad)
destroy(this.st_12)
destroy(this.st_13)
destroy(this.st_17)
destroy(this.st_18)
destroy(this.st_19)
destroy(this.dw_puertos)
destroy(this.cbx_puertos)
destroy(this.cbx_puertoscons)
destroy(this.st_22)
destroy(this.cbx_4)
destroy(this.dw_tiposalida)
destroy(this.st_20)
destroy(this.st_calidad)
destroy(this.em_calidad)
destroy(this.cbx_calidad)
destroy(this.st_21)
destroy(this.em_norden)
destroy(this.cbx_5)
destroy(this.cbx_6)
destroy(this.st_11)
destroy(this.uo_selplanta)
destroy(this.st_23)
destroy(this.uo_selzona)
destroy(this.st_24)
destroy(this.uo_selproductor)
destroy(this.uo_seldestino)
destroy(this.st_25)
destroy(this.uo_selcliente)
destroy(this.uo_selplantaorigen)
destroy(this.uo_seltransportista)
destroy(this.uo_seltipocamion)
end on

event open;call super::open;x	=	0
y	=	0

Boolean	lb_Cerrar

IF IsNull(uo_SelZona.Codigo) THEN lb_Cerrar = True
IF IsNull(uo_SelVariedad.Codigo) THEN lb_Cerrar = True
IF IsNull(uo_SelProductor.Codigo) THEN lb_Cerrar = True
IF IsNull(uo_SelEspecie.Codigo) THEN lb_Cerrar = True
IF IsNull(uo_SelDestino.Codigo) THEN lb_Cerrar = True
IF IsNull(uo_SelCliente.Codigo) THEN lb_Cerrar = True
IF IsNull(uo_SelPlantaOrigen.Codigo) THEN lb_Cerrar = True
IF IsNull(uo_SelPlanta.Codigo) THEN lb_Cerrar = True
IF IsNull(uo_SelTransportista.Codigo) THEN lb_Cerrar = True
IF IsNull(uo_SelTipoCamion.Codigo) THEN lb_Cerrar = True

IF lb_Cerrar THEN
	Close(This)
ELSE
	uo_SelEspecie.Seleccion(True,True)
	uo_SelDestino.Seleccion(True,True)
	uo_SelProductor.Seleccion(True,True)
	uo_SelZona.Seleccion(True,True)
	uo_SelVariedad.Seleccion(True,True)
	uo_SelPlanta.Seleccion(True, False)
	uo_SelCliente.Seleccion(True, False)
	uo_SelPlantaOrigen.Seleccion(True, True)
	uo_SelTransportista.Seleccion(True, False)
	uo_SelTipoCamion.Seleccion(True, False)
	
	iuo_calibre		=	Create uo_calibre
END IF

dw_embarques.GetChild("embq_codigo", idwc_embarque)
idwc_embarque.SetTransObject(SQLCA)
idwc_embarque.Retrieve(gi_CodExport, 0)
dw_embarques.InsertRow(0)

dw_operaciones.GetChild("oper_codigo", idwc_operaciones)
idwc_operaciones.SetTransObject(SQLCA)
idwc_operaciones.Retrieve(gi_CodExport)
dw_operaciones.InsertRow(0)

dw_puertos.GetChild("puer_codigo", idwc_puertos)
idwc_puertos.SetTransObject(sqlca)
idwc_puertos.Retrieve(900)
dw_puertos.InsertRow(0)

dw_tiposalida.GetChild("defe_tiposa", idwc_tiposalida)
idwc_tiposalida.SetTransObject(sqlca)
idwc_tiposalida.Retrieve()
dw_tiposalida.InsertRow(0)

em_desde.Text				=	String(RelativeDate(Today(), -365))
em_hasta.Text				=	String(Today())
istr_mant.argumento[3]	= 	em_desde.Text				//	fecha inicio
istr_mant.argumento[4]	=	em_hasta.Text				//	fecha final
istr_mant.argumento[5]	= 	"-9"							//	operacion
istr_mant.argumento[6]	= 	"-1"							//	embarque
istr_mant.argumento[10]	=	'*'							// Calibre

istr_mant.argumento[30]	= 	"-9"							//	Puerto Origen
istr_mant.argumento[31]	= 	"-1"							//	Tipo Salida

ll_norden					=	-9								// Nº orden

cbx_operacion.Enabled	=	False


end event

type pb_excel from w_para_informes`pb_excel within w_info_traslados_camiones
integer x = 3506
integer y = 1072
end type

type st_computador from w_para_informes`st_computador within w_info_traslados_camiones
integer textsize = -10
fontfamily fontfamily = swiss!
string facename = "Tahoma"
end type

type st_usuario from w_para_informes`st_usuario within w_info_traslados_camiones
integer textsize = -10
fontfamily fontfamily = swiss!
string facename = "Tahoma"
end type

type st_temporada from w_para_informes`st_temporada within w_info_traslados_camiones
integer textsize = -10
fontfamily fontfamily = swiss!
string facename = "Tahoma"
end type

type p_logo from w_para_informes`p_logo within w_info_traslados_camiones
end type

type st_titulo from w_para_informes`st_titulo within w_info_traslados_camiones
integer width = 3131
string text = "Listado de Traslados"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_traslados_camiones
string tag = "Imprimir Reporte"
integer x = 3493
integer y = 1548
integer taborder = 230
end type

event pb_acepta::clicked;SetPointer(Arrow!)

Integer	fila, li_solotermografos, li_PtoOri, li_TipSalida
String		texto_desde, texto_hasta, texto_fecha, is_Instructivo, ls_orden, ls_lista 
			
istr_info.titulo	= 'LISTADO DE TRASLADOS (CAMIONES)'

OpenWithParm(vinf, istr_info)
vinf.dw_1.DataObject = "dw_info_traslados_camiones"  

ii_operacion		=	Integer(istr_mant.argumento[5])				//	operacion
is_embarque	=	istr_mant.argumento[6]							//	embarque

texto_desde		=  f_fecha_texto(em_desde.Text, 1)
texto_hasta		=	f_fecha_texto(em_hasta.Text, 1)
texto_fecha		=	"Desde El :  " + texto_desde + "   Hasta El :  " + texto_hasta

vinf.dw_1.SetTransObject(sqlca)

If cbx_solotermografos.Checked Then
	li_solotermografos = 1
Else
	li_solotermografos = 0
End If

ls_lista = uo_SelProductor.Lista

fila	=	vinf.dw_1.Retrieve(uo_SelCliente.Codigo, uo_SelPlantaOrigen.Codigo, ii_operacion, is_embarque, Date(em_Desde.Text), Date(em_Hasta.Text),&
								uo_SelTipoCamion.Codigo , li_SoloTermografos,uo_SelTransportista.Codigo, uo_selEspecie.Codigo,uo_SelVariedad.Codigo,&
								Integer(istr_mant.argumento[30]),Integer(istr_mant.argumento[31]), istr_mant.argumento[10],&
								ll_norden,uo_SelPlanta.Codigo,uo_SelZona.Codigo,ls_lista,uo_SelDestino.codigo)

If fila = -1 Then
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
			   	"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ElseIf fila = 0 Then
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
Else
	F_Membrete(vinf.dw_1)
	vinf.dw_1.ModIfy("fechas.text = '" + texto_fecha + "'")
	If gs_Ambiente <> 'Windows' Then F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
End If

SetPointer(Arrow!)				
end event

type pb_salir from w_para_informes`pb_salir within w_info_traslados_camiones
string tag = "Salir [Cerrar Ventana Activa]"
integer x = 3497
integer y = 1844
integer taborder = 240
string powertiptext = "Salir [Cerrar Ventana Activa]"
end type

type st_4 from statictext within w_info_traslados_camiones
integer x = 233
integer y = 444
integer width = 1595
integer height = 832
boolean bringtotop = true
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

type st_1 from statictext within w_info_traslados_camiones
integer x = 1856
integer y = 548
integer width = 229
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

type st_2 from statictext within w_info_traslados_camiones
integer x = 1847
integer y = 1456
integer width = 334
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
string text = "Inicio Desp"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type em_desde from editmask within w_info_traslados_camiones
integer x = 2226
integer y = 1440
integer width = 393
integer height = 96
integer taborder = 120
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

type st_6 from statictext within w_info_traslados_camiones
integer x = 302
integer y = 552
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
string text = "Cliente"
boolean focusrectangle = false
end type

type st_7 from statictext within w_info_traslados_camiones
integer x = 2651
integer y = 1456
integer width = 265
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

type em_hasta from editmask within w_info_traslados_camiones
integer x = 2921
integer y = 1440
integer width = 393
integer height = 96
integer taborder = 150
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

type dw_pesoneto from datawindow within w_info_traslados_camiones
boolean visible = false
integer x = 1042
integer y = 2572
integer width = 544
integer height = 84
boolean bringtotop = true
string dataobject = "dddw_pesoneto"
boolean border = false
boolean livescroll = true
end type

event itemerror;RETURN 1
end event

type tit_peso from statictext within w_info_traslados_camiones
boolean visible = false
integer x = 850
integer y = 2588
integer width = 183
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long backcolor = 12632256
string text = "Peso"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_15 from statictext within w_info_traslados_camiones
integer x = 1829
integer y = 1416
integer width = 1541
integer height = 156
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

type st_16 from statictext within w_info_traslados_camiones
integer x = 233
integer y = 1276
integer width = 1595
integer height = 888
boolean bringtotop = true
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

type st_10 from statictext within w_info_traslados_camiones
integer x = 251
integer y = 1384
integer width = 315
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
string text = "Operacion"
boolean focusrectangle = false
end type

type cbx_operacion from checkbox within w_info_traslados_camiones
integer x = 640
integer y = 1312
integer width = 320
integer height = 88
integer taborder = 170
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
	istr_mant.argumento[5]	=	'-1'
	dw_operaciones.Enabled			=	False	
ELSE
	dw_operaciones.Enabled			=	True
	uo_SelCliente.Todos(False)
	uo_SelCliente.Inicia(gi_CodExport)
	dw_operaciones.SetFocus()
END IF



end event

type dw_operaciones from datawindow within w_info_traslados_camiones
integer x = 640
integer y = 1400
integer width = 974
integer height = 92
integer taborder = 200
boolean bringtotop = true
boolean enabled = false
string title = "none"
string dataobject = "dddw_operacion"
boolean border = false
boolean livescroll = true
end type

event itemchanged;String	ls_Columna[]
Integer	li_Nula

SetNull(li_Nula)

If ExisteOperacion(uo_SelCliente.Codigo, Integer(data), ls_Columna[]) Then
	ii_Operacion	=	Integer(data)
	istr_mant.argumento[5] = data
	idwc_embarque.retrieve(uo_SelCliente.Codigo, ii_OPeracion)
	is_Operacion	=	String(ii_Operacion,'###') + ' ' + ls_Columna[4]
	is_NomEmbarque	=	ls_Columna[1] + ' ' +ls_Columna[2]	
End If
end event

event itemerror;RETURN 1
end event

type st_14 from statictext within w_info_traslados_camiones
integer x = 251
integer y = 1568
integer width = 311
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
string text = "Embarque"
boolean focusrectangle = false
end type

type cbx_embarque from checkbox within w_info_traslados_camiones
integer x = 640
integer y = 1492
integer width = 315
integer height = 92
integer taborder = 210
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
	istr_mant.argumento[6]	= 	"-1"
	dw_embarques.Enabled	=	False
ELSE
	uo_SelCliente.Todos(False)
	uo_SelCliente.Inicia(gi_CodExport)
	dw_embarques.Enabled	=	True
	dw_embarques.SetFocus()
END IF
end event

type dw_embarques from datawindow within w_info_traslados_camiones
integer x = 640
integer y = 1572
integer width = 960
integer height = 92
integer taborder = 220
boolean bringtotop = true
boolean enabled = false
string title = "none"
string dataobject = "dddw_embarques_operacion"
boolean border = false
boolean livescroll = true
end type

event itemchanged;String	ls_Columna[], ls_Nula

SetNull(ls_Nula)

If ExistEmbarque(uo_SelCliente.Codigo, data, ls_Columna[]) Then
	istr_mant.argumento[6]	= 	ls_Columna[3]
	If Integer(ls_Columna[5]) <> ii_Operacion Then
		MessageBox("Atención", "Embarque corresponde a otra Operación (" + ls_Columna[3] + ")")
		is_Embarque	=	""
		This.SetItem(1, "embq_codigo", ls_Nula)			
		Return 1
	End If
	
   If ii_Operacion = -1 Then
		dw_operaciones.SetItem(1, "oper_codigo", Integer(ls_Columna[5]))
		ii_Operacion = Integer(ls_Columna[3])
	End If
End If
end event

event itemerror;RETURN 1
end event

type cbx_1 from checkbox within w_info_traslados_camiones
boolean visible = false
integer x = 901
integer y = 2484
integer width = 631
integer height = 80
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
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
	dw_pesoneto.Modify("enva_pesone.BackGround.Color = " + String(RGB(166,180,210)))
END IF

end event

type cbx_operacioncons from checkbox within w_info_traslados_camiones
integer x = 1120
integer y = 1308
integer width = 471
integer height = 92
integer taborder = 190
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Consolidados"
boolean checked = true
end type

event clicked;IF This.Checked = TRUE THEN
	istr_mant.argumento[5]	=	'-9'
	cbx_operacion.Enabled	=	False
	cbx_operacion.Checked	=	True
	dw_operaciones.Enabled	=	False	
ELSE
	istr_mant.argumento[5]	=	'-1'
	cbx_operacion.Enabled	=	True

END IF

	
end event

type st_3 from statictext within w_info_traslados_camiones
integer x = 1847
integer y = 744
integer width = 430
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
string text = "Transportista"
boolean focusrectangle = false
end type

type st_5 from statictext within w_info_traslados_camiones
integer x = 1829
integer y = 1572
integer width = 1541
integer height = 128
boolean bringtotop = true
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

type cbx_solotermografos from checkbox within w_info_traslados_camiones
integer x = 2574
integer y = 1588
integer width = 750
integer height = 96
integer taborder = 60
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Solo Con Termógrafos "
end type

type st_8 from statictext within w_info_traslados_camiones
integer x = 302
integer y = 944
integer width = 384
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
string text = "Tipo Camión"
boolean focusrectangle = false
end type

type uo_selespecie from uo_seleccion_especie within w_info_traslados_camiones
event destroy ( )
integer x = 690
integer y = 1064
integer taborder = 80
boolean bringtotop = true
end type

on uo_selespecie.destroy
call uo_seleccion_especie::destroy
end on

event ue_cambio;call super::ue_cambio;IF IsNull(This.Codigo) THEN RETURN

CHOOSE CASE This.Codigo
	CASE -1, -9
		uo_selvariedad.cbx_Todos.Checked			=	True
		uo_selvariedad.cbx_Consolida.Enabled	=	True
		uo_selvariedad.Enabled						=	False
		
	CASE ELSE
		uo_selvariedad.Enabled		=	True
		uo_selvariedad.Filtra(This.Codigo)

END CHOOSE
end event

type uo_selvariedad from uo_seleccion_variedad within w_info_traslados_camiones
integer x = 2281
integer y = 856
integer height = 184
integer taborder = 100
boolean bringtotop = true
end type

on uo_selvariedad.destroy
call uo_seleccion_variedad::destroy
end on

type st_12 from statictext within w_info_traslados_camiones
integer x = 302
integer y = 1144
integer width = 384
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
string text = "Especie"
boolean focusrectangle = false
end type

type st_13 from statictext within w_info_traslados_camiones
integer x = 1847
integer y = 944
integer width = 361
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
string text = "Variedad"
boolean focusrectangle = false
end type

type st_17 from statictext within w_info_traslados_camiones
integer x = 1829
integer y = 1072
integer width = 1541
integer height = 344
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

type st_18 from statictext within w_info_traslados_camiones
integer x = 1888
integer y = 1288
integer width = 361
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
string text = "Productor"
boolean focusrectangle = false
end type

type st_19 from statictext within w_info_traslados_camiones
integer x = 251
integer y = 1736
integer width = 389
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
string text = "Puerto Orig."
boolean focusrectangle = false
end type

type dw_puertos from datawindow within w_info_traslados_camiones
integer x = 640
integer y = 1752
integer width = 997
integer height = 92
integer taborder = 310
boolean bringtotop = true
boolean enabled = false
string title = "none"
string dataobject = "dddw_puertos"
boolean border = false
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event itemchanged;istr_mant.argumento[30]	=	data

end event

event itemerror;RETURN 1
end event

event losefocus;AcceptText()
end event

type cbx_puertos from checkbox within w_info_traslados_camiones
integer x = 640
integer y = 1684
integer width = 366
integer height = 56
integer taborder = 290
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
	cbx_puertoscons.Enabled									=	True
	dw_puertos.Enabled										=	False
	dw_puertos.Object.puer_codigo.BackGround.Color	=	RGB(192, 192, 192)
	istr_mant.argumento[30]									=	'-1'
ELSE
	cbx_puertoscons.Enabled									=	False
	cbx_puertoscons.Checked									=	False
	dw_puertos.Enabled										=	True
	dw_puertos.Object.puer_codigo.BackGround.Color	=	RGB(255, 255, 255)
	dw_puertos.SetFocus()
END IF


end event

type cbx_puertoscons from checkbox within w_info_traslados_camiones
integer x = 1125
integer y = 1684
integer width = 471
integer height = 56
integer taborder = 270
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Consolidados"
boolean checked = true
end type

event clicked;IF This.Checked = TRUE THEN
	istr_mant.argumento[30]	=	'-9'
	cbx_puertos.Enabled	=	False
	cbx_puertos.Checked	=	True	
ELSE
	cbx_puertos.Enabled	=	True
   istr_mant.argumento[30] =	'-1'
END IF
end event

type st_22 from statictext within w_info_traslados_camiones
integer x = 1847
integer y = 1828
integer width = 357
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
string text = "Tipo Salida"
boolean focusrectangle = false
end type

type cbx_4 from checkbox within w_info_traslados_camiones
integer x = 2304
integer y = 1736
integer width = 402
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
string text = "Todos"
boolean checked = true
end type

event clicked;IF This.Checked THEN
	dw_tiposalida.Enabled = False
	istr_mant.argumento[31]	=	'-1'
	
	uo_selplanta.Enabled = False
	uo_SelPlanta.Seleccion(True, False)
	uo_SelPlanta.LimpiarDatos()
	uo_SelPlanta.cbx_Todos.Checked =	True
	uo_SelPlanta.Codigo = -1
ELSE
	dw_tiposalida.Enabled = True
	uo_SelPlanta.Codigo = -1
	istr_mant.argumento[31]	=	String(dw_tiposalida.Object.defe_tiposa[1])	
	IF dw_tiposalida.Object.defe_tiposa[1] = 11 THEN
		uo_selplanta.Enabled = True
	END IF	
END IF
end event

type dw_tiposalida from datawindow within w_info_traslados_camiones
integer x = 2304
integer y = 1820
integer width = 896
integer height = 92
integer taborder = 110
boolean bringtotop = true
boolean enabled = false
string title = "none"
string dataobject = "ddlbdw_tiposalida"
boolean border = false
boolean livescroll = true
end type

event itemchanged;istr_mant.argumento[31]	=	data

IF data = '11' THEN
	uo_SelPlanta.Enabled = True
ELSE	
	uo_SelPlanta.Seleccion(True, False)
	uo_SelPlanta.LimpiarDatos()
	uo_SelPlanta.Enabled = False
	uo_SelPlanta.Codigo = -1
END IF	
end event

event itemerror;RETURN 1
end event

type st_20 from statictext within w_info_traslados_camiones
integer x = 1829
integer y = 1700
integer width = 1541
integer height = 464
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

type st_calidad from statictext within w_info_traslados_camiones
integer x = 1847
integer y = 1596
integer width = 256
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
string text = "Calidad"
boolean focusrectangle = false
end type

type em_calidad from editmask within w_info_traslados_camiones
integer x = 2103
integer y = 1592
integer width = 297
integer height = 84
integer taborder = 280
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
boolean enabled = false
alignment alignment = center!
textcase textcase = upper!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = stringmask!
string mask = "xxxxx"
end type

event modified;Integer	li_especie, li_variedad, li_cantid
String	ls_calibre

IF This.Text <> '' THEN
	
	li_especie	=	Integer(uo_selespecie.Codigo) // Especie
	li_variedad	=	Integer(uo_selvariedad.Codigo) // Variedad
	ls_calibre	=	This.Text
	
	IF NOT iuo_calibre.existe(li_especie,li_variedad,ls_calibre,True,sqlca) THEN
		em_calidad.Text = ''
		This.SetFocus()
		Return 1
	ELSE	
		istr_mant.argumento[10]	= iuo_calibre.calibre
		em_calidad.Text			 	= iuo_calibre.calibre
		Return 1
	END IF	
END IF	


end event

type cbx_calidad from checkbox within w_info_traslados_camiones
integer x = 2418
integer y = 1592
integer width = 155
integer height = 80
integer taborder = 250
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "T"
boolean checked = true
end type

event clicked;IF This.Checked THEN
	em_calidad.Text			=	''
	em_calidad.Enabled		=	False
	istr_mant.argumento[10]	=	'*'
ELSE
	em_calidad.Enabled		=	True
	em_calidad.SetFocus()
END IF

end event

type st_21 from statictext within w_info_traslados_camiones
integer x = 251
integer y = 1876
integer width = 398
integer height = 68
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Nro. Orden"
boolean focusrectangle = false
end type

type em_norden from editmask within w_info_traslados_camiones
integer x = 649
integer y = 1864
integer width = 430
integer height = 84
integer taborder = 300
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
boolean enabled = false
alignment alignment = center!
borderstyle borderstyle = stylelowered!
string mask = "########"
end type

event modified;ll_norden = Long(this.text)
end event

type cbx_5 from checkbox within w_info_traslados_camiones
integer x = 1102
integer y = 1876
integer width = 270
integer height = 68
integer taborder = 160
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
string text = "Todos"
boolean checked = true
end type

event clicked;IF This.Checked THEN
	em_norden.Enabled = False
	em_norden.Text = ''
	ll_norden = -1
ELSE
	em_norden.Enabled = True
	ll_norden = Long(em_norden.Text)
END IF
end event

type cbx_6 from checkbox within w_info_traslados_camiones
integer x = 1367
integer y = 1876
integer width = 443
integer height = 68
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Consolidada"
boolean checked = true
end type

event clicked;IF This.Checked THEN
	em_norden.Enabled = False
	em_norden.Text = ''
	ll_norden = -9
	cbx_5.Checked = True
	cbx_5.Enabled = False
ELSE
	em_norden.Enabled = False
	ll_norden = -1
	cbx_5.Checked = True
	cbx_5.Enabled = True
END IF
end event

type st_11 from statictext within w_info_traslados_camiones
integer x = 1829
integer y = 448
integer width = 1541
integer height = 620
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

type uo_selplanta from uo_seleccion_plantas within w_info_traslados_camiones
integer x = 2304
integer y = 1952
integer height = 184
integer taborder = 180
boolean bringtotop = true
boolean enabled = false
end type

on uo_selplanta.destroy
call uo_seleccion_plantas::destroy
end on

type st_23 from statictext within w_info_traslados_camiones
integer x = 1847
integer y = 2044
integer width = 434
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
string text = "Planta Destino"
boolean focusrectangle = false
end type

type uo_selzona from uo_seleccion_zonas within w_info_traslados_camiones
integer x = 690
integer y = 656
integer taborder = 140
boolean bringtotop = true
end type

on uo_selzona.destroy
call uo_seleccion_zonas::destroy
end on

type st_24 from statictext within w_info_traslados_camiones
integer x = 302
integer y = 744
integer width = 384
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
string text = "Zonas"
boolean focusrectangle = false
end type

type uo_selproductor from uo_seleccion_varios_productores_clientes within w_info_traslados_camiones
integer x = 2281
integer y = 1108
integer width = 891
integer taborder = 320
boolean bringtotop = true
end type

on uo_selproductor.destroy
call uo_seleccion_varios_productores_clientes::destroy
end on

event ue_cambio;call super::ue_cambio;uo_selproductor.Filtra(-1,-1, uo_SelCliente.Codigo)
end event

type uo_seldestino from uo_seleccion_destinos within w_info_traslados_camiones
integer x = 649
integer y = 1968
integer taborder = 260
boolean bringtotop = true
end type

on uo_seldestino.destroy
call uo_seleccion_destinos::destroy
end on

type st_25 from statictext within w_info_traslados_camiones
integer x = 251
integer y = 2028
integer width = 352
integer height = 68
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Destino"
boolean focusrectangle = false
end type

type uo_selcliente from uo_seleccion_clientesprod within w_info_traslados_camiones
integer x = 690
integer y = 452
integer taborder = 40
boolean bringtotop = true
end type

on uo_selcliente.destroy
call uo_seleccion_clientesprod::destroy
end on

type uo_selplantaorigen from uo_seleccion_plantas within w_info_traslados_camiones
event destroy ( )
integer x = 2281
integer y = 452
integer taborder = 40
boolean bringtotop = true
end type

on uo_selplantaorigen.destroy
call uo_seleccion_plantas::destroy
end on

type uo_seltransportista from uo_seleccion_transportista within w_info_traslados_camiones
event destroy ( )
integer x = 2281
integer y = 656
integer taborder = 80
boolean bringtotop = true
end type

on uo_seltransportista.destroy
call uo_seleccion_transportista::destroy
end on

type uo_seltipocamion from uo_seleccion_tipocamion within w_info_traslados_camiones
integer x = 690
integer y = 860
integer taborder = 150
boolean bringtotop = true
end type

on uo_seltipocamion.destroy
call uo_seleccion_tipocamion::destroy
end on

