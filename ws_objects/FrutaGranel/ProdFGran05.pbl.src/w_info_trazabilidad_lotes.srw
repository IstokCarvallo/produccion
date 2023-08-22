$PBExportHeader$w_info_trazabilidad_lotes.srw
$PBExportComments$Informe Existencia Sitio Revisión
forward
global type w_info_trazabilidad_lotes from w_para_informes
end type
type st_1 from statictext within w_info_trazabilidad_lotes
end type
type st_6 from statictext within w_info_trazabilidad_lotes
end type
type st_8 from statictext within w_info_trazabilidad_lotes
end type
type st_2 from statictext within w_info_trazabilidad_lotes
end type
type em_desde from editmask within w_info_trazabilidad_lotes
end type
type st_7 from statictext within w_info_trazabilidad_lotes
end type
type em_hasta from editmask within w_info_trazabilidad_lotes
end type
type st_3 from statictext within w_info_trazabilidad_lotes
end type
type em_proceso from editmask within w_info_trazabilidad_lotes
end type
type cb_buscaorden from commandbutton within w_info_trazabilidad_lotes
end type
type cbx_todosproc from checkbox within w_info_trazabilidad_lotes
end type
type gb_5 from groupbox within w_info_trazabilidad_lotes
end type
type st_4 from statictext within w_info_trazabilidad_lotes
end type
type cbx_todasfec from checkbox within w_info_trazabilidad_lotes
end type
type cbx_todoscolo from checkbox within w_info_trazabilidad_lotes
end type
type dw_colores from datawindow within w_info_trazabilidad_lotes
end type
type st_9 from statictext within w_info_trazabilidad_lotes
end type
type st_10 from statictext within w_info_trazabilidad_lotes
end type
type uo_selplantas from uo_seleccion_plantas within w_info_trazabilidad_lotes
end type
type uo_selclientes from uo_seleccion_clientesprod within w_info_trazabilidad_lotes
end type
type uo_selproductor from uo_seleccion_productor within w_info_trazabilidad_lotes
end type
type uo_selvariedad from uo_seleccion_variedad within w_info_trazabilidad_lotes
end type
end forward

global type w_info_trazabilidad_lotes from w_para_informes
integer x = 14
integer y = 32
integer width = 2491
integer height = 2208
string title = "CUADRATURA PRODUCTOR"
boolean minbox = false
boolean maxbox = false
boolean resizable = false
windowtype windowtype = response!
string icon = "F:\Desarrollo\Producción\FrutaProcesada\Producc.ico"
st_1 st_1
st_6 st_6
st_8 st_8
st_2 st_2
em_desde em_desde
st_7 st_7
em_hasta em_hasta
st_3 st_3
em_proceso em_proceso
cb_buscaorden cb_buscaorden
cbx_todosproc cbx_todosproc
gb_5 gb_5
st_4 st_4
cbx_todasfec cbx_todasfec
cbx_todoscolo cbx_todoscolo
dw_colores dw_colores
st_9 st_9
st_10 st_10
uo_selplantas uo_selplantas
uo_selclientes uo_selclientes
uo_selproductor uo_selproductor
uo_selvariedad uo_selvariedad
end type
global w_info_trazabilidad_lotes w_info_trazabilidad_lotes

type variables
str_busqueda istr_busq
str_mant istr_mant

DataWindowChild	idwc_colores, idwc_Productor
						

Integer 				ii_TipoOrden, il_ppre_numero
String 				is_color
Date					id_fecini, id_fecter
end variables

forward prototypes
public function boolean noexisteordenproceso (long al_orden)
public function boolean noexisteordenreproceso (long al_orden, integer ai_tipo)
end prototypes

public function boolean noexisteordenproceso (long al_orden);Integer	li_Especie, li_Estado, li_Variedad, li_Seguimto, li_Linea, li_periodo
String		ls_Seguimiento[3]={'Productor','Huerto','Cuartel'} 
Date  		ldt_Fecha
Boolean	lb_Retorno = True
Long		ll_Productor


SELECT	espe_codigo, orpr_estado, orpr_fecpro, prod_codigo,
			vari_codigo, orpr_niveld, line_codigo, pefr_codigo, 
			ppre_numero
	INTO	:li_Especie, :li_Estado, :ldt_Fecha, :ll_Productor, 
			:li_Variedad, :li_Seguimto, :li_Linea, :li_periodo, 
			:il_ppre_numero
	FROM	dbo.spro_ordenproceso
	WHERE	:uo_SelPlantas.Codigo in 	(-1, plde_codigo)
		AND	orpr_tipord		=	:ii_TipoOrden
		AND	orpr_numero	=	:al_Orden
		AND 	clie_codigo		= 	:uo_SelClientes.Codigo;
		 
IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla Spro_Ordenproceso")
	lb_Retorno	=	False
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Error", "No se encuentra el PreProceso ingresado.")
	lb_Retorno	=	False
ELSEIF sqlca.SQLCode = 0 THEN
	em_desde.text			=	String(ldt_Fecha,'dd/mm/yyyy')
	em_hasta.text			=	String(ldt_Fecha,'dd/mm/yyyy')
	lb_Retorno	=	True
END IF

RETURN lb_Retorno
end function

public function boolean noexisteordenreproceso (long al_orden, integer ai_tipo);Integer	li_Especie,  li_Estado, li_Linea, li_Nula, &
			li_Variedad, li_Seguimto, li_periodo
Date  	ldt_Fecha
Boolean	lb_Retorno = True
Long		ll_Productor, ll_ppre_numero

SetNull(li_Nula)

SELECT	espe_codigo, dinp_estado, dinp_fecdoc, line_codigo
	INTO	:li_Especie, :li_Estado, :ldt_Fecha, :li_Linea
	FROM	dba.spro_doctointernopack
	WHERE	:uo_SelPlantas.Codigo in (-1,plde_codigo)
	AND	dinp_tipdoc	=	:ai_tipo
	AND	dinp_numero	=	:al_Orden;
		 
IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla spro_doctointernopack")
ELSEIF sqlca.SQLCode <> 100 THEN
	em_desde.text								=	String(Date(ldt_Fecha))
	em_hasta.text								=	String(Date(ldt_Fecha))
	em_hasta.Enabled			=	FALSE
	lb_Retorno					=	FALSE
END IF

RETURN lb_Retorno
end function

on w_info_trazabilidad_lotes.create
int iCurrent
call super::create
this.st_1=create st_1
this.st_6=create st_6
this.st_8=create st_8
this.st_2=create st_2
this.em_desde=create em_desde
this.st_7=create st_7
this.em_hasta=create em_hasta
this.st_3=create st_3
this.em_proceso=create em_proceso
this.cb_buscaorden=create cb_buscaorden
this.cbx_todosproc=create cbx_todosproc
this.gb_5=create gb_5
this.st_4=create st_4
this.cbx_todasfec=create cbx_todasfec
this.cbx_todoscolo=create cbx_todoscolo
this.dw_colores=create dw_colores
this.st_9=create st_9
this.st_10=create st_10
this.uo_selplantas=create uo_selplantas
this.uo_selclientes=create uo_selclientes
this.uo_selproductor=create uo_selproductor
this.uo_selvariedad=create uo_selvariedad
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_1
this.Control[iCurrent+2]=this.st_6
this.Control[iCurrent+3]=this.st_8
this.Control[iCurrent+4]=this.st_2
this.Control[iCurrent+5]=this.em_desde
this.Control[iCurrent+6]=this.st_7
this.Control[iCurrent+7]=this.em_hasta
this.Control[iCurrent+8]=this.st_3
this.Control[iCurrent+9]=this.em_proceso
this.Control[iCurrent+10]=this.cb_buscaorden
this.Control[iCurrent+11]=this.cbx_todosproc
this.Control[iCurrent+12]=this.gb_5
this.Control[iCurrent+13]=this.st_4
this.Control[iCurrent+14]=this.cbx_todasfec
this.Control[iCurrent+15]=this.cbx_todoscolo
this.Control[iCurrent+16]=this.dw_colores
this.Control[iCurrent+17]=this.st_9
this.Control[iCurrent+18]=this.st_10
this.Control[iCurrent+19]=this.uo_selplantas
this.Control[iCurrent+20]=this.uo_selclientes
this.Control[iCurrent+21]=this.uo_selproductor
this.Control[iCurrent+22]=this.uo_selvariedad
end on

on w_info_trazabilidad_lotes.destroy
call super::destroy
destroy(this.st_1)
destroy(this.st_6)
destroy(this.st_8)
destroy(this.st_2)
destroy(this.em_desde)
destroy(this.st_7)
destroy(this.em_hasta)
destroy(this.st_3)
destroy(this.em_proceso)
destroy(this.cb_buscaorden)
destroy(this.cbx_todosproc)
destroy(this.gb_5)
destroy(this.st_4)
destroy(this.cbx_todasfec)
destroy(this.cbx_todoscolo)
destroy(this.dw_colores)
destroy(this.st_9)
destroy(this.st_10)
destroy(this.uo_selplantas)
destroy(this.uo_selclientes)
destroy(this.uo_selproductor)
destroy(this.uo_selvariedad)
end on

event open;call super::open;Boolean	lb_Cerrar = False

If IsNull(uo_SelClientes.Codigo) Then lb_Cerrar = True
If IsNull(uo_SelPlantas.Codigo) Then lb_Cerrar = True
If IsNull(uo_SelProductor.Codigo) Then lb_Cerrar = True
If IsNull(uo_SelVariedad.Codigo) Then lb_Cerrar = True

If lb_Cerrar Then
	Close(This)
Else
	uo_SelClientes.Seleccion(False, False)
	uo_SelPlantas.Seleccion(False, False)	
	uo_SelProductor.Seleccion(True, False)
	uo_SelVariedad.Seleccion(True, False)
	uo_SelClientes.Inicia(gi_CodExport)
	uo_SelPlantas.Inicia(gi_CodPlanta)
	uo_SelPlantas.Filtra(1)
	uo_SelProductor.Filtra(-1)
	uo_SelVariedad.Filtra(-1)
	
	dw_colores.GetChild("lote_calibr", idwc_colores)
	idwc_colores.SetTransObject(sqlca)
	idwc_colores.Retrieve()
	dw_colores.InsertRow(0)
	
	ii_TipoOrden	=	8
	
	is_color						=	'-1 '
	em_proceso.Text			=	''
	cb_buscaorden.Enabled	=	False
	em_proceso.Enabled		=	False
	
	id_fecini						=	RelativeDate(Today(), - (365 * 7) )
	id_fecter						=	Today()
	em_desde.Text				=	String(RelativeDate(Today(), - (365 * 7) ) )
	em_hasta.Text				=	String(Today())
	em_desde.enabled 		= 	False
	em_hasta.enabled 		= 	False
End If
end event

type pb_excel from w_para_informes`pb_excel within w_info_trazabilidad_lotes
end type

type st_computador from w_para_informes`st_computador within w_info_trazabilidad_lotes
end type

type st_usuario from w_para_informes`st_usuario within w_info_trazabilidad_lotes
end type

type st_temporada from w_para_informes`st_temporada within w_info_trazabilidad_lotes
end type

type p_logo from w_para_informes`p_logo within w_info_trazabilidad_lotes
end type

type st_titulo from w_para_informes`st_titulo within w_info_trazabilidad_lotes
integer width = 1664
string text = "Trazabilidad de Lotes"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_trazabilidad_lotes
integer x = 1998
integer y = 1524
integer taborder = 170
end type

event pb_acepta::clicked;SetPointer(Arrow!)

Integer	fila, li_cliente, li_planta, li_especie, li_variedad, li_etiqueta, li_consplanta,&
			li_consproductor, li_consvariedad, li_consembalaje, li_consetiqueta,li_conscalidad,&
			li_conspacking, li_packing, li_zona
String	texto_desde, texto_hasta, texto_fecha, ls_null
Long		ll_Orden

SetNull(ls_null)

istr_info.titulo	= 'CUADRATURA PRODUCTOR'
OpenWithParm(vinf, istr_info)
vinf.dw_1.DataObject = "dw_info_trazabili_lote"
vinf.dw_1.SetTransObject(sqlca)

vinf.dw_1.GetChild("prod_codori", idwc_Productor)
idwc_Productor.SetTransObject(Sqlca)
idwc_Productor.Retrieve(-1)

IF ii_TipoOrden = 0 or cbx_todosproc.Checked THEN
	ll_Orden = -1
ELSE
	ll_Orden		=	Long(em_Proceso.text)
	
	IF IsNull(ll_Orden) THEN
		MessageBox("Error de Datos", "Falta el Ingreso de un Número de Orden.")
		RETURN
	END IF	
END IF

IF IsNull(ii_TipoOrden) THEN
	MessageBox("Error de Datos", "Falta el Ingreso del Tipo de Proceso.")
	RETURN
END IF

texto_desde	=  f_fecha_texto(String(id_fecini), 1)
texto_hasta	=	f_fecha_texto(String(id_fecter), 1)
texto_fecha	=	"Desde El :  " + texto_desde + "   Hasta El :  " + texto_hasta

IF IsNull(id_fecini) OR IsNull(id_fecter)THEN
	MessageBox("Error", "Debe Ingresar todos los parametros para el informe", StopSign!)
	Return 1
END IF

fila	=	vinf.dw_1.Retrieve(uo_SelClientes.Codigo, uo_SelPlantas.Codigo, uo_SelVariedad.Codigo, is_color, uo_SelProductor.Codigo, ll_Orden, id_fecini,id_fecter)

IF fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
			   	"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", &
	             StopSign!, Ok!)
ELSE
	F_Membrete(vinf.dw_1)
	vinf.dw_1.Modify("fechas.text = '" + texto_fecha + "'")
	vinf.dw_1.Modify("t_cliente.text = '" + uo_SelClientes.Nombre + "'")		
	IF gs_Ambiente <> 'Windows' THEN F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
END IF

SetPointer(Arrow!)				
end event

type pb_salir from w_para_informes`pb_salir within w_info_trazabilidad_lotes
integer x = 1998
integer y = 1796
integer taborder = 180
end type

type st_1 from statictext within w_info_trazabilidad_lotes
integer x = 302
integer y = 688
integer width = 238
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

type st_6 from statictext within w_info_trazabilidad_lotes
integer x = 302
integer y = 536
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

type st_8 from statictext within w_info_trazabilidad_lotes
integer x = 302
integer y = 896
integer width = 329
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

type st_2 from statictext within w_info_trazabilidad_lotes
integer x = 302
integer y = 1836
integer width = 229
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
string text = "Inicio"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type em_desde from editmask within w_info_trazabilidad_lotes
integer x = 608
integer y = 1820
integer width = 462
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

event modified;istr_mant.argumento[9]	=	This.Text
id_fecini					= 	Date(this.Text)
end event

type st_7 from statictext within w_info_trazabilidad_lotes
integer x = 1093
integer y = 1836
integer width = 279
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

type em_hasta from editmask within w_info_trazabilidad_lotes
integer x = 1349
integer y = 1820
integer width = 462
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

event modified;istr_mant.argumento[10]	=	This.Text
id_fecter					= 	Date(this.Text)
end event

type st_3 from statictext within w_info_trazabilidad_lotes
integer x = 302
integer y = 1092
integer width = 393
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
string text = "Nº PreProceso"
borderstyle borderstyle = stylelowered!
boolean focusrectangle = false
end type

type em_proceso from editmask within w_info_trazabilidad_lotes
integer x = 699
integer y = 1088
integer width = 384
integer height = 84
integer taborder = 70
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 1090519039
alignment alignment = center!
borderstyle borderstyle = stylelowered!
string mask = "########"
end type

event modified;Long ll_orden

ll_orden = long(this.text)

CHOOSE CASE ii_TipoOrden
	CASE 4, 8
		IF NoExisteOrdenProceso(ll_orden) THEN
			This.Text=""
		END IF
		
	CASE 5, 6
		IF NoExisteOrdenReProceso(ll_orden,ii_TipoOrden) THEN
			this.Text=""
		END IF
	
END CHOOSE
end event

type cb_buscaorden from commandbutton within w_info_trazabilidad_lotes
integer x = 1115
integer y = 1092
integer width = 91
integer height = 76
integer taborder = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "..."
end type

event clicked;Str_busqueda	lstr_busq

CHOOSE CASE ii_TipoOrden
	CASE 8
		lstr_busq.argum[1]	=	String(uo_SelPlantas.Codigo)
		lstr_busq.argum[2]	=	"1"
		lstr_busq.argum[3]	=	string(ii_TipoOrden)
		lstr_busq.argum[4]	=	string(uo_SelClientes.Codigo)
		lstr_busq.argum[6]	=	""
		
		OpenWithParm(w_busc_spro_ordenproceso, lstr_busq)
		
		lstr_busq	= Message.PowerObjectParm
		
		IF lstr_busq.argum[6] <> "" THEN
			em_proceso.text	=	lstr_busq.argum[6]
			IF NoExisteOrdenProceso(Long(lstr_busq.argum[6])) THEN
				 em_proceso.text=""
			END IF	
		END IF
		RETURN 1
	
	CASE 5
		lstr_Busq.Argum[1]	=	'5'	// Tipo Orde
		OpenWithParm(w_busqueda_doctointernopack, lstr_Busq)
		lstr_Busq	= Message.PowerObjectParm

		IF lstr_Busq.Argum[2] <> "" THEN
			em_proceso.text	=	lstr_busq.argum[3]
			IF NoExisteOrdenReProceso(Long(lstr_Busq.Argum[3]),5) THEN
				em_proceso.Text=""
			END IF
		END IF
		
	CASE 6
		lstr_Busq.Argum[1]	=	'6'	// Tipo Orden
		OpenWithParm(w_busqueda_doctointernopack, lstr_Busq)
		lstr_Busq	= Message.PowerObjectParm

		IF lstr_Busq.Argum[2] <> "" THEN
			em_proceso.text	=	lstr_busq.argum[3]
			IF NoExisteOrdenReProceso(Long(lstr_Busq.Argum[3]),6) THEN
				em_proceso.Text=""
			END IF
		END IF
	
END CHOOSE
end event

type cbx_todosproc from checkbox within w_info_trazabilidad_lotes
integer x = 695
integer y = 1000
integer width = 343
integer height = 72
integer taborder = 60
boolean bringtotop = true
integer textsize = -9
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

event clicked;String ls_Null
SetNull(ls_Null)

IF This.Checked THEN
	em_proceso.Text							=	''
	cb_buscaorden.Enabled					=	False
	em_proceso.Enabled						=	False
	
ELSE
	cb_buscaorden.Enabled					=	True
	em_proceso.Enabled						=	True
	
END IF








end event

type gb_5 from groupbox within w_info_trazabilidad_lotes
integer x = 265
integer y = 1648
integer width = 1614
integer height = 308
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

type st_4 from statictext within w_info_trazabilidad_lotes
integer x = 242
integer y = 428
integer width = 1664
integer height = 1556
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

type cbx_todasfec from checkbox within w_info_trazabilidad_lotes
integer x = 613
integer y = 1740
integer width = 343
integer height = 72
integer taborder = 130
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Todas"
boolean checked = true
end type

event clicked;String ls_Null
SetNull(ls_Null)

IF This.Checked THEN
	id_fecini					=	RelativeDate(Today(), - (365 * 7) )
	id_fecter					=	Today()
	em_desde.Text			=	String(RelativeDate(Today(), - (365 * 7) ) )
	em_hasta.Text			=	String(Today())
	em_desde.enabled 	= 	False
	em_hasta.enabled 	= 	False
ELSE
	em_desde.enabled 	= 	True
	em_hasta.enabled 	= 	True
END IF








end event

type cbx_todoscolo from checkbox within w_info_trazabilidad_lotes
integer x = 699
integer y = 1408
integer width = 402
integer height = 80
integer taborder = 110
boolean bringtotop = true
integer textsize = -9
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
	dw_colores.Enabled											=	False
	dw_colores.Object.lote_calibr.BackGround.Color		=	RGB(192, 192, 192)
	is_color															= 	'-1 '
ELSE
	dw_colores.Enabled											=	True
	dw_colores.Object.lote_calibr.BackGround.Color		=	RGB(255, 255, 255)
	dw_colores.SetFocus()
	SetNull(is_color)
END IF
end event

type dw_colores from datawindow within w_info_trazabilidad_lotes
integer x = 699
integer y = 1488
integer width = 1143
integer height = 92
integer taborder = 120
boolean bringtotop = true
boolean enabled = false
string title = "none"
string dataobject = "dddw_colores"
boolean border = false
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event itemchanged;is_color  = data
end event

event itemerror;RETURN 1
end event

type st_9 from statictext within w_info_trazabilidad_lotes
integer x = 302
integer y = 1292
integer width = 393
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
string text = "Variedad"
borderstyle borderstyle = stylelowered!
boolean focusrectangle = false
end type

type st_10 from statictext within w_info_trazabilidad_lotes
integer x = 302
integer y = 1496
integer width = 393
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
string text = "Color"
borderstyle borderstyle = stylelowered!
boolean focusrectangle = false
end type

type uo_selplantas from uo_seleccion_plantas within w_info_trazabilidad_lotes
event destroy ( )
integer x = 699
integer y = 676
integer height = 96
integer taborder = 60
boolean bringtotop = true
end type

on uo_selplantas.destroy
call uo_seleccion_plantas::destroy
end on

type uo_selclientes from uo_seleccion_clientesprod within w_info_trazabilidad_lotes
event destroy ( )
integer x = 699
integer y = 524
integer height = 96
integer taborder = 20
boolean bringtotop = true
end type

on uo_selclientes.destroy
call uo_seleccion_clientesprod::destroy
end on

type uo_selproductor from uo_seleccion_productor within w_info_trazabilidad_lotes
integer x = 699
integer y = 820
integer taborder = 50
boolean bringtotop = true
end type

on uo_selproductor.destroy
call uo_seleccion_productor::destroy
end on

type uo_selvariedad from uo_seleccion_variedad within w_info_trazabilidad_lotes
integer x = 690
integer y = 1192
integer taborder = 100
boolean bringtotop = true
end type

on uo_selvariedad.destroy
call uo_seleccion_variedad::destroy
end on

