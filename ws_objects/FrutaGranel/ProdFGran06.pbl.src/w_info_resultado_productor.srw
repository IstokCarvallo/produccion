$PBExportHeader$w_info_resultado_productor.srw
$PBExportComments$Resultado de Proceso Kilos
forward
global type w_info_resultado_productor from w_para_informes
end type
type cb_buscaorden from commandbutton within w_info_resultado_productor
end type
type em_fecdes from editmask within w_info_resultado_productor
end type
type st_fecha from statictext within w_info_resultado_productor
end type
type em_proceso from editmask within w_info_resultado_productor
end type
type st_7 from statictext within w_info_resultado_productor
end type
type st_6 from statictext within w_info_resultado_productor
end type
type dw_planta from datawindow within w_info_resultado_productor
end type
type st_2 from statictext within w_info_resultado_productor
end type
type ddlb_tipoproc from dropdownlistbox within w_info_resultado_productor
end type
type cbx_todosproc from checkbox within w_info_resultado_productor
end type
type em_fecter from editmask within w_info_resultado_productor
end type
type cbx_consfecha from checkbox within w_info_resultado_productor
end type
type st_12 from statictext within w_info_resultado_productor
end type
type dw_contratista from datawindow within w_info_resultado_productor
end type
type st_1 from statictext within w_info_resultado_productor
end type
type cbx_todcont from checkbox within w_info_resultado_productor
end type
type cbx_concont from checkbox within w_info_resultado_productor
end type
type dw_2 from datawindow within w_info_resultado_productor
end type
type st_3 from statictext within w_info_resultado_productor
end type
type cbx_todfec from checkbox within w_info_resultado_productor
end type
type gb_3 from groupbox within w_info_resultado_productor
end type
type st_fecter from statictext within w_info_resultado_productor
end type
type st_9 from statictext within w_info_resultado_productor
end type
type st_4 from statictext within w_info_resultado_productor
end type
type dw_productor from datawindow within w_info_resultado_productor
end type
end forward

global type w_info_resultado_productor from w_para_informes
integer width = 2190
integer height = 1048
string title = "Informe Facturación Servicios"
cb_buscaorden cb_buscaorden
em_fecdes em_fecdes
st_fecha st_fecha
em_proceso em_proceso
st_7 st_7
st_6 st_6
dw_planta dw_planta
st_2 st_2
ddlb_tipoproc ddlb_tipoproc
cbx_todosproc cbx_todosproc
em_fecter em_fecter
cbx_consfecha cbx_consfecha
st_12 st_12
dw_contratista dw_contratista
st_1 st_1
cbx_todcont cbx_todcont
cbx_concont cbx_concont
dw_2 dw_2
st_3 st_3
cbx_todfec cbx_todfec
gb_3 gb_3
st_fecter st_fecter
st_9 st_9
st_4 st_4
dw_productor dw_productor
end type
global w_info_resultado_productor w_info_resultado_productor

type variables
DataWindowChild	idwc_Planta, idwc_linea, idwc_variedad, idwc_especie,idwc_cliente,idwc_productor,idwc_progproc,idwc_contratista
uo_plantadesp		iuo_Planta
uo_especie			iuo_Especie
uo_Variedades		iuo_Variedad
uo_productores		iuo_productor


Integer    ii_TipoOrden,ii_cliente, ii_planta, ii_especie, il_ppre_numero
Long il_programa, il_productor
end variables

forward prototypes
public function boolean noexisteordenreproceso (long al_orden, integer ai_tipo)
public function boolean noexisteordenproceso (long al_orden)
public function boolean existecontratista (integer ai_codigo)
end prototypes

public function boolean noexisteordenreproceso (long al_orden, integer ai_tipo);Integer	li_Especie, li_Planta, li_Estado, li_Linea, li_Nula, li_cliente,&
			li_Variedad, li_Seguimto, li_periodo
Date  	ldt_Fecha
Boolean	lb_Retorno = True
Long		ll_Productor, ll_ppre_numero

SetNull(li_Nula)

li_Planta	=	dw_Planta.Object.plde_codigo[1]
li_cliente 	= 	dw_2.Object.Clie_codigo[1]

SELECT	espe_codigo, dinp_estado, dinp_fecdoc, line_codigo
	INTO	:li_Especie, :li_Estado, :ldt_Fecha, :li_Linea
	FROM	dba.spro_doctointernopack
	WHERE	plde_codigo	=	:li_Planta
	AND	dinp_tipdoc	=	:ai_tipo
	AND	dinp_numero	=	:al_Orden;
		 
IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla spro_doctointernopack")
ELSEIF sqlca.SQLCode <> 100 THEN
	em_fecdes.text								=	String(Date(ldt_Fecha))
	em_fecter.text								=	String(Date(ldt_Fecha))
	em_fecdes.Enabled			=	FALSE
	lb_Retorno					=	FALSE
END IF

RETURN lb_Retorno
end function

public function boolean noexisteordenproceso (long al_orden);Integer	li_Especie, li_Planta, li_Estado, li_cliente, &
			li_Variedad, li_Seguimto, li_Linea, li_periodo
String	ls_Seguimiento[3]={'Productor','Huerto','Cuartel'} 
Date  	ldt_Fecha
Boolean	lb_Retorno = True
Long		ll_Productor

li_Planta	=	dw_Planta.Object.plde_codigo[1]
li_cliente 	= 	dw_2.Object.Clie_codigo[1]

SELECT	espe_codigo, orpr_estado, orpr_fecpro, prod_codigo,
			vari_codigo, orpr_niveld, line_codigo, pefr_codigo, 
			ppre_numero
	INTO	:li_Especie, :li_Estado, :ldt_Fecha, :ll_Productor, 
			:li_Variedad, :li_Seguimto, :li_Linea, :li_periodo, 
			:il_ppre_numero
	FROM	dba.spro_ordenproceso
	WHERE	plde_codigo	=	:li_Planta
	AND	orpr_tipord	=	:ii_TipoOrden
	AND	orpr_numero	=	:al_Orden
	AND 	clie_codigo	= 	:li_cliente;
		 
IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla Spro_Ordenproceso")
ELSEIF sqlca.SQLCode = 0 THEN
		
	em_fecdes.text			=	String(ldt_Fecha,'dd/mm/yyyy')
	em_fecter.text			=	String(ldt_Fecha,'dd/mm/yyyy')

	lb_Retorno	=	False
END IF

RETURN lb_Retorno
end function

public function boolean existecontratista (integer ai_codigo);Long		ll_count
Boolean	lb_Retorno = True

SELECT	Count()
	INTO	:ll_count
	FROM	dba.contratista
	WHERE	cont_codigo = :ai_codigo;
		 
IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla Contratista")
	lb_Retorno = False
ELSEIF ll_count = 0 THEN
	MessageBox("Atención", "No existe Contratista, ~nIngrese Otro.", Exclamation!)
	lb_Retorno = False
END IF	

Return lb_retorno
	
end function

on w_info_resultado_productor.create
int iCurrent
call super::create
this.cb_buscaorden=create cb_buscaorden
this.em_fecdes=create em_fecdes
this.st_fecha=create st_fecha
this.em_proceso=create em_proceso
this.st_7=create st_7
this.st_6=create st_6
this.dw_planta=create dw_planta
this.st_2=create st_2
this.ddlb_tipoproc=create ddlb_tipoproc
this.cbx_todosproc=create cbx_todosproc
this.em_fecter=create em_fecter
this.cbx_consfecha=create cbx_consfecha
this.st_12=create st_12
this.dw_contratista=create dw_contratista
this.st_1=create st_1
this.cbx_todcont=create cbx_todcont
this.cbx_concont=create cbx_concont
this.dw_2=create dw_2
this.st_3=create st_3
this.cbx_todfec=create cbx_todfec
this.gb_3=create gb_3
this.st_fecter=create st_fecter
this.st_9=create st_9
this.st_4=create st_4
this.dw_productor=create dw_productor
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.cb_buscaorden
this.Control[iCurrent+2]=this.em_fecdes
this.Control[iCurrent+3]=this.st_fecha
this.Control[iCurrent+4]=this.em_proceso
this.Control[iCurrent+5]=this.st_7
this.Control[iCurrent+6]=this.st_6
this.Control[iCurrent+7]=this.dw_planta
this.Control[iCurrent+8]=this.st_2
this.Control[iCurrent+9]=this.ddlb_tipoproc
this.Control[iCurrent+10]=this.cbx_todosproc
this.Control[iCurrent+11]=this.em_fecter
this.Control[iCurrent+12]=this.cbx_consfecha
this.Control[iCurrent+13]=this.st_12
this.Control[iCurrent+14]=this.dw_contratista
this.Control[iCurrent+15]=this.st_1
this.Control[iCurrent+16]=this.cbx_todcont
this.Control[iCurrent+17]=this.cbx_concont
this.Control[iCurrent+18]=this.dw_2
this.Control[iCurrent+19]=this.st_3
this.Control[iCurrent+20]=this.cbx_todfec
this.Control[iCurrent+21]=this.gb_3
this.Control[iCurrent+22]=this.st_fecter
this.Control[iCurrent+23]=this.st_9
this.Control[iCurrent+24]=this.st_4
this.Control[iCurrent+25]=this.dw_productor
end on

on w_info_resultado_productor.destroy
call super::destroy
destroy(this.cb_buscaorden)
destroy(this.em_fecdes)
destroy(this.st_fecha)
destroy(this.em_proceso)
destroy(this.st_7)
destroy(this.st_6)
destroy(this.dw_planta)
destroy(this.st_2)
destroy(this.ddlb_tipoproc)
destroy(this.cbx_todosproc)
destroy(this.em_fecter)
destroy(this.cbx_consfecha)
destroy(this.st_12)
destroy(this.dw_contratista)
destroy(this.st_1)
destroy(this.cbx_todcont)
destroy(this.cbx_concont)
destroy(this.dw_2)
destroy(this.st_3)
destroy(this.cbx_todfec)
destroy(this.gb_3)
destroy(this.st_fecter)
destroy(this.st_9)
destroy(this.st_4)
destroy(this.dw_productor)
end on

event open;X	=	0
Y	=	0


dw_2.GetChild("clie_codigo", idwc_cliente)

idwc_cliente.SetTransObject(SQLCA)
idwc_cliente.Retrieve()

dw_2.SetTransObject(SQLCA)
dw_2.InsertRow(0)
dw_2.SetItem(1, "clie_codigo", gi_codexport)
ii_cliente = gi_codexport

dw_planta.GetChild("plde_codigo",idwc_planta)
idwc_planta.SetTransObject(SQLCA)

IF idwc_planta.Retrieve()=0 THEN
	idwc_planta.InsertRow(0)
END IF

dw_planta.SetTransObject(SQLCA)
dw_planta.InsertRow(0)
dw_planta.SetItem(1, "plde_codigo", gstr_paramplanta.CodigoPlanta)
ii_planta = gstr_paramplanta.CodigoPlanta

dw_contratista.GetChild("clie_codigo", idwc_contratista)
idwc_contratista.SetTransObject(SQLCA)
idwc_contratista.Retrieve()
dw_contratista.SetTransObject(SQLCA)
dw_contratista.InsertRow(0)

iuo_productor	=	Create uo_productores
dw_productor.GetChild("prod_codigo",idwc_productor)
idwc_productor.SetTransObject(sqlca)
idwc_productor.Retrieve()
dw_productor.InsertRow(0)

em_fecdes.text	=	String(Date(Today()),'dd/mm/yyyy')
em_fecter.text	=	String(Date(Today()),'dd/mm/yyyy')

ii_TipoOrden	=	4

ddlb_tipoproc.Text = 'Proceso'

Boolean	lb_Cerrar
end event

type st_titulo from w_para_informes`st_titulo within w_info_resultado_productor
integer x = 41
integer y = 28
integer width = 1755
string text = "Informe Facturación Servicios"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_resultado_productor
integer x = 1893
integer y = 448
integer taborder = 80
end type

event pb_acepta::clicked;SetPointer(HourGlass!)

Long		ll_Fila, ll_Orden, ll_Productor
Integer  	li_Planta, li_Especie, li_Variedad, &
			li_ConsPredio, li_TipoInforme, li_Linea, li_Periodo, li_ConsFecha, li_contratista
Date		ld_fecdes,ld_fecter
Dec{2}   	ld_peso

li_Planta	=	dw_Planta.Object.plde_codigo[1]
ld_Fecdes	=	Date(left(em_fecdes.Text, 10))
ld_Fecter	=	Date(left(em_fecter.Text, 10))
ii_cliente	=	dw_2.Object.clie_codigo[1]

IF IsNull(li_planta) THEN
	MessageBox("Error de Datos", "Falta el Ingreso de Planta.")
	RETURN
END IF

IF IsNull(ii_TipoOrden) THEN
	MessageBox("Error de Datos", "Falta el Ingreso del Tipo de Proceso.")
	RETURN
END IF

IF ii_TipoOrden = 0 or cbx_todosproc.Checked THEN
	ll_Orden = -1
ELSE
	ll_Orden		=	Long(em_Proceso.text)
	
	IF IsNull(ll_Orden) THEN
		MessageBox("Error de Datos", "Falta el Ingreso de un Número de Orden.")
		
		RETURN
	END IF	
END IF

IF cbx_todfec.Checked THEN
	ld_fecdes = Date('19000101')
	ld_fecter = Date(Today())
ELSEIF cbx_consfecha.Checked THEN
	li_ConsFecha = 1 
END IF

IF cbx_todcont.Checked THEN
	li_contratista = -1
ELSEIF cbx_concont.Checked THEN
	li_contratista = -9
ELSE	
	IF IsNull(dw_contratista.Object.cont_codigo[1]) THEN
		MessageBox("Error de Datos", "Falta el Ingreso de un Contratista.")
		RETURN
	ELSE
		li_contratista = dw_contratista.Object.cont_codigo[1]
	END IF
	
END IF

istr_info.Titulo		=	"Facturación de Servicios Productor"

li_Especie			=	-1
li_Variedad			=	-1
li_Linea				=	-1
li_Periodo			=	-1

istr_info.Copias	=	1

OpenWithParm(vinf, istr_info)

Vinf.dw_1.DataObject =	"dw_info_procesokilos_productor"
	
Vinf.dw_1.SetTransObject(sqlca)
ll_Fila	=	vinf.dw_1.Retrieve(ii_cliente,li_planta,il_productor,ld_fecdes,ld_fecter)

 
IF ll_Fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF ll_Fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", &
					StopSign!, Ok!)
ELSE
	F_Membrete(vinf.dw_1)
	
	vinf.dw_1.Modify('DataWindow.Print.Preview = Yes')
	vinf.dw_1.Modify('DataWindow.Print.Preview.Zoom = 100')
	ParamTemporada(gstr_paramtempo)
	vinf.dw_1.Modify("nombre_temporada.text = '" + gstr_paramtempo.nombre + "'")
	
	vinf.Visible	=	True
	vinf.Enabled	=	True
END IF

SetPointer(Arrow!)
end event

type pb_salir from w_para_informes`pb_salir within w_info_resultado_productor
integer x = 1893
integer y = 712
integer taborder = 90
end type

type cb_buscaorden from commandbutton within w_info_resultado_productor
boolean visible = false
integer x = 3072
integer y = 844
integer width = 91
integer height = 76
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
		
	CASE 4
		
		lstr_busq.argum[1]	=	String(dw_planta.Object.plde_codigo[1])
		lstr_busq.argum[2]	=	"1"
		lstr_busq.argum[3]	=	string(ii_TipoOrden)
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
		
		lstr_Busq.Argum[1]	=	'5'	// Tipo Orden

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

type em_fecdes from editmask within w_info_resultado_productor
integer x = 503
integer y = 760
integer width = 384
integer height = 84
integer taborder = 60
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16777215
string text = "none"
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
end type

type st_fecha from statictext within w_info_resultado_productor
integer x = 110
integer y = 768
integer width = 347
integer height = 76
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Fecha Inicio"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type em_proceso from editmask within w_info_resultado_productor
boolean visible = false
integer x = 2395
integer y = 760
integer width = 384
integer height = 84
integer taborder = 50
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 1090519039
boolean enabled = false
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

type st_7 from statictext within w_info_resultado_productor
boolean visible = false
integer x = 2263
integer y = 600
integer width = 347
integer height = 72
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
boolean enabled = false
string text = "Nº Proceso"
borderstyle borderstyle = stylelowered!
boolean focusrectangle = false
end type

type st_6 from statictext within w_info_resultado_productor
integer x = 110
integer y = 304
integer width = 347
integer height = 68
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
boolean enabled = false
string text = "Planta"
boolean focusrectangle = false
end type

type dw_planta from datawindow within w_info_resultado_productor
integer x = 507
integer y = 292
integer width = 882
integer height = 92
integer taborder = 20
boolean bringtotop = true
boolean enabled = false
string title = "none"
string dataobject = "dddw_planta"
boolean maxbox = true
boolean border = false
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event itemchanged;IF data <> '' THEN
	IF idwc_Planta.Find("plde_codigo = " + data, 1, idwc_Planta.RowCount()) = 0 THEN
		MessageBox("Atención", "Código de Planta indicado no ha sido~r" + &
						"creado en tabla respectiva.~r~rIngrese o seleccione" + &
						"otra Planta.")
		
		RETURN 1
	ELSE
		ii_planta = Integer(Data)
		IF idwc_linea.Retrieve(Integer(data)) = 0 THEN
			idwc_linea.InsertRow(0)
		END IF
	END IF
END IF
end event

type st_2 from statictext within w_info_resultado_productor
boolean visible = false
integer x = 2871
integer y = 436
integer width = 347
integer height = 72
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
boolean enabled = false
string text = "Tipo Proceso"
boolean focusrectangle = false
end type

type ddlb_tipoproc from dropdownlistbox within w_info_resultado_productor
boolean visible = false
integer x = 2409
integer y = 140
integer width = 640
integer height = 368
integer taborder = 30
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 1090519039
boolean enabled = false
string text = "none"
boolean sorted = false
boolean vscrollbar = true
string item[] = {"Todos","Proceso","Re-Proceso","Re-Embalaje","Pre-Proceso"}
borderstyle borderstyle = stylelowered!
end type

event selectionchanged;CHOOSE CASE index
		
	CASE 1
		ii_TipoOrden				=	-1
		em_proceso.Enabled		=	False
		cb_buscaorden.Enabled	=	False
		cbx_todosproc.Enabled	=	False
		em_fecdes.Enabled			=	True
		em_fecter.Enabled			=	True
		em_fecter.Visible			=	True
		st_fecter.Visible			=	True
		cbx_todosproc.Checked	=	True
		cbx_todosproc.TriggerEvent(Clicked!)
		
	CASE 2,3,4
		ii_TipoOrden	=	index + 2
		em_proceso.Enabled		=	True
		em_proceso.Text			=	''
		cb_buscaorden.Enabled	=	True
		cbx_todosproc.Checked	=	False
		cbx_todosproc.Enabled	=	True
		em_fecter.Visible			=	False
		st_fecter.Visible			=	False
		cbx_todosproc.TriggerEvent(Clicked!)
		
	CASE 5
		ii_TipoOrden				=	8
		em_proceso.Enabled		=	True
		em_proceso.Text			=	''
		cb_buscaorden.Enabled	=	True
		cbx_todosproc.Checked	=	False
		cbx_todosproc.Enabled	=	True
		em_fecter.Visible			=	False
		st_fecter.Visible			=	False
		cbx_todosproc.TriggerEvent(Clicked!)				
END CHOOSE
end event

type cbx_todosproc from checkbox within w_info_resultado_productor
boolean visible = false
integer x = 521
integer y = 440
integer width = 343
integer height = 72
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Todos"
boolean checked = true
end type

event clicked;String ls_Null
SetNull(ls_Null)

IF This.Checked THEN
	dw_productor.enabled = False
	dw_productor.object.prod_codigo[1] = Integer(ls_null)
	
ELSE
	cb_buscaorden.Enabled					=	True
	em_proceso.Enabled						=	True
	
END IF








end event

type em_fecter from editmask within w_info_resultado_productor
integer x = 1321
integer y = 760
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
long backcolor = 16777215
string text = "none"
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
end type

type cbx_consfecha from checkbox within w_info_resultado_productor
boolean visible = false
integer x = 1321
integer y = 652
integer width = 361
integer height = 96
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Consolida"
end type

event clicked;IF This.Checked THEN
	em_fecdes.Enabled 	  = False
	em_fecter.Enabled 	  = False
	cbx_todfec.Checked 	  = False	
ELSE
	em_fecdes.Enabled 	  = True
	em_fecter.Enabled 	  = True
	cbx_todfec.Checked 	  = False	
END IF
end event

type st_12 from statictext within w_info_resultado_productor
integer x = 110
integer y = 184
integer width = 347
integer height = 68
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
boolean enabled = false
string text = "Cliente"
boolean focusrectangle = false
end type

type dw_contratista from datawindow within w_info_resultado_productor
boolean visible = false
integer x = 2565
integer y = 976
integer width = 882
integer height = 92
integer taborder = 40
boolean bringtotop = true
boolean enabled = false
string title = "none"
string dataobject = "dddw_contratista"
boolean border = false
boolean livescroll = true
end type

event itemchanged;Integer li_null

SetNull(li_null)

IF Not existecontratista(Integer(Data)) THEN
	This.SetItem(Row,'cont_codigo', li_Null)
	RETURN 1
END IF	
end event

event itemerror;Return 1
end event

type st_1 from statictext within w_info_resultado_productor
boolean visible = false
integer x = 2656
integer y = 260
integer width = 347
integer height = 72
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
boolean enabled = false
string text = "Contratista"
boolean focusrectangle = false
end type

type cbx_todcont from checkbox within w_info_resultado_productor
boolean visible = false
integer x = 3072
integer y = 540
integer width = 288
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
boolean enabled = false
string text = "Todos"
boolean checked = true
end type

event clicked;IF This.Checked THEN
	dw_contratista.Enabled = False
	cbx_concont.Checked 	  = False
	dw_contratista.Reset()
	dw_contratista.SetTransObject(SQLCA)
	dw_contratista.InsertRow(0)
ELSE
	dw_contratista.Reset()
	dw_contratista.SetTransObject(SQLCA)
	dw_contratista.InsertRow(0)
	dw_contratista.Enabled = True
	cbx_concont.Checked 	  = False
END IF
end event

type cbx_concont from checkbox within w_info_resultado_productor
boolean visible = false
integer x = 2715
integer y = 620
integer width = 370
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
boolean enabled = false
string text = "Consolida"
end type

event clicked;IF This.Checked THEN
	dw_contratista.Enabled = False
	cbx_todcont.Checked 	  = False	
	dw_contratista.Reset()
	dw_contratista.SetTransObject(SQLCA)
	dw_contratista.InsertRow(0)
ELSE
	dw_contratista.Enabled = True
	cbx_todcont.Checked 	  = False
	dw_contratista.Reset()
	dw_contratista.SetTransObject(SQLCA)
	dw_contratista.InsertRow(0)
END IF
end event

type dw_2 from datawindow within w_info_resultado_productor
integer x = 507
integer y = 176
integer width = 1175
integer height = 92
integer taborder = 10
string dataobject = "dddw_clientesprod"
boolean border = false
boolean livescroll = true
end type

event itemchanged;ii_cliente = idwc_cliente.GetItemNumber(idwc_cliente.GetRow(),"clie_codigo")

IF idwc_planta.Retrieve(ii_cliente)=0 THEN
	idwc_planta.InsertRow(0)
END IF



end event

type st_3 from statictext within w_info_resultado_productor
integer x = 41
integer y = 132
integer width = 1755
integer height = 292
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
boolean enabled = false
alignment alignment = center!
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type cbx_todfec from checkbox within w_info_resultado_productor
integer x = 503
integer y = 652
integer width = 402
integer height = 96
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Todas"
end type

event clicked;IF This.Checked THEN
	em_fecdes.Enabled 	  = False
	cbx_consfecha.Checked  = False
	em_fecter.Enabled 	  = False
ELSE
	em_fecdes.Enabled 	  = True
	em_fecter.Enabled 	  = True
	cbx_consfecha.Checked  = False	
END IF
end event

type gb_3 from groupbox within w_info_resultado_productor
integer x = 73
integer y = 596
integer width = 1682
integer height = 276
integer taborder = 60
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
end type

type st_fecter from statictext within w_info_resultado_productor
integer x = 914
integer y = 772
integer width = 402
integer height = 72
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Fecha Término"
boolean focusrectangle = false
end type

type st_9 from statictext within w_info_resultado_productor
integer x = 41
integer y = 428
integer width = 1755
integer height = 472
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
boolean enabled = false
alignment alignment = center!
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

event clicked;//IF This.Checked THEN
//	dw_contratista.Enabled = False
//	cbx_concont.Checked 	  = False	
//ELSE
//	dw_contratista.Enabled = True
//	cbx_concont.Checked 	  = False
//END IF
end event

type st_4 from statictext within w_info_resultado_productor
integer x = 110
integer y = 528
integer width = 347
integer height = 68
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
boolean enabled = false
string text = "Productor"
boolean focusrectangle = false
end type

type dw_productor from datawindow within w_info_resultado_productor
integer x = 507
integer y = 516
integer width = 896
integer height = 92
integer taborder = 90
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_productores"
boolean border = false
boolean livescroll = true
end type

event itemchanged;Integer	li_nula
String		ls_columna

SetNull(li_nula)

ls_columna	=	dwo.name

CHOOSE CASE ls_columna
	CASE "prod_codigo"
		IF NOT iuo_productor.Existe(Long(data), true, sqlca) THEN
			il_productor	=	-1
			cbx_todosproc.Checked	=	TRUE
			cbx_todosproc.PostEvent(clicked!)
		ELSE
			il_productor	=	Long(data)
		END IF
		
END CHOOSE
end event

