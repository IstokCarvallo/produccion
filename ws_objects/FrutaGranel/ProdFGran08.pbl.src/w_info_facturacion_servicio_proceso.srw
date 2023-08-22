$PBExportHeader$w_info_facturacion_servicio_proceso.srw
$PBExportComments$Informe Existencia Sitio Revisión
forward
global type w_info_facturacion_servicio_proceso from w_para_informes
end type
type st_1 from statictext within w_info_facturacion_servicio_proceso
end type
type dw_cliente from datawindow within w_info_facturacion_servicio_proceso
end type
type st_6 from statictext within w_info_facturacion_servicio_proceso
end type
type dw_planta from datawindow within w_info_facturacion_servicio_proceso
end type
type st_8 from statictext within w_info_facturacion_servicio_proceso
end type
type dw_productor from datawindow within w_info_facturacion_servicio_proceso
end type
type cbx_productor from checkbox within w_info_facturacion_servicio_proceso
end type
type cbx_productorcons from checkbox within w_info_facturacion_servicio_proceso
end type
type cbx_planta from checkbox within w_info_facturacion_servicio_proceso
end type
type st_2 from statictext within w_info_facturacion_servicio_proceso
end type
type em_desde from editmask within w_info_facturacion_servicio_proceso
end type
type gb_3 from groupbox within w_info_facturacion_servicio_proceso
end type
type st_3 from statictext within w_info_facturacion_servicio_proceso
end type
type em_proceso from editmask within w_info_facturacion_servicio_proceso
end type
type cb_buscaorden from commandbutton within w_info_facturacion_servicio_proceso
end type
type cbx_todosproc from checkbox within w_info_facturacion_servicio_proceso
end type
type gb_4 from groupbox within w_info_facturacion_servicio_proceso
end type
type st_5 from statictext within w_info_facturacion_servicio_proceso
end type
type ddlb_tipoproc from dropdownlistbox within w_info_facturacion_servicio_proceso
end type
type cbx_consolproc from checkbox within w_info_facturacion_servicio_proceso
end type
type cbx_consolmesproc from checkbox within w_info_facturacion_servicio_proceso
end type
type st_7 from statictext within w_info_facturacion_servicio_proceso
end type
type dw_especie from datawindow within w_info_facturacion_servicio_proceso
end type
type cbx_todasesp from checkbox within w_info_facturacion_servicio_proceso
end type
type gb_5 from groupbox within w_info_facturacion_servicio_proceso
end type
type st_4 from statictext within w_info_facturacion_servicio_proceso
end type
type rb_1 from radiobutton within w_info_facturacion_servicio_proceso
end type
type rb_2 from radiobutton within w_info_facturacion_servicio_proceso
end type
end forward

global type w_info_facturacion_servicio_proceso from w_para_informes
integer x = 14
integer y = 32
integer width = 1970
integer height = 1852
string title = "CUADRATURA PRODUCTOR"
boolean minbox = false
boolean maxbox = false
boolean resizable = false
windowtype windowtype = response!
string icon = "F:\Desarrollo\Producción\FrutaProcesada\Producc.ico"
st_1 st_1
dw_cliente dw_cliente
st_6 st_6
dw_planta dw_planta
st_8 st_8
dw_productor dw_productor
cbx_productor cbx_productor
cbx_productorcons cbx_productorcons
cbx_planta cbx_planta
st_2 st_2
em_desde em_desde
gb_3 gb_3
st_3 st_3
em_proceso em_proceso
cb_buscaorden cb_buscaorden
cbx_todosproc cbx_todosproc
gb_4 gb_4
st_5 st_5
ddlb_tipoproc ddlb_tipoproc
cbx_consolproc cbx_consolproc
cbx_consolmesproc cbx_consolmesproc
st_7 st_7
dw_especie dw_especie
cbx_todasesp cbx_todasesp
gb_5 gb_5
st_4 st_4
rb_1 rb_1
rb_2 rb_2
end type
global w_info_facturacion_servicio_proceso w_info_facturacion_servicio_proceso

type variables
str_busqueda istr_busq
str_mant istr_mant

DataWindowChild	idwc_cliente, idwc_planta, idwc_productor
						

Integer 				ii_cliente, ii_planta, ii_productor, ii_TipoOrden, il_ppre_numero, ii_especie
String				 	is_Cliente, is_NomPlanta
Date					id_fecini, id_fecter		

uo_especie			iuo_Especie
DatawindowChild	idwc_especie	
end variables

forward prototypes
public function boolean existeproductor (long ll_productor)
public subroutine buscadescliente (integer ai_cliente)
public function boolean existeplanta (integer codigo)
public function boolean noexisteordenproceso (long al_orden)
public function boolean noexisteordenreproceso (long al_orden, integer ai_tipo)
end prototypes

public function boolean existeproductor (long ll_productor);String	ls_Nombre

SELECT	prod_nombre
	INTO	:ls_Nombre
	FROM	dba.productores
	WHERE	prod_codigo	=	:ll_Productor ;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de tabla Productor")
	
	RETURN False
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "Código de Productor no ha sido Definido.~r~r" + &
					"Ingrese o seleccione otro Código.")
	RETURN False
ELSE
	istr_mant.argumento[4] = String(ll_Productor)	
	RETURN True
END IF
end function

public subroutine buscadescliente (integer ai_cliente);
SELECT	clie_nombre
	INTO	:is_Cliente
	FROM	dba.clientesprod
	WHERE	clie_codigo	=	:ai_Cliente ;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, " Error en Lectura de tabla Clientes Producción")	
	is_Cliente = ''
ELSEIF sqlca.SQLCode = 100 THEN
	is_Cliente = ''
END IF
end subroutine

public function boolean existeplanta (integer codigo);String	plde_nombre

SELECT	plde_nombre INTO :plde_nombre
	FROM	dba.plantadesp
	WHERE	plde_codigo = :codigo ;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca,"Lectura de la Tabla Plantadesp")
	RETURN False
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox(	"Atención", "Código de Planta no ha sido creado.~r~nIngrese otro código", &
									Exclamation!, Ok!)
	RETURN False
ELSE
	istr_mant.argumento[3]	= String(Codigo)
	RETURN True
END IF
end function

public function boolean noexisteordenproceso (long al_orden);Integer	li_Especie, li_Planta, li_Estado, li_cliente, &
			li_Variedad, li_Seguimto, li_Linea, li_periodo
String	ls_Seguimiento[3]={'Productor','Huerto','Cuartel'} 
Date  	ldt_Fecha
Boolean	lb_Retorno = True
Long		ll_Productor

li_Planta	=	dw_Planta.Object.plde_codigo[1]
li_cliente 	= 	dw_cliente.Object.Clie_codigo[1]

IF isnull(li_Planta) THEN
	li_planta = -1
END IF	

SELECT	espe_codigo, orpr_estado, orpr_fecpro, prod_codigo,
			vari_codigo, orpr_niveld, line_codigo, pefr_codigo, 
			ppre_numero
	INTO	:li_Especie, :li_Estado, :ldt_Fecha, :ll_Productor, 
			:li_Variedad, :li_Seguimto, :li_Linea, :li_periodo, 
			:il_ppre_numero
	FROM	dba.spro_ordenproceso
	WHERE	:li_Planta in (-1,plde_codigo)
	AND	orpr_tipord	=	:ii_TipoOrden
	AND	orpr_numero	=	:al_Orden
	AND 	clie_codigo	= 	:li_cliente;
		 
IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla Spro_Ordenproceso")
ELSEIF sqlca.SQLCode = 0 THEN
		
	em_desde.text			=	String(ldt_Fecha,'mm/yyyy')

	lb_Retorno	=	False
END IF

RETURN lb_Retorno
end function

public function boolean noexisteordenreproceso (long al_orden, integer ai_tipo);Integer	li_Especie, li_Planta, li_Estado, li_Linea, li_Nula, li_cliente,&
			li_Variedad, li_Seguimto, li_periodo
Date  	ldt_Fecha
Boolean	lb_Retorno = True
Long		ll_Productor, ll_ppre_numero

SetNull(li_Nula)

li_Planta	=	dw_Planta.Object.plde_codigo[1]
li_cliente 	= 	dw_cliente.Object.Clie_codigo[1]

IF isnull(li_Planta) THEN
	li_planta = -1
END IF	

SELECT	espe_codigo, dinp_estado, dinp_fecdoc, line_codigo
	INTO	:li_Especie, :li_Estado, :ldt_Fecha, :li_Linea
	FROM	dba.spro_doctointernopack
	WHERE	:li_Planta in (-1,plde_codigo)
	AND	dinp_tipdoc	=	:ai_tipo
	AND	dinp_numero	=	:al_Orden;
		 
IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla spro_doctointernopack")
ELSEIF sqlca.SQLCode <> 100 THEN
	em_desde.text								=	String(Date(ldt_Fecha), 'mm/yyyy')
	lb_Retorno					=	FALSE
END IF

RETURN lb_Retorno
end function

on w_info_facturacion_servicio_proceso.create
int iCurrent
call super::create
this.st_1=create st_1
this.dw_cliente=create dw_cliente
this.st_6=create st_6
this.dw_planta=create dw_planta
this.st_8=create st_8
this.dw_productor=create dw_productor
this.cbx_productor=create cbx_productor
this.cbx_productorcons=create cbx_productorcons
this.cbx_planta=create cbx_planta
this.st_2=create st_2
this.em_desde=create em_desde
this.gb_3=create gb_3
this.st_3=create st_3
this.em_proceso=create em_proceso
this.cb_buscaorden=create cb_buscaorden
this.cbx_todosproc=create cbx_todosproc
this.gb_4=create gb_4
this.st_5=create st_5
this.ddlb_tipoproc=create ddlb_tipoproc
this.cbx_consolproc=create cbx_consolproc
this.cbx_consolmesproc=create cbx_consolmesproc
this.st_7=create st_7
this.dw_especie=create dw_especie
this.cbx_todasesp=create cbx_todasesp
this.gb_5=create gb_5
this.st_4=create st_4
this.rb_1=create rb_1
this.rb_2=create rb_2
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_1
this.Control[iCurrent+2]=this.dw_cliente
this.Control[iCurrent+3]=this.st_6
this.Control[iCurrent+4]=this.dw_planta
this.Control[iCurrent+5]=this.st_8
this.Control[iCurrent+6]=this.dw_productor
this.Control[iCurrent+7]=this.cbx_productor
this.Control[iCurrent+8]=this.cbx_productorcons
this.Control[iCurrent+9]=this.cbx_planta
this.Control[iCurrent+10]=this.st_2
this.Control[iCurrent+11]=this.em_desde
this.Control[iCurrent+12]=this.gb_3
this.Control[iCurrent+13]=this.st_3
this.Control[iCurrent+14]=this.em_proceso
this.Control[iCurrent+15]=this.cb_buscaorden
this.Control[iCurrent+16]=this.cbx_todosproc
this.Control[iCurrent+17]=this.gb_4
this.Control[iCurrent+18]=this.st_5
this.Control[iCurrent+19]=this.ddlb_tipoproc
this.Control[iCurrent+20]=this.cbx_consolproc
this.Control[iCurrent+21]=this.cbx_consolmesproc
this.Control[iCurrent+22]=this.st_7
this.Control[iCurrent+23]=this.dw_especie
this.Control[iCurrent+24]=this.cbx_todasesp
this.Control[iCurrent+25]=this.gb_5
this.Control[iCurrent+26]=this.st_4
this.Control[iCurrent+27]=this.rb_1
this.Control[iCurrent+28]=this.rb_2
end on

on w_info_facturacion_servicio_proceso.destroy
call super::destroy
destroy(this.st_1)
destroy(this.dw_cliente)
destroy(this.st_6)
destroy(this.dw_planta)
destroy(this.st_8)
destroy(this.dw_productor)
destroy(this.cbx_productor)
destroy(this.cbx_productorcons)
destroy(this.cbx_planta)
destroy(this.st_2)
destroy(this.em_desde)
destroy(this.gb_3)
destroy(this.st_3)
destroy(this.em_proceso)
destroy(this.cb_buscaorden)
destroy(this.cbx_todosproc)
destroy(this.gb_4)
destroy(this.st_5)
destroy(this.ddlb_tipoproc)
destroy(this.cbx_consolproc)
destroy(this.cbx_consolmesproc)
destroy(this.st_7)
destroy(this.dw_especie)
destroy(this.cbx_todasesp)
destroy(this.gb_5)
destroy(this.st_4)
destroy(this.rb_1)
destroy(this.rb_2)
end on

event open;String	ls_mes, ls_year

x	=	0
y	=	0

iuo_Especie	=	Create uo_especie

dw_cliente.GetChild("clie_codigo", idwc_cliente)
idwc_cliente.SetTransObject(SQLCA)
idwc_cliente.Retrieve()
dw_cliente.InsertRow(0)
dw_cliente.SetItem(1, "clie_codigo", gi_CodExport)

dw_planta.GetChild("plde_codigo", idwc_planta)
idwc_planta.SetTransObject(sqlca)
idwc_planta.Retrieve(1)
dw_planta.InsertRow(0)

dw_planta.Enabled												=	False
dw_planta.Object.plde_codigo.BackGround.Color			=	RGB(192, 192, 192)

dw_productor.GetChild("prod_codigo", idwc_productor)
idwc_productor.SetTransObject(sqlca)
idwc_productor.Retrieve()
dw_productor.InsertRow(0)

dw_productor.Enabled											=	False
dw_productor.Object.prod_codigo.BackGround.Color		=	RGB(192, 192, 192)

dw_especie.GetChild("espe_codigo", idwc_especie)
idwc_especie.SetTransObject(sqlca)
idwc_especie.Retrieve()
dw_especie.InsertRow(0)	

dw_especie.Enabled												=	False
dw_especie.Object.espe_codigo.BackGround.Color		=	RGB(192, 192, 192)

em_desde.Text				=	String(Today(), 'mm/yyyy')

ii_cliente 		= 	gi_CodExport
ii_planta			= 	-1
ii_productor		= 	-1
ii_especie 		= 	-1

ls_mes 			=	Left(em_desde.Text, 2)
ls_year			=	Right(em_desde.Text, 5)

id_fecini			= 	Date('01/' + em_desde.Text)
id_fecter			=	RelativeDate (Date('01/' + String(Integer(ls_mes) + 1)  + ls_year) , -1)

ii_TipoOrden	=	4

ddlb_tipoproc.Text 		= 'Proceso'
istr_mant.argumento[9]	=	'01/' + em_desde.Text
end event

event resize;//
end event

type st_titulo from w_para_informes`st_titulo within w_info_facturacion_servicio_proceso
integer x = 37
integer y = 64
integer width = 1618
string text = "Facturación Servicios"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_facturacion_servicio_proceso
integer x = 1742
integer y = 1288
integer height = 120
integer taborder = 130
end type

event pb_acepta::clicked;SetPointer(Arrow!)

Integer	fila, li_cliente, li_planta, li_especie, li_variedad, li_etiqueta, li_consplanta,&
			li_consproductor, li_consvariedad, li_consembalaje, li_consetiqueta,li_conscalidad,&
			li_conspacking, li_packing, li_zona
String	texto_desde, texto_hasta, texto_fecha, ls_null
Long		ll_Orden, ll_ConsOrden, ll_ConsFecha

SetNull(ls_null)

istr_info.titulo	= 'FACTURACION DE SERVICIOS PROCESO'	

BuscaDesCliente(ii_cliente)

OpenWithParm(vinf, istr_info)

IF rb_1.Checked THEN
	vinf.dw_1.DataObject = "dw_info_facturacion_servicios_proceso"
ELSE
	vinf.dw_1.DataObject = "dw_info_facturacion_servicios_resumen"
END IF

vinf.dw_1.SetTransObject(sqlca)

IF cbx_todosproc.Checked THEN
	ll_Orden = -1
	IF cbx_consolproc.Checked THEN
		ll_ConsOrden = -9
	ELSE
		ll_ConsOrden = 0
	END IF
ELSE
	ll_Orden		=	Long(em_Proceso.text)
	
	IF IsNull(ll_Orden) THEN
		MessageBox("Error de Datos", "Falta el Ingreso de un Número de Orden.")
		
		RETURN
	END IF	
END IF

IF cbx_consolproc.Checked THEN
	ll_ConsFecha = -9
END IF

IF IsNull(ii_TipoOrden) THEN
	MessageBox("Error de Datos", "Falta el Ingreso del Tipo de Proceso.")
	RETURN
END IF

texto_desde		=  f_fecha_texto(String(id_fecini), 1)
texto_hasta		=	f_fecha_texto(String(id_fecter), 1)
texto_fecha		=	"Desde El :  " + texto_desde + "   Hasta El :  " + texto_hasta

IF IsNull(ii_cliente) OR IsNull(ii_planta) OR IsNull(ii_productor) OR IsNull(id_fecini) &
	OR IsNull(id_fecter)THEN
		MessageBox("Error", "Debe Ingresar todos los parametros para el informe", StopSign!)
		return 1
END IF


IF rb_1.Checked THEN
	fila	=	vinf.dw_1.Retrieve(ii_cliente, ii_planta,ii_TipoOrden,ll_Orden, ii_productor, ii_especie,id_fecini,id_fecter,ll_ConsOrden, ll_ConsFecha, gstr_paramplanta.porcentajeiva )
ELSE
	fila	=	vinf.dw_1.Retrieve(ii_cliente, ii_planta,ii_TipoOrden,ll_Orden, ii_productor, ii_especie,id_fecini,id_fecter, gstr_paramplanta.porcentajeiva )
END IF

IF fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
			   	"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", &
	             StopSign!, Ok!)
ELSE
	F_Membrete(vinf.dw_1)
	vinf.dw_1.Modify('DataWindow.Print.Preview = Yes')
	vinf.dw_1.Modify('DataWindow.Print.Preview.Zoom = 75')
	vinf.Visible	= True
	vinf.Enabled	= True
END IF

SetPointer(Arrow!)				
end event

type pb_salir from w_para_informes`pb_salir within w_info_facturacion_servicio_proceso
integer x = 1742
integer y = 1552
integer taborder = 140
end type

type st_1 from statictext within w_info_facturacion_servicio_proceso
integer x = 96
integer y = 628
integer width = 238
integer height = 76
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
boolean enabled = false
string text = "Planta"
boolean focusrectangle = false
end type

type dw_cliente from datawindow within w_info_facturacion_servicio_proceso
integer x = 475
integer y = 436
integer width = 1143
integer height = 104
integer taborder = 10
boolean bringtotop = true
string dataobject = "dddw_clientesprod"
boolean border = false
boolean livescroll = true
end type

event itemchanged;String	ls_Columna[], ls_null

SetNull(ls_null)

IF ExisteCliente(Integer(data), ls_Columna[]) THEN
	istr_mant.argumento[1]	=	data
	idwc_planta.Retrieve(1)
	idwc_productor.Retrieve()	
	ii_cliente = integer(data)
ELSE
	This.SetItem(1, "clie_codigo", ls_null)
	RETURN 1
	SetNull(ii_cliente)
END IF
end event

event itemerror;RETURN 1
end event

type st_6 from statictext within w_info_facturacion_servicio_proceso
integer x = 101
integer y = 456
integer width = 233
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Cliente"
boolean focusrectangle = false
end type

type dw_planta from datawindow within w_info_facturacion_servicio_proceso
integer x = 475
integer y = 620
integer width = 1152
integer height = 92
integer taborder = 20
boolean bringtotop = true
boolean enabled = false
string title = "none"
string dataobject = "dddw_plantadesp_tipo"
boolean border = false
boolean livescroll = true
end type

event itemchanged;String	ls_Columna[], ls_null
Integer	li_Cliente

SetNull(ls_null)

li_Cliente	=	Integer(ii_cliente)

IF NOT Existeplanta(Integer(data)) THEN
	This.SetItem(1, "plde_codigo",Long(ls_null))
	RETURN 1
	SetNull(ii_planta)
ELSE
	istr_mant.Argumento[3] 	= 	Data
	ii_planta 						= 	Integer(data)
END IF
		
//IF ExistePlanta(li_Cliente, Integer(data), ls_Columna[]) THEN
//	istr_mant.argumento[3]	=	data
//	ii_planta 					= 	Integer(data)
//ELSE
//	This.SetItem(1, "plde_codigo", ls_null)
//	RETURN 1
//	SetNull(ii_planta)
//END IF
end event

event itemerror;RETURN 1
end event

type st_8 from statictext within w_info_facturacion_servicio_proceso
integer x = 101
integer y = 812
integer width = 329
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = "Productor"
boolean focusrectangle = false
end type

type dw_productor from datawindow within w_info_facturacion_servicio_proceso
integer x = 475
integer y = 800
integer width = 1065
integer height = 92
integer taborder = 50
boolean bringtotop = true
boolean enabled = false
string title = "none"
string dataobject = "dddw_productores"
boolean border = false
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event itemerror;RETURN 1
end event

event itemchanged;String	 ls_null
SetNull(ls_null)

IF ExisteProductor(Long(data)) THEN
	istr_mant.argumento[4]	=	data	
	ii_productor 				= 	Integer(data)
	RETURN 0
ELSE
	This.SetItem(1, "prod_codigo", ls_null)
	RETURN 1
	SetNull(ii_productor)
END IF
end event

type cbx_productor from checkbox within w_info_facturacion_servicio_proceso
integer x = 480
integer y = 720
integer width = 402
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = "Todos"
boolean checked = true
end type

event clicked;IF This.Checked THEN
	cbx_productorcons.Enabled									=	True
	dw_productor.Enabled											=	False
	dw_productor.Object.prod_codigo.BackGround.Color	=	RGB(192, 192, 192)
	istr_mant.argumento[4]										=	'0'
	istr_mant.argumento[14]										=	'0'
	ii_productor													= 	-1
ELSE
	cbx_productorcons.Enabled									=	False
	cbx_productorcons.Checked									=	False
	dw_productor.Enabled											=	True
	dw_productor.Object.prod_codigo.BackGround.Color	=	RGB(255, 255, 255)
	dw_productor.SetFocus()
	istr_mant.argumento[14]	=	'0'
	SetNull(ii_productor)
END IF
end event

type cbx_productorcons from checkbox within w_info_facturacion_servicio_proceso
boolean visible = false
integer x = 1006
integer y = 720
integer width = 471
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = "Consolidados"
end type

event clicked;IF This.Checked = TRUE THEN
	istr_mant.argumento[14]	=	'1'
	ii_productor				= 	-9
ELSE
	istr_mant.argumento[14]	=	'0'
	ii_productor				= 	-1
END IF
	
end event

type cbx_planta from checkbox within w_info_facturacion_servicio_proceso
integer x = 485
integer y = 536
integer width = 402
integer height = 76
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = "Todas"
boolean checked = true
end type

event clicked;IF This.Checked THEN
	dw_planta.Enabled											=	False
	dw_planta.Object.plde_codigo.BackGround.Color	=	RGB(192, 192, 192)
	istr_mant.argumento[3]									=	'0'
	istr_mant.argumento[13]									=	'0'
	ii_planta 													= 	-1
ELSE
	dw_planta.Enabled											=	True
	dw_planta.Object.plde_codigo.BackGround.Color	=	RGB(255, 255, 255)
	SetNull(ii_planta)
	dw_planta.SetFocus()
END IF
end event

type st_2 from statictext within w_info_facturacion_servicio_proceso
integer x = 101
integer y = 1592
integer width = 379
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Mes Proceso"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type em_desde from editmask within w_info_facturacion_servicio_proceso
integer x = 475
integer y = 1576
integer width = 375
integer height = 96
integer taborder = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 16777215
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "mm/yyyy"
end type

event modified;String	ls_mes, ls_year

ls_mes 						=	Left(This.Text, 2)
ls_year						=	Right(This.Text, 5)

istr_mant.argumento[9]	=	'01/' + This.Text
id_fecini						= 	Date('01/' + this.Text)
id_fecter						=	RelativeDate (Date('01/' + String(Integer(ls_mes) + 1) + ls_year) , -1)
end event

type gb_3 from groupbox within w_info_facturacion_servicio_proceso
integer x = 78
integer y = 1096
integer width = 1531
integer height = 400
integer taborder = 150
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Proceso"
end type

type st_3 from statictext within w_info_facturacion_servicio_proceso
integer x = 101
integer y = 1352
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
string text = "Nº Proceso"
borderstyle borderstyle = stylelowered!
boolean focusrectangle = false
end type

type em_proceso from editmask within w_info_facturacion_servicio_proceso
integer x = 475
integer y = 1348
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

type cb_buscaorden from commandbutton within w_info_facturacion_servicio_proceso
integer x = 864
integer y = 1352
integer width = 91
integer height = 76
integer taborder = 70
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

type cbx_todosproc from checkbox within w_info_facturacion_servicio_proceso
integer x = 974
integer y = 1352
integer width = 251
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
	em_proceso.Text							=	''
	cb_buscaorden.Enabled					=	False
	em_proceso.Enabled						=	False
	cbx_consolproc.Enabled					=	True
ELSE
	cb_buscaorden.Enabled					=	True
	em_proceso.Enabled						=	True
	cbx_consolproc.Enabled					=	False
	cbx_consolproc.Checked					=	False
END IF








end event

type gb_4 from groupbox within w_info_facturacion_servicio_proceso
integer x = 78
integer y = 1504
integer width = 1531
integer height = 200
integer taborder = 160
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
end type

type st_5 from statictext within w_info_facturacion_servicio_proceso
integer x = 101
integer y = 1208
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

type ddlb_tipoproc from dropdownlistbox within w_info_facturacion_servicio_proceso
integer x = 475
integer y = 1192
integer width = 640
integer height = 368
integer taborder = 40
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 1090519039
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
		em_desde.Enabled			=	True
		//em_hasta.Enabled			=	True
		//em_hasta.Visible			=	True
		st_2.Visible			=	True
		cbx_todosproc.Checked	=	True
		cbx_todosproc.TriggerEvent(Clicked!)
		
	CASE 2,3,4
		ii_TipoOrden	=	index + 2
		em_proceso.Enabled		=	True
		em_proceso.Text			=	''
		cb_buscaorden.Enabled	=	True
		cbx_todosproc.Checked	=	False
		cbx_todosproc.Enabled	=	True
		//em_hasta.Visible			=	False
		//st_2.Visible				=	False
		cbx_todosproc.TriggerEvent(Clicked!)
		
	CASE 5
		ii_TipoOrden				=	8
		em_proceso.Enabled		=	True
		em_proceso.Text			=	''
		cb_buscaorden.Enabled	=	True
		cbx_todosproc.Checked	=	False
		cbx_todosproc.Enabled	=	True
		//em_hasta.Visible			=	False
		//st_2.Visible			=	False
		cbx_todosproc.TriggerEvent(Clicked!)				
END CHOOSE
end event

type cbx_consolproc from checkbox within w_info_facturacion_servicio_proceso
integer x = 1248
integer y = 1352
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
string text = "Consolida"
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

type cbx_consolmesproc from checkbox within w_info_facturacion_servicio_proceso
integer x = 1248
integer y = 1588
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
string text = "Consolida"
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

type st_7 from statictext within w_info_facturacion_servicio_proceso
integer x = 101
integer y = 984
integer width = 329
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = "Especie"
boolean focusrectangle = false
end type

type dw_especie from datawindow within w_info_facturacion_servicio_proceso
integer x = 475
integer y = 972
integer width = 878
integer height = 92
integer taborder = 130
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_especies"
boolean border = false
boolean livescroll = true
end type

event itemchanged;Integer li_Nula

SetNull(li_Nula)

IF dwo.Name = 'espe_codigo' THEN
	IF iuo_Especie.Existe(Integer(data),True,SQLCA) THEN
		
		ii_especie = Integer(data)
	ELSE
		This.SetItem(row,'espe_codigo',li_Nula)
		RETURN 1
	END IF
END IF
end event

event itemerror;RETURN 1
end event

type cbx_todasesp from checkbox within w_info_facturacion_servicio_proceso
integer x = 480
integer y = 900
integer width = 283
integer height = 72
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Todas"
boolean checked = true
end type

event clicked;String ls_Null

SetNull(ls_Null)

IF This.Checked THEN
	dw_especie.Object.espe_codigo.Protect						=	1
	dw_especie.Object.espe_codigo.BackGround.Color		=	RGB(192,192,192)
	dw_especie.SetItem(1,"espe_codigo",Integer(ls_Null))
	ii_especie 															= 	-1
	dw_especie.Enabled												=	False
ELSE
	dw_especie.Object.espe_codigo.Protect						=	0
	dw_especie.Object.espe_codigo.BackGround.Color		=	RGB(255,255,255)
	dw_especie.Enabled												=	True
	
	dw_especie.GetChild("espe_codigo", idwc_especie)
	idwc_especie.SetTransObject(sqlca)
	idwc_especie.Retrieve()
	SetNull(ii_especie)
END IF

end event

type gb_5 from groupbox within w_info_facturacion_servicio_proceso
integer x = 78
integer y = 200
integer width = 1531
integer height = 228
integer taborder = 10
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Informe"
end type

type st_4 from statictext within w_info_facturacion_servicio_proceso
integer x = 37
integer y = 192
integer width = 1618
integer height = 1552
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type rb_1 from radiobutton within w_info_facturacion_servicio_proceso
integer x = 343
integer y = 292
integer width = 402
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Detallado"
boolean checked = true
end type

type rb_2 from radiobutton within w_info_facturacion_servicio_proceso
integer x = 850
integer y = 292
integer width = 402
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Resumen"
end type

