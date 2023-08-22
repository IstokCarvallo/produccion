$PBExportHeader$w_info_numerales_porcaja.srw
forward
global type w_info_numerales_porcaja from w_para_informes
end type
type gb_7 from groupbox within w_info_numerales_porcaja
end type
type gb_4 from groupbox within w_info_numerales_porcaja
end type
type st_2 from statictext within w_info_numerales_porcaja
end type
type mle_1 from multilineedit within w_info_numerales_porcaja
end type
type st_variedad from statictext within w_info_numerales_porcaja
end type
type st_4 from statictext within w_info_numerales_porcaja
end type
type st_8 from statictext within w_info_numerales_porcaja
end type
type st_10 from statictext within w_info_numerales_porcaja
end type
type dw_cliente from datawindow within w_info_numerales_porcaja
end type
type cbx_planta from checkbox within w_info_numerales_porcaja
end type
type dw_planta from datawindow within w_info_numerales_porcaja
end type
type dw_tipoprod from datawindow within w_info_numerales_porcaja
end type
type dw_productor from datawindow within w_info_numerales_porcaja
end type
type st_5 from statictext within w_info_numerales_porcaja
end type
type dw_periodo from datawindow within w_info_numerales_porcaja
end type
type dw_envases from datawindow within w_info_numerales_porcaja
end type
type st_9 from statictext within w_info_numerales_porcaja
end type
type dw_tipoenvase from datawindow within w_info_numerales_porcaja
end type
type st_11 from statictext within w_info_numerales_porcaja
end type
type cbx_tipoenvases from checkbox within w_info_numerales_porcaja
end type
type dw_embalajes from uo_dw within w_info_numerales_porcaja
end type
type st_7 from statictext within w_info_numerales_porcaja
end type
type st_3 from statictext within w_info_numerales_porcaja
end type
type st_6 from statictext within w_info_numerales_porcaja
end type
type st_1 from statictext within w_info_numerales_porcaja
end type
type cbx_productor from checkbox within w_info_numerales_porcaja
end type
type cbx_embalajes from checkbox within w_info_numerales_porcaja
end type
type uo_selespecie from uo_seleccion_especie within w_info_numerales_porcaja
end type
type uo_selvariedad from uo_seleccion_variedad within w_info_numerales_porcaja
end type
type cbx_varirotula from checkbox within w_info_numerales_porcaja
end type
type st_21 from statictext within w_info_numerales_porcaja
end type
type dw_frio from datawindow within w_info_numerales_porcaja
end type
type cbx_frio from checkbox within w_info_numerales_porcaja
end type
type cbx_1 from checkbox within w_info_numerales_porcaja
end type
type st_12 from statictext within w_info_numerales_porcaja
end type
type st_13 from statictext within w_info_numerales_porcaja
end type
type cbx_tipoinfo from checkbox within w_info_numerales_porcaja
end type
end forward

global type w_info_numerales_porcaja from w_para_informes
integer x = 517
integer y = 656
integer width = 3022
integer height = 2004
string title = "INFORME NUMERALES"
gb_7 gb_7
gb_4 gb_4
st_2 st_2
mle_1 mle_1
st_variedad st_variedad
st_4 st_4
st_8 st_8
st_10 st_10
dw_cliente dw_cliente
cbx_planta cbx_planta
dw_planta dw_planta
dw_tipoprod dw_tipoprod
dw_productor dw_productor
st_5 st_5
dw_periodo dw_periodo
dw_envases dw_envases
st_9 st_9
dw_tipoenvase dw_tipoenvase
st_11 st_11
cbx_tipoenvases cbx_tipoenvases
dw_embalajes dw_embalajes
st_7 st_7
st_3 st_3
st_6 st_6
st_1 st_1
cbx_productor cbx_productor
cbx_embalajes cbx_embalajes
uo_selespecie uo_selespecie
uo_selvariedad uo_selvariedad
cbx_varirotula cbx_varirotula
st_21 st_21
dw_frio dw_frio
cbx_frio cbx_frio
cbx_1 cbx_1
st_12 st_12
st_13 st_13
cbx_tipoinfo cbx_tipoinfo
end type
global w_info_numerales_porcaja w_info_numerales_porcaja

type variables
Str_mant				istr_mant
DataWindowChild 	idwc_productor, idwc_periodo, idwc_tipoprod, idwc_tipoen, &
						idwc_planta, idwc_envases, idwc_cliente, idwc_embalajes, idwc_tipofrio

Boolean 				l_b_respo_instancia, l_b_respo

Integer 				ii_cliente, ii_planta, ii_periodo, ii_Productor, ii_TipoProd, &
						ii_EnvaTipoen, ii_EnvaCodigo

String 				is_embalaje, is_periodo, is_frio

uo_seleccion_especie  	iuo_selespecie
uo_seleccion_variedad  iuo_selvariedad
end variables

forward prototypes
public function boolean existevariedad (string variedad, ref string nombre)
public function boolean existeproductor (integer ll_cliente, long ll_productor)
public function boolean periodoabierto (integer ai_periodo)
public function boolean noexisteembalaje (string as_embalaje)
public function boolean noexisteenvase (integer ai_envacodigo)
public function boolean existefrio (string frio)
end prototypes

public function boolean existevariedad (string variedad, ref string nombre);Return true
end function

public function boolean existeproductor (integer ll_cliente, long ll_productor);String	ls_Nombre
Integer	li_cliente

li_cliente	= dw_cliente.Object.clie_Codigo[1]

SELECT	prod_nombre
	INTO	:ls_Nombre
	FROM	dbo.productores as pro,dbo.productoresclientes as cli
	WHERE	pro.prod_codigo =	:ll_productor
	AND	pro.prod_codigo = cli.prod_codigo
	AND	:li_cliente in (-1,cli.clie_codigo);
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de tabla Productor")
	
	RETURN False
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "Código de productor no ha sido definido o pertenece a otro cliente.~r~r" + &
					"Ingrese o seleccione otro Código.")
	RETURN False
ELSE
	ii_productor = ll_Productor
	RETURN True
END IF
end function

public function boolean periodoabierto (integer ai_periodo);integer	li_estado

li_estado = 1

SELECT	fape_estado,fape_observ
	INTO	:li_estado, :is_periodo
	FROM	dbo.FacturPeriodos
	WHERE	clie_codigo	=	:ii_cliente
	and 	fape_numero =  :ai_periodo;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de tabla FaturPeriodos")
	
	RETURN False
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "Periodo Seleccionado no ha sido ingresado para este Cliente.~r~r" + &
					"Ingrese o seleccione otro Código.")
	RETURN False
ELSEIF li_estado = 1 THEN
	MessageBox("Atención", "Periodo Seleccionado ha sido Cerrado.~r~r" + &
					"Ingrese o seleccione otro Código.")
	RETURN False
ELSE
	RETURN True
END IF
end function

public function boolean noexisteembalaje (string as_embalaje);Integer	li_existe, li_envase
IF IsNull(ii_envacodigo) THEN
	li_envase = 0
ELSE
	li_envase = ii_envacodigo
END IF

//SELECT Count(*)
//INTO :li_existe
//FROM dbo.EmbalajesProd
//WHERE clie_codigo =: ii_cliente
//	and emba_codigo =: as_embalaje
//	and :ii_EnvaTipoen in (enva_tipoen, 0)
//	and enva_codigo =: li_envase;
//	
//IF li_existe = 0 OR IsNull(li_existe) THEN
//	RETURN True
//ELSE
//	RETURN False
//END IF
	
SELECT enva_tipoen,enva_codigo
INTO :ii_EnvaTipoen, :ii_envacodigo
FROM dbo.EmbalajesProd
WHERE clie_codigo =: ii_cliente
	and emba_codigo =: as_embalaje;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura tabla EmbalajesProd")
	RETURN True
ELSEIF sqlca.SQLCode = 100 THEN
       RETURN True
END IF
	
RETURN False
//
end function

public function boolean noexisteenvase (integer ai_envacodigo);Integer 	li_existe

SELECT Count(*)
INTO :li_existe
FROM dbo.Envases
WHERE :ii_EnvaTipoen in (enva_tipoen, 0)
	and enva_codigo =: ai_EnvaCodigo;
	
IF li_existe = 0 OR IsNull(li_existe) THEN
	RETURN True
ELSE
	RETURN False
END IF
end function

public function boolean existefrio (string frio);String		ls_Nombre

SELECT	frio_nombre
	INTO	:ls_Nombre
	FROM	dbo.tipofrio
	WHERE	frio_codigo	=	:frio ;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de tabla tipofrio")
	
	RETURN False
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "Código de Frio no ha sido Definido.~r~r" + &
					"Ingrese o seleccione otro Código.")
	RETURN False
ELSE
	RETURN True
END IF
end function

on w_info_numerales_porcaja.create
int iCurrent
call super::create
this.gb_7=create gb_7
this.gb_4=create gb_4
this.st_2=create st_2
this.mle_1=create mle_1
this.st_variedad=create st_variedad
this.st_4=create st_4
this.st_8=create st_8
this.st_10=create st_10
this.dw_cliente=create dw_cliente
this.cbx_planta=create cbx_planta
this.dw_planta=create dw_planta
this.dw_tipoprod=create dw_tipoprod
this.dw_productor=create dw_productor
this.st_5=create st_5
this.dw_periodo=create dw_periodo
this.dw_envases=create dw_envases
this.st_9=create st_9
this.dw_tipoenvase=create dw_tipoenvase
this.st_11=create st_11
this.cbx_tipoenvases=create cbx_tipoenvases
this.dw_embalajes=create dw_embalajes
this.st_7=create st_7
this.st_3=create st_3
this.st_6=create st_6
this.st_1=create st_1
this.cbx_productor=create cbx_productor
this.cbx_embalajes=create cbx_embalajes
this.uo_selespecie=create uo_selespecie
this.uo_selvariedad=create uo_selvariedad
this.cbx_varirotula=create cbx_varirotula
this.st_21=create st_21
this.dw_frio=create dw_frio
this.cbx_frio=create cbx_frio
this.cbx_1=create cbx_1
this.st_12=create st_12
this.st_13=create st_13
this.cbx_tipoinfo=create cbx_tipoinfo
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.gb_7
this.Control[iCurrent+2]=this.gb_4
this.Control[iCurrent+3]=this.st_2
this.Control[iCurrent+4]=this.mle_1
this.Control[iCurrent+5]=this.st_variedad
this.Control[iCurrent+6]=this.st_4
this.Control[iCurrent+7]=this.st_8
this.Control[iCurrent+8]=this.st_10
this.Control[iCurrent+9]=this.dw_cliente
this.Control[iCurrent+10]=this.cbx_planta
this.Control[iCurrent+11]=this.dw_planta
this.Control[iCurrent+12]=this.dw_tipoprod
this.Control[iCurrent+13]=this.dw_productor
this.Control[iCurrent+14]=this.st_5
this.Control[iCurrent+15]=this.dw_periodo
this.Control[iCurrent+16]=this.dw_envases
this.Control[iCurrent+17]=this.st_9
this.Control[iCurrent+18]=this.dw_tipoenvase
this.Control[iCurrent+19]=this.st_11
this.Control[iCurrent+20]=this.cbx_tipoenvases
this.Control[iCurrent+21]=this.dw_embalajes
this.Control[iCurrent+22]=this.st_7
this.Control[iCurrent+23]=this.st_3
this.Control[iCurrent+24]=this.st_6
this.Control[iCurrent+25]=this.st_1
this.Control[iCurrent+26]=this.cbx_productor
this.Control[iCurrent+27]=this.cbx_embalajes
this.Control[iCurrent+28]=this.uo_selespecie
this.Control[iCurrent+29]=this.uo_selvariedad
this.Control[iCurrent+30]=this.cbx_varirotula
this.Control[iCurrent+31]=this.st_21
this.Control[iCurrent+32]=this.dw_frio
this.Control[iCurrent+33]=this.cbx_frio
this.Control[iCurrent+34]=this.cbx_1
this.Control[iCurrent+35]=this.st_12
this.Control[iCurrent+36]=this.st_13
this.Control[iCurrent+37]=this.cbx_tipoinfo
end on

on w_info_numerales_porcaja.destroy
call super::destroy
destroy(this.gb_7)
destroy(this.gb_4)
destroy(this.st_2)
destroy(this.mle_1)
destroy(this.st_variedad)
destroy(this.st_4)
destroy(this.st_8)
destroy(this.st_10)
destroy(this.dw_cliente)
destroy(this.cbx_planta)
destroy(this.dw_planta)
destroy(this.dw_tipoprod)
destroy(this.dw_productor)
destroy(this.st_5)
destroy(this.dw_periodo)
destroy(this.dw_envases)
destroy(this.st_9)
destroy(this.dw_tipoenvase)
destroy(this.st_11)
destroy(this.cbx_tipoenvases)
destroy(this.dw_embalajes)
destroy(this.st_7)
destroy(this.st_3)
destroy(this.st_6)
destroy(this.st_1)
destroy(this.cbx_productor)
destroy(this.cbx_embalajes)
destroy(this.uo_selespecie)
destroy(this.uo_selvariedad)
destroy(this.cbx_varirotula)
destroy(this.st_21)
destroy(this.dw_frio)
destroy(this.cbx_frio)
destroy(this.cbx_1)
destroy(this.st_12)
destroy(this.st_13)
destroy(this.cbx_tipoinfo)
end on

event open;call super::open;Boolean	lb_Cerrar

istr_mant = Message.PowerObjectParm

dw_cliente.SetTransObject(sqlca)
dw_tipoprod.SetTransObject(sqlca)
dw_productor.SetTransobject(sqlca)
dw_planta.SetTransObject(sqlca)
dw_periodo.SetTransObject(sqlca)
dw_tipoenvase.SetTransObject(sqlca)
dw_envases.SetTransObject(sqlca)

dw_cliente.GetChild("Clie_codigo", idwc_cliente)
dw_tipoprod.GetChild("tipr_codigo", idwc_TipoProd)
dw_productor.GetChild('prod_codigo',idwc_productor)
dw_planta.GetChild('plde_codigo',idwc_planta)
dw_periodo.GetChild("fape_numero", idwc_periodo)
dw_tipoenvase.GetChild("enva_tipoen", idwc_tipoen)
dw_envases.GetChild("enva_nombre", idwc_envases)
dw_embalajes.GetChild("emba_codigo", idwc_embalajes)

idwc_cliente.SetTransObject(sqlca)
idwc_TipoProd.SetTransObject(sqlca)
idwc_productor.SetTransobject(sqlca)
idwc_planta.SetTransobject(sqlca)
idwc_periodo.SetTransObject(sqlca)
idwc_tipoen.SetTransObject(sqlca)
idwc_envases.SetTransObject(sqlca)
idwc_embalajes.SetTransObject(sqlca)

idwc_cliente.Retrieve()
idwc_TipoProd.Retrieve()
idwc_productor.Retrieve(1, gi_CodExport)
idwc_periodo.Retrieve(gi_CodExport)
idwc_tipoen.Retrieve(-1)
idwc_planta.Retrieve(1)
idwc_envases.Retrieve(0)
idwc_embalajes.Retrieve(0,0,0)

dw_cliente.InsertRow(0)
dw_tipoprod.InsertRow(0)
dw_productor.InsertRow(0)
dw_planta.InsertRow(0)
dw_periodo.InsertRow(0)
dw_tipoenvase.InsertRow(0)
//dw_envases.InsertRow(0)
dw_embalajes.InsertRow(0)

// iuo_selespecie = Create uo_seleccion_especie
IF IsNull(uo_selespecie.Codigo) THEN lb_Cerrar = True
IF lb_Cerrar THEN
	Close(This)
ELSE
	uo_selespecie.Seleccion(True,True)
END IF

// iuo_selvariedad = Create uo_seleccion_variedad
IF IsNull(uo_selvariedad.Codigo) THEN lb_Cerrar = True
IF lb_Cerrar THEN
	Close(This)
ELSE
	uo_selvariedad.Seleccion(True,True)
END IF

cbx_planta.TriggerEvent("clicked")
cbx_productor.TriggerEvent("clicked")
//cbx_variedad.TriggerEvent("clicked")
cbx_embalajes.TriggerEvent("clicked")

dw_frio.GetChild("frio_codigo", idwc_tipofrio)
idwc_tipofrio.SetTransObject(sqlca)
idwc_tipofrio.Retrieve()
dw_frio.InsertRow(0)
dw_frio.Object.frio_codigo.BackGround.Color	=	RGB(192, 192, 192)

ii_Cliente 									= 	gi_CodExport
ii_Planta										=	-9 //gi_codplanta
ii_TipoProd									= 	1
ii_Periodo									=	dw_periodo.Object.fape_numero[1]//GetItemNumber(1, "fape_numero")
//ii_Especie									=	gi_CodEspecie
//ii_Variedad								=	0 	//gi_CodVariedad
//ii_productor								=	0	//gi_CodProductor
ii_EnvaTipoen								=	0
is_embalaje									=	'z'
is_frio											=	'C' //todo tipo de frio
SetNull(ii_EnvaCodigo)

dw_cliente.Object.clie_codigo[1] 		= 	gi_CodExport
dw_planta.Object.plde_codigo[1]			=	gi_CodPlanta
dw_periodo.Object.fape_numero[1]			=	dw_periodo.Object.fape_numero[1]//GetItemNumber(1,"fape_numero")
dw_tipoprod.Object.tipr_codigo[1] 		= 	1
dw_productor.Object.prod_codigo[1]		=	gi_CodProductor
end event

type pb_excel from w_para_informes`pb_excel within w_info_numerales_porcaja
integer x = 2619
fontcharset fontcharset = ansi!
string facename = "Tahoma"
end type

type st_computador from w_para_informes`st_computador within w_info_numerales_porcaja
integer textsize = -10
fontfamily fontfamily = swiss!
string facename = "Tahoma"
end type

type st_usuario from w_para_informes`st_usuario within w_info_numerales_porcaja
integer textsize = -10
fontfamily fontfamily = swiss!
string facename = "Tahoma"
end type

type st_temporada from w_para_informes`st_temporada within w_info_numerales_porcaja
integer textsize = -10
fontfamily fontfamily = swiss!
string facename = "Tahoma"
end type

type p_logo from w_para_informes`p_logo within w_info_numerales_porcaja
end type

type st_titulo from w_para_informes`st_titulo within w_info_numerales_porcaja
integer width = 2249
string text = "Informe  Numerales"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_numerales_porcaja
integer x = 2615
integer y = 1092
integer taborder = 140
fontcharset fontcharset = ansi!
string facename = "Tahoma"
boolean default = false
alignment htextalign = center!
end type

event pb_acepta::clicked;SetPointer(HourGlass!)
Long		fila
String		ls_titulo
Integer	li_varirotula

istr_info.copias	=	1

ls_titulo			= 'Calculo de Numerales por Caja'
istr_info.titulo	= 'Movimiento de Fruta  Procesada '+ ls_titulo 

OpenWithParm(vinf, istr_info)
IF cbx_tipoinfo.Checked THEN
	vinf.dw_1.DataObject = "dw_info_numerales_porcaja_pallet"
ELSE
	vinf.dw_1.DataObject = "dw_info_numerales_porcaja"
END IF

vinf.dw_1.SetTransObject(sqlca)
/*
Especies
*/
IF IsNull(uo_selespecie.Codigo)THEN
	MessageBox("Atención","Debe Seleccionar una Especie Previamente",Exclamation!)
	uo_selespecie.dw_Seleccion.SetFocus()
	RETURN
END IF
/*
Variedad
*/
IF IsNull(uo_selvariedad.Codigo)THEN
	MessageBox("Atención","Debe Seleccionar una Variedad Previamente",Exclamation!)
	uo_selvariedad.dw_Seleccion.SetFocus()
	RETURN
END IF

/*
Productor
*/
IF cbx_productor.Checked THEN
	ii_Productor = 0
ELSE
	ii_Productor = dw_productor.Object.prod_codigo[1]
END IF	

IF cbx_varirotula.Checked THEN
	li_varirotula = 1
ELSE
	li_varirotula = 0
END IF

SELECT	fape_observ
	INTO	:is_periodo
	FROM	dbo.FacturPeriodos
	WHERE	clie_codigo=  :ii_cliente
	and 	fape_numero =  :ii_periodo;

IF isnull(is_embalaje) THEN
	Messagebox("Error","Debe seleccionar el embalaje", StopSign!)
	Return -1
END IF

fila = vinf.dw_1.Retrieve(ii_cliente, ii_planta, ii_periodo, ii_Productor, ii_TipoProd, &
								  uo_selespecie.Codigo, uo_selvariedad.Codigo, is_embalaje,li_varirotula, is_frio)
IF fila = -1 THEN
		MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
		"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF fila = 0 THEN
		MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
ELSE
	F_Membrete(vinf.dw_1)
	vinf.dw_1.Modify("t_especie.text = '" + uo_SelEspecie.Nombre + "'")
	vinf.dw_1.Modify("t_periodo_des.text =	'" + is_periodo + "'")

	IF gs_Ambiente <> 'Windows' THEN F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
END IF

SetPointer(Arrow!)				
end event

type pb_salir from w_para_informes`pb_salir within w_info_numerales_porcaja
integer x = 2615
integer y = 1364
integer taborder = 150
fontcharset fontcharset = ansi!
string facename = "Tahoma"
end type

type gb_7 from groupbox within w_info_numerales_porcaja
boolean visible = false
integer x = 1641
integer y = 1952
integer width = 946
integer height = 196
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long backcolor = 79741120
string text = "Tipo Entrada"
end type

type gb_4 from groupbox within w_info_numerales_porcaja
boolean visible = false
integer x = 32
integer y = 1952
integer width = 1518
integer height = 356
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long backcolor = 79741120
string text = "Productores"
end type

type st_2 from statictext within w_info_numerales_porcaja
boolean visible = false
integer x = 87
integer y = 1812
integer width = 183
integer height = 76
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long backcolor = 79741120
boolean enabled = false
string text = "Hasta "
boolean focusrectangle = false
end type

type mle_1 from multilineedit within w_info_numerales_porcaja
boolean visible = false
integer x = 2240
integer y = 1952
integer width = 681
integer height = 444
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
string text = "@cliente decimal(3), @planta decimal(4), @periodo decimal(4), @Productor decimal(5), @TipoProd decimal(2), @especie decimal(2), @variedad decimal(4), @EnvaTipoen decimal(1), @EnvaCodigo decimal(3)"
borderstyle borderstyle = stylelowered!
end type

type st_variedad from statictext within w_info_numerales_porcaja
integer x = 343
integer y = 1284
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
string text = "Variedad"
boolean focusrectangle = false
end type

type st_4 from statictext within w_info_numerales_porcaja
integer x = 343
integer y = 1108
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
string text = "Especie"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_8 from statictext within w_info_numerales_porcaja
integer x = 343
integer y = 932
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
string text = "Productor"
boolean focusrectangle = false
end type

type st_10 from statictext within w_info_numerales_porcaja
integer x = 343
integer y = 820
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
string text = "Tipo Prod."
boolean focusrectangle = false
end type

type dw_cliente from datawindow within w_info_numerales_porcaja
integer x = 795
integer y = 468
integer width = 1143
integer height = 100
integer taborder = 10
boolean bringtotop = true
string dataobject = "dddw_clientesprod"
boolean border = false
boolean livescroll = true
end type

event itemchanged;String	ls_Columna[], ls_null

SetNull(ls_null)

IF ExisteCliente(Integer(data), ls_Columna[]) THEN
	ii_cliente	=	Integer(data)

	dw_periodo.SetTransObject(sqlca)
	dw_periodo.GetChild("fape_numero", idwc_periodo)
	idwc_periodo.SetTransObject(sqlca)
	idwc_periodo.Retrieve(ii_cliente)
	dw_periodo.InsertRow(0)
	
	dw_productor.SetTransobject(sqlca)
	dw_productor.GetChild('prod_codigo',idwc_productor)
	idwc_productor.SetTransobject(sqlca)
	idwc_productor.Retrieve(dw_tipoprod.Object.tipr_codigo[1],Integer(data))
	dw_productor.InsertRow(0)
ELSE
	This.SetItem(1, "clie_codigo", ls_null)
	RETURN 1
END IF
end event

event itemerror;//RETURN 1
end event

type cbx_planta from checkbox within w_info_numerales_porcaja
integer x = 1938
integer y = 596
integer width = 503
integer height = 64
integer taborder = 30
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Consolidadas"
boolean checked = true
end type

event clicked;IF This.Checked THEN
	dw_planta.Enabled											=	False
	dw_planta.Object.plde_codigo.BackGround.Color	=	RGB(192, 192, 192)
	ii_planta														=	-9
ELSE
	dw_planta.Enabled											=	True
	dw_planta.Object.plde_codigo.BackGround.Color	=	RGB(255, 255, 255)
	ii_planta = dw_planta.Object.plde_codigo[1]
	dw_planta.SetFocus()
END IF
end event

type dw_planta from datawindow within w_info_numerales_porcaja
integer x = 800
integer y = 580
integer width = 969
integer height = 100
integer taborder = 20
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_plantadesp_tipo"
boolean border = false
boolean livescroll = true
end type

event itemchanged;String	ls_Columna[], ls_null

SetNull(ls_null)

IF ExistePlanta(ii_Cliente, Integer(data), ls_Columna[]) THEN
	ii_planta	=	integer(data)
ELSE
	This.SetItem(1, "plde_codigo", ls_null)
	RETURN 1
END IF
end event

event itemerror;RETURN 1
end event

type dw_tipoprod from datawindow within w_info_numerales_porcaja
integer x = 795
integer y = 804
integer width = 1006
integer height = 100
integer taborder = 50
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_tipoproduc"
boolean border = false
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event itemchanged;ii_TipoProd	=	integer(data)
dw_productor.GetChild("prod_codigo", idwc_productor)
idwc_productor.Retrieve(integer(data),dw_cliente.Object.clie_codigo[1])
dw_productor.InsertRow(0)

end event

event itemerror;RETURN 1
end event

type dw_productor from datawindow within w_info_numerales_porcaja
integer x = 782
integer y = 908
integer width = 1074
integer height = 96
integer taborder = 60
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_productores_tipopr_clientes"
boolean border = false
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event itemchanged;String	 ls_null
SetNull(ls_null)

IF ExisteProductor(ii_cliente,Long(data)) THEN
	ii_productor	=	integer(data)
	RETURN 0
ELSE
	This.SetItem(1, "prod_codigo", ls_null)
	RETURN 1
END IF
end event

event itemerror;RETURN 1
end event

type st_5 from statictext within w_info_numerales_porcaja
integer x = 343
integer y = 708
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
string text = "Periodo"
boolean focusrectangle = false
end type

type dw_periodo from datawindow within w_info_numerales_porcaja
integer x = 791
integer y = 692
integer width = 1006
integer height = 100
integer taborder = 40
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_facperiodos"
boolean border = false
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event itemchanged;Integer	 li_null
SetNull(li_null)

IF periodoabierto(integer(data)) THEN
ii_periodo	=	integer(data)
	RETURN 0
ELSE
	This.SetItem(1, "fape_numero", li_null)
	RETURN 1
END IF
end event

event itemerror;RETURN 1
end event

type dw_envases from datawindow within w_info_numerales_porcaja
boolean visible = false
integer x = 521
integer y = 2368
integer width = 1193
integer height = 100
integer taborder = 160
boolean bringtotop = true
boolean enabled = false
string title = "none"
string dataobject = "dddw_envases"
boolean border = false
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event itemchanged;String 	ls_columna

ls_columna = dwo.name

IF NoExisteEnvase(Integer(data)) THEN
	MessageBox("Error", "El Codigo de envase ingresado no existe o no corresponde al tipo de envase seleccionado", StopSign!)
	SetNull(ii_envacodigo)
	This.SetItem(row, ls_columna, ii_envacodigo)
ELSE
	ii_envacodigo	=	Integer(data)
	
	dw_embalajes.Reset()
	dw_embalajes.SetFocus()
	dw_embalajes.GetChild("emba_codigo", idwc_embalajes)
	idwc_embalajes.SetTransObject(sqlca)
		
	idwc_embalajes.Retrieve(ii_cliente, ii_EnvaTipoen, ii_EnvaCodigo)
	dw_embalajes.InsertRow(0)
	setNull(is_embalaje)
END IF
end event

event itemerror;//RETURN 1
end event

type st_9 from statictext within w_info_numerales_porcaja
boolean visible = false
integer x = 151
integer y = 1876
integer width = 443
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
long backcolor = 12632256
string text = "Codigo Envase"
boolean focusrectangle = false
end type

type dw_tipoenvase from datawindow within w_info_numerales_porcaja
boolean visible = false
integer x = 1797
integer y = 2372
integer width = 1193
integer height = 96
boolean bringtotop = true
boolean enabled = false
string title = "none"
string dataobject = "dddw_tipoenvases"
boolean border = false
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event itemchanged;ii_envatipoen	=	integer(data)

dw_envases.GetChild("enva_nombre", idwc_envases)
idwc_envases.SetTransObject(sqlca)
dw_envases.SetTransObject(sqlca)

idwc_envases.Retrieve()

idwc_envases.SetFilter("enva_tipoen = " + String(ii_envatipoen))
idwc_envases.Filter()

dw_envases.InsertRow(0)
end event

event itemerror;//RETURN 1
end event

type st_11 from statictext within w_info_numerales_porcaja
boolean visible = false
integer x = 1819
integer y = 1864
integer width = 366
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
long backcolor = 12632256
string text = "Tipo Envase"
boolean focusrectangle = false
end type

type cbx_tipoenvases from checkbox within w_info_numerales_porcaja
boolean visible = false
integer x = 1317
integer y = 1832
integer width = 402
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
string text = "Todos"
boolean checked = true
end type

event clicked;IF This.Checked THEN
	dw_tipoenvase.Enabled										=	False
	dw_tipoenvase.Object.enva_tipoen.BackGround.Color	=	RGB(192, 192, 192)
	ii_EnvaTipoen													= 	0
	
	dw_envases.GetChild("enva_nombre", idwc_envases)
	idwc_envases.SetTransObject(sqlca)
	dw_envases.SetTransObject(sqlca)
	
	idwc_envases.Retrieve()
	
	idwc_envases.SetFilter("")
	idwc_envases.Filter()
	
	dw_envases.InsertRow(0)
ELSE
	dw_tipoenvase.Enabled										=	True
	dw_tipoenvase.Object.enva_tipoen.BackGround.Color	=	RGB(255, 255, 255)
	dw_tipoenvase.SetFocus()
	setNull(ii_EnvaTipoen)
END IF
end event

type dw_embalajes from uo_dw within w_info_numerales_porcaja
integer x = 791
integer y = 1400
integer width = 1157
integer height = 84
integer taborder = 120
boolean bringtotop = true
string dataobject = "dddw_embalajesprod"
boolean vscrollbar = false
boolean border = false
end type

event itemchanged;call super::itemchanged;String 	ls_columna

ls_columna = dwo.name
CHOOSE CASE ls_columna
	CASE "emba_codigo"
		IF NoExisteEmbalaje(data) THEN
			MessageBox("Error", "El Embalaje ingresado no existe o no corresponde al Codigo de Envase seleccionado.", StopSign!)
			SetNull(is_embalaje)
			THIS.SetItem(row, ls_columna, is_embalaje)
		ELSE
			is_embalaje 	= 	Data
			dw_tipoenvase.SetItem(row, 'enva_tipoen', ii_envatipoen)
		
//			dw_envases.GetChild("enva_codigo", idwc_envases)
//			idwc_envases.SetTransObject(sqlca)
//			dw_envases.SetTransObject(sqlca)
//			idwc_envases.Retrieve()
//		
//			idwc_envases.SetFilter("enva_tipoen = " + String(ii_envatipoen)+" and enva_codigo= "+String(ii_envacodigo) )
//			idwc_envases.Filter()
//			
//			dw_envases.InsertRow(0)
//			dw_envases.SetItem(row, 'enva_codigo', ii_envacodigo)
		END IF
		
END CHOOSE

	
end event

type st_7 from statictext within w_info_numerales_porcaja
integer x = 343
integer y = 1412
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
string text = "Embalaje"
boolean focusrectangle = false
end type

type st_3 from statictext within w_info_numerales_porcaja
integer x = 343
integer y = 596
integer width = 334
integer height = 64
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

type st_6 from statictext within w_info_numerales_porcaja
integer x = 343
integer y = 484
integer width = 334
integer height = 64
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

type st_1 from statictext within w_info_numerales_porcaja
boolean visible = false
integer x = 50
integer y = 1856
integer width = 197
integer height = 76
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long backcolor = 79741120
boolean enabled = false
string text = "Desde"
boolean focusrectangle = false
end type

type cbx_productor from checkbox within w_info_numerales_porcaja
integer x = 1938
integer y = 932
integer width = 402
integer height = 64
integer taborder = 70
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
	dw_productor.Enabled											=	False
	dw_productor.Object.prod_codigo.BackGround.Color	=	RGB(192, 192, 192)
	ii_productor															=	0
ELSE
	
	dw_productor.Enabled											=	True
	dw_productor.Object.prod_codigo.BackGround.Color	=	RGB(255, 255, 255)
	dw_productor.SetFocus()
	setnull(ii_productor)

	dw_productor.GetChild("prod_codigo", idwc_productor)
	idwc_productor.Retrieve(integer(ii_TipoProd),dw_cliente.Object.clie_codigo[1])
	
END IF
end event

type cbx_embalajes from checkbox within w_info_numerales_porcaja
integer x = 1938
integer y = 1412
integer width = 402
integer height = 64
integer taborder = 130
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
	dw_embalajes.Enabled											=	False
	dw_embalajes.Object.emba_codigo.BackGround.Color	=	RGB(192, 192, 192)
	is_embalaje															= 	"z"
	
//ELSEIF ii_envacodigo = 0 or IsNull(ii_envacodigo) THEN
//	MessageBox("Error", "Debe ingresar el codigo de envase antes de seleccionar Embalaje", StopSign!)
//	This.Checked = True


	dw_tipoenvase.Object.enva_tipoen.BackGround.Color	=	RGB(192, 192, 192)
///	dw_envases.Object.enva_codigo.BackGround.Color	=	RGB(192, 192, 192)
	
ELSE
	dw_embalajes.Enabled											=	True
	dw_embalajes.Object.emba_codigo.BackGround.Color	=	RGB(255, 255, 255)
	
	dw_tipoenvase.Object.enva_tipoen.BackGround.Color	=	RGB(255, 255, 255)
	dw_tipoenvase.Reset()
	dw_tipoenvase.InsertRow(0)

//	dw_envases.Object.enva_codigo.BackGround.Color		=	RGB(255, 255, 255)
//	dw_envases.Reset()
//	dw_envases.InsertRow(0)	
//		
	dw_embalajes.Reset()
	dw_embalajes.SetFocus()
	dw_embalajes.GetChild("emba_codigo", idwc_embalajes)
	idwc_embalajes.SetTransObject(sqlca)
	dw_embalajes.SetTransObject(sqlca)
	
	idwc_embalajes.Retrieve(ii_cliente)
	dw_embalajes.InsertRow(0)
	
END IF

end event

type uo_selespecie from uo_seleccion_especie within w_info_numerales_porcaja
integer x = 786
integer y = 1008
integer height = 180
integer taborder = 240
boolean bringtotop = true
end type

event ue_cambio;call super::ue_cambio;IF IsNull(This.Codigo) THEN RETURN

CHOOSE CASE This.Codigo
	CASE -1, -9
		
	CASE ELSE
		uo_selvariedad.Filtra(This.Codigo)

END CHOOSE
end event

on uo_selespecie.destroy
call uo_seleccion_especie::destroy
end on

type uo_selvariedad from uo_seleccion_variedad within w_info_numerales_porcaja
integer x = 782
integer y = 1192
integer taborder = 260
boolean bringtotop = true
end type

on uo_selvariedad.destroy
call uo_seleccion_variedad::destroy
end on

type cbx_varirotula from checkbox within w_info_numerales_porcaja
integer x = 1938
integer y = 1288
integer width = 379
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
string text = "Rotulada"
end type

type st_21 from statictext within w_info_numerales_porcaja
integer x = 343
integer y = 1564
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
string text = "Tipo Frío"
boolean focusrectangle = false
end type

type dw_frio from datawindow within w_info_numerales_porcaja
integer x = 786
integer y = 1556
integer width = 1033
integer height = 92
integer taborder = 210
boolean bringtotop = true
boolean enabled = false
string title = "none"
string dataobject = "dddw_tipofrio"
boolean border = false
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event itemchanged;String	ls_null

SetNull(ls_null)

IF ExisteFrio(data) THEN
	is_frio	=	data
ELSE
	This.SetItem(1, "frio_codigo", ls_null)
	is_frio = ''
	RETURN 1
END IF
end event

type cbx_frio from checkbox within w_info_numerales_porcaja
integer x = 795
integer y = 1492
integer width = 366
integer height = 52
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
boolean enabled = false
string text = "Todos"
boolean checked = true
end type

event clicked;IF This.Checked THEN
	
	dw_frio.Enabled										=	False
	dw_frio.Object.frio_codigo.BackGround.Color	=	RGB(192, 192, 192)
	dw_frio.Reset()
	idwc_tipofrio.Retrieve()
	dw_frio.InsertRow(0)
	is_frio = '*'
		
ELSE
	dw_frio.Enabled										=	True
	dw_frio.Object.frio_codigo.BackGround.Color	=	RGB(255, 255, 255)
	dw_frio.SetFocus()
END IF


end event

type cbx_1 from checkbox within w_info_numerales_porcaja
integer x = 1289
integer y = 1492
integer width = 471
integer height = 52
integer taborder = 30
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Consolidadas"
boolean checked = true
end type

event clicked;IF This.Checked THEN
	dw_frio.Enabled											=	False
	dw_frio.Object.frio_codigo.BackGround.Color		=	RGB(192, 192, 192)
	is_frio														=	'C'
	cbx_frio.Enabled											=	False
	cbx_frio.Checked											=	True
	dw_frio.Reset()
	idwc_tipofrio.Retrieve()
	dw_frio.InsertRow(0)
ELSE
	cbx_frio.Enabled											=	True
	dw_frio.Enabled											=	False
	is_frio														=	'*'
END IF
end event

type st_12 from statictext within w_info_numerales_porcaja
integer x = 247
integer y = 440
integer width = 2249
integer height = 1228
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long backcolor = 16711680
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_13 from statictext within w_info_numerales_porcaja
integer x = 247
integer y = 1668
integer width = 2249
integer height = 128
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long backcolor = 16711680
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type cbx_tipoinfo from checkbox within w_info_numerales_porcaja
integer x = 1070
integer y = 1692
integer width = 503
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
string text = "Por Pallet"
boolean lefttext = true
end type

