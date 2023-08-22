$PBExportHeader$w_info_numerales_facturacion.srw
forward
global type w_info_numerales_facturacion from w_para_informes
end type
type gb_7 from groupbox within w_info_numerales_facturacion
end type
type gb_4 from groupbox within w_info_numerales_facturacion
end type
type st_2 from statictext within w_info_numerales_facturacion
end type
type mle_1 from multilineedit within w_info_numerales_facturacion
end type
type sle_variedad from singlelineedit within w_info_numerales_facturacion
end type
type cb_buscavariedad from commandbutton within w_info_numerales_facturacion
end type
type em_variedad from editmask within w_info_numerales_facturacion
end type
type st_variedad from statictext within w_info_numerales_facturacion
end type
type st_4 from statictext within w_info_numerales_facturacion
end type
type st_8 from statictext within w_info_numerales_facturacion
end type
type st_10 from statictext within w_info_numerales_facturacion
end type
type dw_cliente from datawindow within w_info_numerales_facturacion
end type
type cbx_planta from checkbox within w_info_numerales_facturacion
end type
type dw_planta from datawindow within w_info_numerales_facturacion
end type
type dw_tipoprod from datawindow within w_info_numerales_facturacion
end type
type dw_productor from datawindow within w_info_numerales_facturacion
end type
type dw_especie from datawindow within w_info_numerales_facturacion
end type
type st_5 from statictext within w_info_numerales_facturacion
end type
type dw_periodo from datawindow within w_info_numerales_facturacion
end type
type dw_envases from datawindow within w_info_numerales_facturacion
end type
type st_9 from statictext within w_info_numerales_facturacion
end type
type dw_tipoenvase from datawindow within w_info_numerales_facturacion
end type
type st_11 from statictext within w_info_numerales_facturacion
end type
type cbx_tipoenvases from checkbox within w_info_numerales_facturacion
end type
type dw_embalajes from uo_dw within w_info_numerales_facturacion
end type
type st_7 from statictext within w_info_numerales_facturacion
end type
type st_3 from statictext within w_info_numerales_facturacion
end type
type st_6 from statictext within w_info_numerales_facturacion
end type
type st_1 from statictext within w_info_numerales_facturacion
end type
type cbx_productor from checkbox within w_info_numerales_facturacion
end type
type cbx_variedad from checkbox within w_info_numerales_facturacion
end type
type cbx_embalajes from checkbox within w_info_numerales_facturacion
end type
type gb_3 from groupbox within w_info_numerales_facturacion
end type
end forward

global type w_info_numerales_facturacion from w_para_informes
integer x = 517
integer y = 656
integer width = 3118
integer height = 2096
string title = "INFORME NUMERALES"
boolean minbox = false
boolean maxbox = false
boolean resizable = false
windowtype windowtype = response!
gb_7 gb_7
gb_4 gb_4
st_2 st_2
mle_1 mle_1
sle_variedad sle_variedad
cb_buscavariedad cb_buscavariedad
em_variedad em_variedad
st_variedad st_variedad
st_4 st_4
st_8 st_8
st_10 st_10
dw_cliente dw_cliente
cbx_planta cbx_planta
dw_planta dw_planta
dw_tipoprod dw_tipoprod
dw_productor dw_productor
dw_especie dw_especie
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
cbx_variedad cbx_variedad
cbx_embalajes cbx_embalajes
gb_3 gb_3
end type
global w_info_numerales_facturacion w_info_numerales_facturacion

type variables
Str_mant				istr_mant
DataWindowChild 	idwc_productor, idwc_periodo, idwc_especie, idwc_tipoprod
DataWindowChild 	idwc_planta, idwc_tipoen, idwc_envases, idwc_cliente, idwc_embalajes

Boolean 				l_b_respo_instancia, l_b_respo

Integer 				ii_cliente, ii_planta, ii_periodo, ii_Productor, ii_TipoProd
Integer 				ii_variedad, ii_EnvaTipoen, ii_EnvaCodigo, ii_especie

String 				is_especie, is_embalaje, is_periodo
end variables

forward prototypes
public function boolean existevariedad (string variedad, ref string nombre)
public function boolean existeespecie (integer cliente, integer especie)
public function boolean existeproductor (integer ll_cliente, long ll_productor)
public function boolean periodoabierto (integer ai_periodo)
public function boolean noexisteembalaje (string as_embalaje)
public function boolean noexisteenvase (integer ai_envacodigo)
end prototypes

public function boolean existevariedad (string variedad, ref string nombre);Return true
end function

public function boolean existeespecie (integer cliente, integer especie);String		ls_Nombre

SELECT	espe_nombre
	INTO	:is_especie
	FROM	dba.especies
	WHERE	espe_codigo	=	:Especie ;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de tabla Especies")
	
	RETURN False
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "Código de Especie no ha sido Definido.~r~r" + &
					"Ingrese o seleccione otro Código.")
	RETURN False
ELSE
	RETURN True
END IF
end function

public function boolean existeproductor (integer ll_cliente, long ll_productor);String	ls_Nombre

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
	ii_productor = ll_Productor
	RETURN True
END IF
end function

public function boolean periodoabierto (integer ai_periodo);integer	li_estado

li_estado = 1

SELECT	fape_estado,fape_observ
	INTO	:li_estado, :is_periodo
	FROM	dba.FacturPeriodos
	WHERE	clie_codigo	=	:ii_cliente
	and 	fape_numero =  :ai_periodo;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de tabla FaturPeriodos")
	
	RETURN False
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "Periodo Seleccionado no ha sido ingresado para este Cliente.~r~r" + &
					"Ingrese o seleccione otro Código.")
	RETURN False
//ELSEIF li_estado = 1 THEN
//	MessageBox("Atención", "Periodo Seleccionado ha sido Cerrado.~r~r" + &
//					"Ingrese o seleccione otro Código.")
//	RETURN False
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
//FROM dba.EmbalajesProd
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
FROM dba.EmbalajesProd
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
FROM dba.Envases
WHERE :ii_EnvaTipoen in (enva_tipoen, 0)
	and enva_codigo =: ai_EnvaCodigo;
	
IF li_existe = 0 OR IsNull(li_existe) THEN
	RETURN True
ELSE
	RETURN False
END IF
end function

on w_info_numerales_facturacion.create
int iCurrent
call super::create
this.gb_7=create gb_7
this.gb_4=create gb_4
this.st_2=create st_2
this.mle_1=create mle_1
this.sle_variedad=create sle_variedad
this.cb_buscavariedad=create cb_buscavariedad
this.em_variedad=create em_variedad
this.st_variedad=create st_variedad
this.st_4=create st_4
this.st_8=create st_8
this.st_10=create st_10
this.dw_cliente=create dw_cliente
this.cbx_planta=create cbx_planta
this.dw_planta=create dw_planta
this.dw_tipoprod=create dw_tipoprod
this.dw_productor=create dw_productor
this.dw_especie=create dw_especie
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
this.cbx_variedad=create cbx_variedad
this.cbx_embalajes=create cbx_embalajes
this.gb_3=create gb_3
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.gb_7
this.Control[iCurrent+2]=this.gb_4
this.Control[iCurrent+3]=this.st_2
this.Control[iCurrent+4]=this.mle_1
this.Control[iCurrent+5]=this.sle_variedad
this.Control[iCurrent+6]=this.cb_buscavariedad
this.Control[iCurrent+7]=this.em_variedad
this.Control[iCurrent+8]=this.st_variedad
this.Control[iCurrent+9]=this.st_4
this.Control[iCurrent+10]=this.st_8
this.Control[iCurrent+11]=this.st_10
this.Control[iCurrent+12]=this.dw_cliente
this.Control[iCurrent+13]=this.cbx_planta
this.Control[iCurrent+14]=this.dw_planta
this.Control[iCurrent+15]=this.dw_tipoprod
this.Control[iCurrent+16]=this.dw_productor
this.Control[iCurrent+17]=this.dw_especie
this.Control[iCurrent+18]=this.st_5
this.Control[iCurrent+19]=this.dw_periodo
this.Control[iCurrent+20]=this.dw_envases
this.Control[iCurrent+21]=this.st_9
this.Control[iCurrent+22]=this.dw_tipoenvase
this.Control[iCurrent+23]=this.st_11
this.Control[iCurrent+24]=this.cbx_tipoenvases
this.Control[iCurrent+25]=this.dw_embalajes
this.Control[iCurrent+26]=this.st_7
this.Control[iCurrent+27]=this.st_3
this.Control[iCurrent+28]=this.st_6
this.Control[iCurrent+29]=this.st_1
this.Control[iCurrent+30]=this.cbx_productor
this.Control[iCurrent+31]=this.cbx_variedad
this.Control[iCurrent+32]=this.cbx_embalajes
this.Control[iCurrent+33]=this.gb_3
end on

on w_info_numerales_facturacion.destroy
call super::destroy
destroy(this.gb_7)
destroy(this.gb_4)
destroy(this.st_2)
destroy(this.mle_1)
destroy(this.sle_variedad)
destroy(this.cb_buscavariedad)
destroy(this.em_variedad)
destroy(this.st_variedad)
destroy(this.st_4)
destroy(this.st_8)
destroy(this.st_10)
destroy(this.dw_cliente)
destroy(this.cbx_planta)
destroy(this.dw_planta)
destroy(this.dw_tipoprod)
destroy(this.dw_productor)
destroy(this.dw_especie)
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
destroy(this.cbx_variedad)
destroy(this.cbx_embalajes)
destroy(this.gb_3)
end on

event open;call super::open;istr_mant = Message.PowerObjectParm

dw_cliente.SetTransObject(sqlca)
dw_tipoprod.SetTransObject(sqlca)
dw_productor.SetTransobject(sqlca)
dw_especie.SetTransObject(sqlca)
dw_planta.SetTransObject(sqlca)
dw_periodo.SetTransObject(sqlca)
dw_tipoenvase.SetTransObject(sqlca)
dw_envases.SetTransObject(sqlca)

dw_cliente.GetChild("Clie_codigo", idwc_cliente)
dw_tipoprod.GetChild("tipr_codigo", idwc_TipoProd)
dw_productor.GetChild('prod_codigo',idwc_productor)
dw_especie.GetChild('espe_codigo',idwc_especie)
dw_planta.GetChild('plde_codigo',idwc_planta)
dw_periodo.GetChild("fape_numero", idwc_periodo)
dw_tipoenvase.GetChild("enva_tipoen", idwc_tipoen)
dw_envases.GetChild("enva_nombre", idwc_envases)
dw_embalajes.GetChild("emba_codigo", idwc_embalajes)

idwc_cliente.SetTransObject(sqlca)
idwc_TipoProd.SetTransObject(sqlca)
idwc_productor.SetTransobject(sqlca)
idwc_especie.SetTransobject(sqlca)
idwc_planta.SetTransobject(sqlca)
idwc_periodo.SetTransObject(sqlca)
idwc_tipoen.SetTransObject(sqlca)
//idwc_envases.SetTransObject(sqlca)
idwc_embalajes.SetTransObject(sqlca)

idwc_cliente.Retrieve()
idwc_TipoProd.Retrieve()
idwc_productor.Retrieve(1)
idwc_especie.Retrieve()
idwc_periodo.Retrieve(gi_CodExport)
idwc_tipoen.Retrieve()
idwc_planta.Retrieve(-1)
//idwc_envases.Retrieve(0)
idwc_embalajes.Retrieve(0,0,0)

dw_cliente.InsertRow(0)
dw_tipoprod.InsertRow(0)
dw_productor.InsertRow(0)
dw_especie.InsertRow(0)
dw_planta.InsertRow(0)
dw_periodo.InsertRow(0)
dw_tipoenvase.InsertRow(0)
//dw_envases.InsertRow(0)
dw_embalajes.InsertRow(0)

ii_TipoProd									= 	1
ii_Periodo									=	idwc_Periodo.GetItemNumber(1, "fape_numero")
ii_Especie									=	gi_CodEspecie
ii_Variedad									=	-1 	//gi_CodVariedad
ii_productor								=	0	//gi_CodProductor
ii_EnvaTipoen								=	0
is_embalaje									=	'z'
SetNull(ii_EnvaCodigo)

ii_Cliente	=	Integer(istr_mant.argumento[1])
ii_Planta	=	Integer(istr_mant.argumento[2])

dw_cliente.Object.clie_codigo[1] 	= 	ii_Cliente
IF ii_Planta = -9 THEN
	dw_planta.Object.plde_codigo.BackGround.Color	=	RGB(166,180,210)
	dw_planta.Enabled = False
	cbx_planta.Checked = True
ELSE
	dw_planta.Object.plde_codigo.BackGround.Color	=	RGB(255, 255, 255)
	dw_planta.Enabled = True
	cbx_planta.Checked = False
END IF	

dw_periodo.Object.fape_numero[1]		=	idwc_Periodo.GetItemNumber(Integer(istr_mant.argumento[16]),"fape_numero")
dw_tipoprod.Object.tipr_codigo[1] 	= 	1
dw_especie.Object.espe_codigo[1]		=	gi_CodEspecie

istr_mant.argumento[18] = String(0)
istr_mant.argumento[17] = String(1)
istr_mant.argumento[19] = String(gi_CodEspecie)
istr_mant.argumento[20] = String(-1)
istr_mant.argumento[21] = String('z')

dw_productor.Object.prod_codigo.BackGround.Color	=	RGB(166,180,210)
dw_cliente.Object.clie_codigo.BackGround.Color		=	RGB(166,180,210)
dw_periodo.Object.fape_numero.BackGround.Color		=	RGB(166,180,210)
dw_embalajes.Object.emba_codigo.BackGround.Color	=	RGB(166,180,210)
end event

type st_computador from w_para_informes`st_computador within w_info_numerales_facturacion
end type

type st_usuario from w_para_informes`st_usuario within w_info_numerales_facturacion
end type

type st_temporada from w_para_informes`st_temporada within w_info_numerales_facturacion
end type

type p_logo from w_para_informes`p_logo within w_info_numerales_facturacion
integer y = 24
end type

type st_titulo from w_para_informes`st_titulo within w_info_numerales_facturacion
integer width = 2327
string text = "Informe  Numerales"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_numerales_facturacion
boolean visible = false
integer x = 2807
integer y = 980
integer taborder = 100
boolean default = false
alignment htextalign = center!
end type

event pb_acepta::clicked;SetPointer(HourGlass!)
Long		fila
String	l_s_titulo

istr_info.copias	=	1

l_s_titulo	= 'Calculo de Numerales por Caja'
istr_info.titulo	= 'Movimiento de Fruta  Procesada '+l_s_titulo 

OpenWithParm(vinf, istr_info)

vinf.dw_1.DataObject = "dw_info_numerales_porcaja"

vinf.dw_1.SetTransObject(sqlca)
dw_especie.AcceptText()


SELECT	fape_observ
	INTO	:is_periodo
	FROM	dba.FacturPeriodos
	WHERE	clie_codigo=  :ii_cliente
	and 	fape_numero =  :ii_periodo;

//IF isnull(ii_EnvaCodigo) THEN
//	Messagebox("Error","Debe seleccionar el envase", StopSign!)
//	Return -1
//END IF
IF isnull(is_embalaje) THEN
	Messagebox("Error","Debe seleccionar el embalaje", StopSign!)
	Return -1
END IF
//fila = vinf.dw_1.Retrieve(ii_cliente, ii_planta, ii_periodo, ii_Productor, ii_TipoProd, &
//									  ii_especie, ii_variedad, ii_EnvaTipoen, ii_EnvaCodigo, is_embalaje)

fila = vinf.dw_1.Retrieve(ii_cliente, ii_planta, ii_periodo, ii_Productor, ii_TipoProd, &
									  ii_especie, ii_variedad, is_embalaje)
IF fila = -1 THEN
		MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
		"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF fila = 0 THEN
		MessageBox( "No Existe información", "No existe información para este informe.", &
		StopSign!, Ok!)
ELSE
		F_Membrete(vinf.dw_1)
	IF gs_Ambiente <> 'Windows' THEN
		F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
	END IF
		vinf.dw_1.Modify("t_especie.text = '" + is_especie + "'")
		//vinf.dw_1.Modify("t_embalaje.text =	'" + is_embalaje + "'")
		vinf.dw_1.Modify("t_periodo_des.text =	'" + is_periodo + "'")

		vinf.Visible	= True
		vinf.Enabled	= True
END IF

SetPointer(Arrow!)				
end event

type pb_salir from w_para_informes`pb_salir within w_info_numerales_facturacion
integer x = 2811
integer y = 1252
integer taborder = 110
end type

event pb_salir::clicked;istr_mant.argumento[17]	=	String(ii_TipoProd)
istr_mant.argumento[18]	=	String(ii_productor)
istr_mant.argumento[19]	=	String(ii_especie)
istr_mant.argumento[20]	=	String(ii_variedad)
istr_mant.argumento[21]	=	is_embalaje

CloseWithReturn(Parent, istr_mant)
end event

type gb_7 from groupbox within w_info_numerales_facturacion
boolean visible = false
integer x = 1664
integer y = 1724
integer width = 946
integer height = 196
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 79741120
string text = "Tipo Entrada"
end type

type gb_4 from groupbox within w_info_numerales_facturacion
boolean visible = false
integer x = 27
integer y = 1604
integer width = 1518
integer height = 356
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 79741120
string text = "Productores"
end type

type st_2 from statictext within w_info_numerales_facturacion
boolean visible = false
integer x = 87
integer y = 1828
integer width = 183
integer height = 76
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 79741120
boolean enabled = false
string text = "Hasta "
boolean focusrectangle = false
end type

type mle_1 from multilineedit within w_info_numerales_facturacion
boolean visible = false
integer x = 1874
integer y = 1800
integer width = 681
integer height = 444
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
string text = "@cliente decimal(3), @planta decimal(4), @periodo decimal(4), @Productor decimal(5), @TipoProd decimal(2), @especie decimal(2), @variedad decimal(4), @EnvaTipoen decimal(1), @EnvaCodigo decimal(3)"
borderstyle borderstyle = stylelowered!
end type

type sle_variedad from singlelineedit within w_info_numerales_facturacion
integer x = 1166
integer y = 1196
integer width = 809
integer height = 96
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
borderstyle borderstyle = stylelowered!
end type

type cb_buscavariedad from commandbutton within w_info_numerales_facturacion
integer x = 1061
integer y = 1204
integer width = 96
integer height = 84
integer taborder = 60
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

lstr_busq.argum[1]	=	String(ii_Cliente)
lstr_busq.argum[2]	=	String(ii_Especie)

OpenWithParm(w_busc_variedades, lstr_busq)

lstr_busq	       = Message.PowerObjectParm

IF lstr_busq.argum[4] = "" THEN
	em_variedad.SetFocus()
ELSE
	em_variedad.Text			=	lstr_busq.argum[4]
	sle_variedad.Text			=	lstr_busq.argum[5]
	ii_variedad					=	integer(lstr_busq.argum[4])
END IF

end event

type em_variedad from editmask within w_info_numerales_facturacion
integer x = 795
integer y = 1200
integer width = 261
integer height = 88
integer taborder = 50
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 16777215
boolean enabled = false
string text = "none"
alignment alignment = center!
borderstyle borderstyle = stylelowered!
string mask = "####"
end type

event modified;Integer		li_cliente,	li_especie, li_variedad
String		ls_Nombre

ii_variedad = Integer(this.Text)

SELECT	vari_nombre
	INTO	:ls_Nombre
	FROM	dba.variedades
	WHERE	espe_codigo	=	:ii_especie
	AND	vari_codigo	=	:ii_variedad;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de tabla Variedades")
	SetNull(ii_variedad)
	This.SetFocus()
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "Código de Variedad no ha sido Definido.~r~r" + &
		"Ingrese o seleccione otro Código.")
	SetNull(ii_variedad)
	This.SetFocus()
ELSE
	sle_variedad.Text			=	ls_nombre
	istr_mant.argumento[20] = 	String(ii_variedad)
END IF
end event

type st_variedad from statictext within w_info_numerales_facturacion
integer x = 343
integer y = 1212
integer width = 302
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
string text = "Variedad"
boolean focusrectangle = false
end type

type st_4 from statictext within w_info_numerales_facturacion
integer x = 343
integer y = 1100
integer width = 270
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
string text = "Especie"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_8 from statictext within w_info_numerales_facturacion
integer x = 343
integer y = 988
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
long backcolor = 33543637
string text = "Productor"
boolean focusrectangle = false
end type

type st_10 from statictext within w_info_numerales_facturacion
integer x = 343
integer y = 876
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
long backcolor = 33543637
string text = "Tipo Prod."
boolean focusrectangle = false
end type

type dw_cliente from datawindow within w_info_numerales_facturacion
integer x = 795
integer y = 524
integer width = 1143
integer height = 100
boolean bringtotop = true
boolean enabled = false
string dataobject = "dddw_clientesprod"
boolean border = false
boolean livescroll = true
end type

event itemchanged;String	ls_Columna[], ls_null

SetNull(ls_null)

IF ExisteCliente(Integer(data), ls_Columna[]) THEN
	ii_cliente	=	Integer(data)
	//dw_periodo.Reset()
	//dw_periodo.GetChild("fape_numero", idwc_periodo)
	//idwc_periodo.SetTRansObject(SQLCA)
	idwc_periodo.Retrieve(ii_cliente)
	//dw_periodo.InsertRow(0)
ELSE
	This.SetItem(1, "clie_codigo", ls_null)
	//dw_periodo.Reset()
	RETURN 1
END IF
end event

event itemerror;//RETURN 1
end event

type cbx_planta from checkbox within w_info_numerales_facturacion
integer x = 2048
integer y = 652
integer width = 471
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
string text = "Consolidadas"
boolean checked = true
end type

event clicked;IF This.Checked THEN
	dw_planta.Enabled											=	False
	dw_planta.Object.plde_codigo.BackGround.Color	=	RGB(166,180,210)
	ii_planta														=	-9
ELSE
	dw_planta.Enabled											=	True
	dw_planta.Object.plde_codigo.BackGround.Color	=	RGB(255, 255, 255)
	dw_planta.SetFocus()
END IF
end event

type dw_planta from datawindow within w_info_numerales_facturacion
integer x = 791
integer y = 636
integer width = 969
integer height = 100
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

type dw_tipoprod from datawindow within w_info_numerales_facturacion
integer x = 791
integer y = 860
integer width = 1006
integer height = 100
integer taborder = 10
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_tipoproduc"
boolean border = false
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event itemchanged;
ii_TipoProd	=	integer(data)
dw_productor.GetChild("prod_codigo", idwc_productor)
idwc_productor.Retrieve(integer(data))
istr_mant.argumento[17] = String(data)


end event

event itemerror;RETURN 1
end event

type dw_productor from datawindow within w_info_numerales_facturacion
integer x = 791
integer y = 972
integer width = 1074
integer height = 88
integer taborder = 20
boolean bringtotop = true
boolean enabled = false
string title = "none"
string dataobject = "dddw_productores_tipopr"
boolean border = false
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event itemchanged;String	 ls_null
SetNull(ls_null)

IF ExisteProductor(ii_cliente,Long(data)) THEN
	ii_productor				=	integer(data)
	istr_mant.argumento[18] = String(data)
	RETURN 0
ELSE
	This.SetItem(1, "prod_codigo", ls_null)
	RETURN 1
END IF
end event

event itemerror;RETURN 1
end event

type dw_especie from datawindow within w_info_numerales_facturacion
integer x = 791
integer y = 1084
integer width = 878
integer height = 100
integer taborder = 40
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_especies"
boolean border = false
boolean livescroll = true
end type

event itemchanged;Integer li_fila

IF ExisteEspecie(ii_Cliente, Integer(data)) THEN
	ii_especie	=	integer(data)
	
	SELECT espe_nombre
		INTO :is_especie
		FROM dba.especies
		WHERE espe_codigo =	:ii_especie;
ELSE
	This.SetItem(1, "espe_codigo", gi_CodEspecie)
	ii_especie	=	gi_CodEspecie
	RETURN 1
END IF
end event

event itemerror;RETURN 1
end event

type st_5 from statictext within w_info_numerales_facturacion
integer x = 343
integer y = 764
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
long backcolor = 33543637
string text = "Periodo"
boolean focusrectangle = false
end type

type dw_periodo from datawindow within w_info_numerales_facturacion
integer x = 791
integer y = 748
integer width = 1006
integer height = 100
boolean bringtotop = true
boolean enabled = false
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

type dw_envases from datawindow within w_info_numerales_facturacion
boolean visible = false
integer x = 791
integer y = 1556
integer width = 1193
integer height = 100
integer taborder = 120
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

type st_9 from statictext within w_info_numerales_facturacion
boolean visible = false
integer x = 343
integer y = 1572
integer width = 443
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
string text = "Codigo Envase"
boolean focusrectangle = false
end type

type dw_tipoenvase from datawindow within w_info_numerales_facturacion
boolean visible = false
integer x = 791
integer y = 1448
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

type st_11 from statictext within w_info_numerales_facturacion
boolean visible = false
integer x = 347
integer y = 1464
integer width = 366
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
string text = "Tipo Envase"
boolean focusrectangle = false
end type

type cbx_tipoenvases from checkbox within w_info_numerales_facturacion
boolean visible = false
integer x = 2048
integer y = 1464
integer width = 402
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
string text = "Todos"
boolean checked = true
end type

event clicked;IF This.Checked THEN
	dw_tipoenvase.Enabled										=	False
	dw_tipoenvase.Object.enva_tipoen.BackGround.Color	=	RGB(166,180,210)
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

type dw_embalajes from uo_dw within w_info_numerales_facturacion
integer x = 791
integer y = 1328
integer width = 1193
integer height = 88
integer taborder = 80
boolean bringtotop = true
boolean enabled = false
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
			is_embalaje 				= 	Data
			istr_mant.argumento[21] = data
		END IF
		
END CHOOSE

	
end event

type st_7 from statictext within w_info_numerales_facturacion
integer x = 343
integer y = 1336
integer width = 443
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
string text = "Embalaje"
boolean focusrectangle = false
end type

type st_3 from statictext within w_info_numerales_facturacion
integer x = 347
integer y = 652
integer width = 238
integer height = 64
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
boolean enabled = false
string text = "Planta"
boolean focusrectangle = false
end type

type st_6 from statictext within w_info_numerales_facturacion
integer x = 343
integer y = 540
integer width = 233
integer height = 64
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
string text = "Cliente"
boolean focusrectangle = false
end type

type st_1 from statictext within w_info_numerales_facturacion
boolean visible = false
integer x = 343
integer y = 648
integer width = 197
integer height = 76
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 79741120
boolean enabled = false
string text = "Desde"
boolean focusrectangle = false
end type

type cbx_productor from checkbox within w_info_numerales_facturacion
integer x = 2048
integer y = 988
integer width = 402
integer height = 64
integer taborder = 30
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
string text = "Todos"
boolean checked = true
end type

event clicked;IF This.Checked THEN
	dw_productor.Enabled											=	False
	dw_productor.Object.prod_codigo.BackGround.Color	=	RGB(166,180,210)
	ii_productor													=	0
	
	istr_mant.argumento[18] 									= String(0)

ELSE
	
	dw_productor.Enabled											=	True
	dw_productor.Object.prod_codigo.BackGround.Color	=	RGB(255, 255, 255)
	dw_productor.SetFocus()
	setnull(ii_productor)

	dw_productor.GetChild("prod_codigo", idwc_productor)
	idwc_productor.Retrieve(integer(ii_TipoProd))
	
END IF
end event

type cbx_variedad from checkbox within w_info_numerales_facturacion
integer x = 2048
integer y = 1212
integer width = 402
integer height = 64
integer taborder = 70
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
string text = "Todas"
boolean checked = true
end type

event clicked;IF This.Checked THEN
	em_variedad.Enabled			=	False
	cb_buscavariedad.Enabled	=	False
	em_variedad.Text				=	''
	sle_variedad.Text				=	''
	ii_variedad						= 	-1
	istr_mant.argumento[20] 	= String(-1)
ELSE
	em_variedad.Enabled			=	True
	cb_buscavariedad.Enabled	=	True
END IF

end event

type cbx_embalajes from checkbox within w_info_numerales_facturacion
integer x = 2048
integer y = 1332
integer width = 402
integer height = 64
integer taborder = 90
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
string text = "Todos"
boolean checked = true
end type

event clicked;IF This.Checked THEN
	dw_embalajes.Enabled											=	False
	dw_embalajes.Object.emba_codigo.BackGround.Color	=	RGB(166,180,210)
	is_embalaje															= 	"z"
	
ELSE
	dw_embalajes.Enabled											=	True
	dw_embalajes.Object.emba_codigo.BackGround.Color	=	RGB(255, 255, 255)

END IF

end event

type gb_3 from groupbox within w_info_numerales_facturacion
integer x = 247
integer y = 432
integer width = 2322
integer height = 1020
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
borderstyle borderstyle = styleraised!
end type

