$PBExportHeader$w_info_produccion_prodvarzonasem.srw
forward
global type w_info_produccion_prodvarzonasem from w_para_informes
end type
type st_1 from statictext within w_info_produccion_prodvarzonasem
end type
type dw_cliente from datawindow within w_info_produccion_prodvarzonasem
end type
type st_6 from statictext within w_info_produccion_prodvarzonasem
end type
type dw_planta from datawindow within w_info_produccion_prodvarzonasem
end type
type st_3 from statictext within w_info_produccion_prodvarzonasem
end type
type cbx_planta from checkbox within w_info_produccion_prodvarzonasem
end type
type st_8 from statictext within w_info_produccion_prodvarzonasem
end type
type gb_3 from groupbox within w_info_produccion_prodvarzonasem
end type
type st_5 from statictext within w_info_produccion_prodvarzonasem
end type
type cbx_peso from checkbox within w_info_produccion_prodvarzonasem
end type
type dw_pesoneto from datawindow within w_info_produccion_prodvarzonasem
end type
type tit_peso from statictext within w_info_produccion_prodvarzonasem
end type
type st_13 from statictext within w_info_produccion_prodvarzonasem
end type
type em_semana from editmask within w_info_produccion_prodvarzonasem
end type
type st_14 from statictext within w_info_produccion_prodvarzonasem
end type
type em_ano from editmask within w_info_produccion_prodvarzonasem
end type
type st_7 from statictext within w_info_produccion_prodvarzonasem
end type
type cbx_plantascons from checkbox within w_info_produccion_prodvarzonasem
end type
type st_9 from statictext within w_info_produccion_prodvarzonasem
end type
type cbx_zonas from checkbox within w_info_produccion_prodvarzonasem
end type
type dw_zonas from datawindow within w_info_produccion_prodvarzonasem
end type
type uo_selespecie from uo_seleccion_especie within w_info_produccion_prodvarzonasem
end type
type cbx_varirotula from checkbox within w_info_produccion_prodvarzonasem
end type
type st_4 from statictext within w_info_produccion_prodvarzonasem
end type
type uo_selproductor from uo_seleccion_varios_productores_clientes within w_info_produccion_prodvarzonasem
end type
end forward

global type w_info_produccion_prodvarzonasem from w_para_informes
integer x = 14
integer y = 32
integer width = 2542
integer height = 2432
string title = "Producción Productor/Variedad/Zonas/Semanas"
boolean minbox = false
boolean maxbox = false
boolean resizable = false
windowtype windowtype = response!
string icon = "F:\Desarrollo\Producción\FrutaProcesada\Producc.ico"
event ue_imprimeresu ( )
st_1 st_1
dw_cliente dw_cliente
st_6 st_6
dw_planta dw_planta
st_3 st_3
cbx_planta cbx_planta
st_8 st_8
gb_3 gb_3
st_5 st_5
cbx_peso cbx_peso
dw_pesoneto dw_pesoneto
tit_peso tit_peso
st_13 st_13
em_semana em_semana
st_14 st_14
em_ano em_ano
st_7 st_7
cbx_plantascons cbx_plantascons
st_9 st_9
cbx_zonas cbx_zonas
dw_zonas dw_zonas
uo_selespecie uo_selespecie
cbx_varirotula cbx_varirotula
st_4 st_4
uo_selproductor uo_selproductor
end type
global w_info_produccion_prodvarzonasem w_info_produccion_prodvarzonasem

type variables
str_busqueda istr_busq
str_mant istr_mant

DataWindowChild	idwc_tipopro, idwc_cliente, idwc_planta, idwc_especie,&
						idwc_productor, idwc_packing, idwc_pesoneto, idwc_recibidor,&
						idwc_zonas

String is_NomPlanta

uo_seleccion_especie					iuo_selespecie
uo_seleccion_varios_productores	iuo_selproductor
end variables

forward prototypes
public function boolean existeespecie (integer especie)
public function boolean existepacking (integer li_planta)
public function boolean existeproductor (long ll_productor)
end prototypes

public function boolean existeespecie (integer especie);String		ls_Nombre

SELECT	espe_nombre
	INTO	:ls_Nombre
	FROM	dba.especies
	WHERE		espe_codigo	=	:Especie ;
	
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

public function boolean existepacking (integer li_planta);Integer  li_codigo

SELECT	plde_codigo
INTO    :li_Codigo
FROM	dba.plantadesp
WHERE	 plde_codigo =  :li_planta;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de tabla Planta")
	
	RETURN False
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "Código de Planta no ha sido Definido.~r~r" + &
					"Ingrese o seleccione otro Código.")
	RETURN False
ELSE
	istr_mant.argumento[7] = String(li_planta)
	RETURN True 
END IF
end function

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
	istr_mant.argumento[6] = String(ll_Productor)	
	RETURN True
END IF
end function

on w_info_produccion_prodvarzonasem.create
int iCurrent
call super::create
this.st_1=create st_1
this.dw_cliente=create dw_cliente
this.st_6=create st_6
this.dw_planta=create dw_planta
this.st_3=create st_3
this.cbx_planta=create cbx_planta
this.st_8=create st_8
this.gb_3=create gb_3
this.st_5=create st_5
this.cbx_peso=create cbx_peso
this.dw_pesoneto=create dw_pesoneto
this.tit_peso=create tit_peso
this.st_13=create st_13
this.em_semana=create em_semana
this.st_14=create st_14
this.em_ano=create em_ano
this.st_7=create st_7
this.cbx_plantascons=create cbx_plantascons
this.st_9=create st_9
this.cbx_zonas=create cbx_zonas
this.dw_zonas=create dw_zonas
this.uo_selespecie=create uo_selespecie
this.cbx_varirotula=create cbx_varirotula
this.st_4=create st_4
this.uo_selproductor=create uo_selproductor
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_1
this.Control[iCurrent+2]=this.dw_cliente
this.Control[iCurrent+3]=this.st_6
this.Control[iCurrent+4]=this.dw_planta
this.Control[iCurrent+5]=this.st_3
this.Control[iCurrent+6]=this.cbx_planta
this.Control[iCurrent+7]=this.st_8
this.Control[iCurrent+8]=this.gb_3
this.Control[iCurrent+9]=this.st_5
this.Control[iCurrent+10]=this.cbx_peso
this.Control[iCurrent+11]=this.dw_pesoneto
this.Control[iCurrent+12]=this.tit_peso
this.Control[iCurrent+13]=this.st_13
this.Control[iCurrent+14]=this.em_semana
this.Control[iCurrent+15]=this.st_14
this.Control[iCurrent+16]=this.em_ano
this.Control[iCurrent+17]=this.st_7
this.Control[iCurrent+18]=this.cbx_plantascons
this.Control[iCurrent+19]=this.st_9
this.Control[iCurrent+20]=this.cbx_zonas
this.Control[iCurrent+21]=this.dw_zonas
this.Control[iCurrent+22]=this.uo_selespecie
this.Control[iCurrent+23]=this.cbx_varirotula
this.Control[iCurrent+24]=this.st_4
this.Control[iCurrent+25]=this.uo_selproductor
end on

on w_info_produccion_prodvarzonasem.destroy
call super::destroy
destroy(this.st_1)
destroy(this.dw_cliente)
destroy(this.st_6)
destroy(this.dw_planta)
destroy(this.st_3)
destroy(this.cbx_planta)
destroy(this.st_8)
destroy(this.gb_3)
destroy(this.st_5)
destroy(this.cbx_peso)
destroy(this.dw_pesoneto)
destroy(this.tit_peso)
destroy(this.st_13)
destroy(this.em_semana)
destroy(this.st_14)
destroy(this.em_ano)
destroy(this.st_7)
destroy(this.cbx_plantascons)
destroy(this.st_9)
destroy(this.cbx_zonas)
destroy(this.dw_zonas)
destroy(this.uo_selespecie)
destroy(this.cbx_varirotula)
destroy(this.st_4)
destroy(this.uo_selproductor)
end on

event open;call super::open;Boolean lb_Cerrar

x	=	0
y	=	0

dw_cliente.GetChild("clie_codigo", idwc_cliente)
idwc_cliente.SetTransObject(SQLCA)
idwc_cliente.Retrieve()
dw_cliente.InsertRow(0)
dw_cliente.SetItem(1, "clie_codigo", gi_CodExport)

dw_planta.GetChild("plde_codigo", idwc_planta)
idwc_planta.SetTransObject(sqlca)
idwc_planta.Retrieve(1)
dw_planta.InsertRow(0)

dw_zonas.GetChild("zona_codigo", idwc_zonas)
idwc_zonas.SetTransObject(sqlca)
idwc_zonas.Retrieve()
dw_zonas.InsertRow(0)
//dw_zonas.SetItem(1, "zona_codigo", 2)

// uo_seleccion_productor
IF IsNull(uo_selproductor.Codigo) THEN lb_Cerrar = True
IF lb_Cerrar THEN
	//Close(This)
	lb_Cerrar = False
ELSE
	uo_selproductor.Seleccion(True,True)
END IF

// uo_seleccion_especie
IF IsNull(uo_selespecie.Codigo) THEN lb_Cerrar = True
IF lb_Cerrar THEN
	Close(This)
ELSE
	uo_selespecie.Seleccion(True,True)
END IF

dw_pesoneto.GetChild("enva_pesone", idwc_pesoneto)
idwc_pesoneto.SetTransObject(SQLCA)
idwc_pesoneto.Retrieve()
dw_pesoneto.InsertRow(0)
dw_pesoneto.SetItem(1, "enva_pesone", 8.20)
tit_peso.Enabled		=	False
dw_pesoneto.Enabled	=	False
dw_pesoneto.Modify("enva_pesone.BackGround.Color = " + String(RGB(166,180,210)))

em_semana.Text				=	"45"
em_ano.Text					=	String(year(gd_TempoInicio))
istr_mant.argumento[1]	= 	String(gi_CodExport) 	// cliente
istr_mant.argumento[2]	= 	"0"							//	planta
istr_mant.argumento[8]  =  "1"							//	peso
istr_mant.argumento[9]  =  em_semana.text				//	semana inicial
istr_mant.argumento[10] =  em_ano.text					//	año inicio temporada
istr_mant.argumento[12]	=	"1"							// Consolida Plantas   1 = Si
//istr_mant.argumento[13]	=	"0"							// Variedad
istr_mant.argumento[14]	=	"0"							// Zona

dw_planta.Object.plde_codigo.BackGround.Color		=	RGB(166,180,210)
dw_zonas.Object.zona_codigo.BackGround.Color			=	RGB(166,180,210)
end event

type st_computador from w_para_informes`st_computador within w_info_produccion_prodvarzonasem
end type

type st_usuario from w_para_informes`st_usuario within w_info_produccion_prodvarzonasem
end type

type st_temporada from w_para_informes`st_temporada within w_info_produccion_prodvarzonasem
end type

type p_logo from w_para_informes`p_logo within w_info_produccion_prodvarzonasem
end type

type st_titulo from w_para_informes`st_titulo within w_info_produccion_prodvarzonasem
integer width = 1847
string text = "Producción Productor/Variedad/Zonas/Semanas"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_produccion_prodvarzonasem
integer x = 2213
integer y = 1680
integer taborder = 90
end type

event pb_acepta::clicked;SetPointer(Arrow!)
 
Integer	ll_Fila, li_cliente, li_planta, li_semana, li_NroSemana, li_tipo = 1, li_semanatope, &
			li_Variedad = -1 /*Todos*/,li_varirotula
Long		ll_semana_ano, ll_productor, ll_semanatope
Date		ld_desde, ld_hasta, ld_FechaInicio, ld_Fecha, ld_FechaPrincipal
String	ls_cajas, ls_especie, ls_planta, ls_productor, ls_encabezado, ls_lista

istr_info.titulo	= 'PRODUCCION POR PRODUCTOR/VARIEDAD/ZONAS/SEMANAS'

OpenWithParm(vinf, istr_info)
vinf.dw_1.DataObject = "dw_info_produccion_semanal_01"

/*
Especies
*/
IF IsNull(uo_selespecie.Codigo)THEN
	MessageBox("Atención","Debe Seleccionar una Especie Previamente",Exclamation!)
	uo_selespecie.dw_Seleccion.SetFocus()
	RETURN
END IF

/*
productor
*/
ls_lista = uo_selproductor.Lista

IF cbx_varirotula.Checked THEN
	li_varirotula = 1
ELSE
	li_varirotula = 0
END IF

li_cliente 		=	Integer(istr_mant.argumento[1])
li_planta		=	Integer(istr_mant.argumento[2])
li_semana		=	Integer(istr_mant.argumento[9])
ll_semana_ano	=	Integer(istr_mant.argumento[10]) * 100 + li_Semana
ll_productor	=	Long(istr_mant.argumento[6])

IF	cbx_zonas.Checked	=	False	THEN
	istr_mant.argumento[14]	=	String(dw_zonas.Object.zona_codigo[1])
END IF

IF cbx_peso.Checked	=	False THEN
	ls_cajas = "Bulto"
	istr_mant.argumento[8]	=	"1"
ELSE
	istr_mant.argumento[8]	=	String(dw_pesoneto.Object.enva_pesone[1])
	ls_cajas						=	"Base " + istr_mant.argumento[8] 
END IF

DECLARE InicioSemana PROCEDURE FOR dba.FProc_InicioSemana
		@semana 		= 	:ll_Semana_Ano,
		@tipo   		= 	:li_tipo  ;
EXECUTE InicioSemana;

IF SQLCA.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca,"Asignacion Inicio de Semana")
	RETURN
ELSEIF SQLCA.SQLCode = 0 THEN
	
	FETCH InicioSemana INTO :ld_FechaInicio ;
	
	IF SQLCA.SQLCode = -1 THEN
		F_ErrorBaseDatos(sqlca,"Asignacion Inicio de Semana")
	ELSE
		CLOSE InicioSemana	;
	END IF
END IF

ld_Fecha				=	ld_FechaInicio

ld_FechaPrincipal	=	Date(em_ano.Text+'-12-31')	

IF cbx_plantascons.checked THEN
	ls_planta = 'Consolidadas'
ELSE
	IF cbx_planta.checked THEN
		ls_planta = 'Todas'
	ELSE
		SELECT plde_nombre INTO:ls_planta
		FROM DBA.plantadesp
		WHERE  plde_codigo=:li_planta;
		ls_planta = String(li_planta,'00')+" "+ls_planta
	END IF
END IF

IF ls_lista = '-9' THEN
	ls_productor = 'Consolidados'
ELSE
	IF ls_lista = '-1' THEN
		ls_productor = 'Todos'
	ELSE
		SELECT prod_nombre INTO:ls_productor
		FROM DBA.productores
		WHERE prod_codigo=:ls_lista;
		ls_productor = String(ls_lista,'00000')+" "+ls_productor
		
		IF ls_productor = '00000' THEN
			ls_productor = ls_lista
		END IF	
		
	END IF
END IF

ld_FechaPrincipal	=	gd_fecultsemana//Date(em_ano.Text+'-12-31')	//

SELECT dba.F_Semana(:ld_FechaPrincipal, 1) 
INTO :ll_semanatope
FROM dba.parempresa;
li_semanatope	=	Integer(Right(String(ll_semanatope),2))

ls_especie = String(uo_selespecie.Codigo,'00')+" "+uo_selespecie.Nombre

vinf.dw_1.SetTransObject(sqlca)

ll_Fila	=	vinf.dw_1.Retrieve(li_cliente, li_planta, uo_selespecie.Codigo,&
										 Dec(istr_mant.argumento[8]),0,li_varirotula, &
										 Integer(istr_mant.argumento[12]),li_semana,&
										 ll_semana_ano,li_Variedad,Integer(istr_mant.argumento[14]),ls_lista)

IF ll_Fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
			   	"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)

ELSEIF ll_Fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", &
	             StopSign!, Ok!)

ELSE
	F_Membrete(vinf.dw_1)

	FOR li_NroSemana = 1 TO li_semanatope
		IF li_NroSemana < 27 THEN
			ls_encabezado	=  "Semana" + String(li_NroSemana, '00') + "_t.Text = '" + &
									String(ld_Fecha, 'dd/mm/yyyy') + "~n~r" + &
									String(li_Semana, '00') + "'"
			vinf.dw_1.Modify(ls_encabezado)
		ELSE
			ls_encabezado	=	"Semana" + String(li_NroSemana, '00') + "_t.Text = '" + &
									String(li_Semana, '00') + "~n~r" + &
									String(ld_Fecha, 'dd/mm/yyyy') + "'"
			vinf.dw_1.Modify(ls_encabezado)
		END IF
		
		ld_Fecha		=	RelativeDate(ld_Fecha, 7)
		li_Semana	++
		
		IF li_Semana = li_semanatope + 1 THEN
			li_Semana	=	1	
		END IF
	NEXT

	vinf.dw_1.Modify("base.text = '" + ls_cajas + "'")
	vinf.dw_1.Modify("tit_especie.text = '" + ls_especie + "'")
	vinf.dw_1.Modify("tit_productor.text = '" + ls_productor + "'")
	vinf.dw_1.Modify("tit_planta.text = '" + ls_planta + "'")
	IF gs_Ambiente <> 'Windows' THEN
		F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
	END IF
END IF

SetPointer(Arrow!)
end event

type pb_salir from w_para_informes`pb_salir within w_info_produccion_prodvarzonasem
integer x = 2213
integer y = 1968
integer taborder = 100
end type

type st_1 from statictext within w_info_produccion_prodvarzonasem
integer x = 343
integer y = 844
integer width = 306
integer height = 76
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 553648127
boolean enabled = false
string text = "Planta"
boolean focusrectangle = false
end type

type dw_cliente from datawindow within w_info_produccion_prodvarzonasem
integer x = 786
integer y = 460
integer width = 1152
integer height = 92
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
	
	uo_selproductor.Filtra(-1,-1,Integer(data))
	
ELSE
	This.SetItem(1, "clie_codigo", ls_null)
	RETURN 1
END IF
end event

event itemerror;RETURN 1
end event

type st_6 from statictext within w_info_produccion_prodvarzonasem
integer x = 343
integer y = 472
integer width = 306
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 553648127
string text = "Cliente"
boolean focusrectangle = false
end type

type dw_planta from datawindow within w_info_produccion_prodvarzonasem
integer x = 786
integer y = 836
integer width = 969
integer height = 92
integer taborder = 30
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

li_Cliente	=	Integer(istr_mant.argumento[1])

IF ExistePlanta(li_Cliente, Integer(data), ls_Columna[]) THEN
	istr_mant.argumento[2]	=	data
	istr_mant.argumento[12]	=  "0"
ELSE
	This.SetItem(1, "plde_codigo", ls_null)
	RETURN 1
END IF
end event

event itemerror;RETURN 1
end event

type st_3 from statictext within w_info_produccion_prodvarzonasem
integer x = 343
integer y = 1340
integer width = 421
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 553648127
string text = "Especie"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type cbx_planta from checkbox within w_info_produccion_prodvarzonasem
integer x = 786
integer y = 756
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
long backcolor = 33543637
string text = "Todas"
boolean checked = true
end type

event clicked;IF This.Checked THEN
	cbx_plantascons.Enabled									=	True
	dw_planta.Enabled											=	False
	dw_planta.Object.plde_codigo.BackGround.Color	=	RGB(166,180,210)
	istr_mant.argumento[2]									=	'0'
	istr_mant.argumento[12]									=	'0'
ELSE
	cbx_plantascons.Enabled									=	False
	cbx_plantascons.Checked									=	False
	dw_planta.Enabled											=	True
	dw_planta.Object.plde_codigo.BackGround.Color	=	RGB(255, 255, 255)
	dw_planta.SetFocus()
END IF
end event

type st_8 from statictext within w_info_produccion_prodvarzonasem
integer x = 343
integer y = 1152
integer width = 306
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 553648127
string text = "Productor"
boolean focusrectangle = false
end type

type gb_3 from groupbox within w_info_produccion_prodvarzonasem
integer x = 320
integer y = 1588
integer width = 1705
integer height = 220
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 553648127
end type

type st_5 from statictext within w_info_produccion_prodvarzonasem
integer x = 247
integer y = 1272
integer width = 1847
integer height = 568
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type cbx_peso from checkbox within w_info_produccion_prodvarzonasem
integer x = 384
integer y = 1672
integer width = 631
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
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

type dw_pesoneto from datawindow within w_info_produccion_prodvarzonasem
integer x = 1280
integer y = 1668
integer width = 695
integer height = 92
integer taborder = 60
boolean bringtotop = true
string dataobject = "dddw_pesoneto"
boolean border = false
boolean livescroll = true
end type

event itemerror;RETURN 1
end event

type tit_peso from statictext within w_info_produccion_prodvarzonasem
integer x = 1111
integer y = 1656
integer width = 160
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 553648127
string text = "Peso"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_13 from statictext within w_info_produccion_prodvarzonasem
integer x = 343
integer y = 1912
integer width = 434
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 553648127
string text = "Semana Inicio"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type em_semana from editmask within w_info_produccion_prodvarzonasem
integer x = 1280
integer y = 1900
integer width = 251
integer height = 92
integer taborder = 70
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
string text = "45"
alignment alignment = center!
borderstyle borderstyle = stylelowered!
string mask = "00"
boolean spin = true
string displaydata = "~t/"
double increment = 1
string minmax = "1~~52"
end type

event modified;istr_mant.argumento[9]  =  This.text	
end event

type st_14 from statictext within w_info_produccion_prodvarzonasem
integer x = 343
integer y = 2028
integer width = 773
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 553648127
string text = "Año Inicio de Temporada"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type em_ano from editmask within w_info_produccion_prodvarzonasem
integer x = 1280
integer y = 2016
integer width = 334
integer height = 92
integer taborder = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
alignment alignment = center!
borderstyle borderstyle = stylelowered!
string mask = "0000"
boolean spin = true
string displaydata = "~t/"
double increment = 1
string minmax = "~~"
end type

event modified;call super::modified;istr_mant.argumento[10]  =  This.text	
end event

type st_7 from statictext within w_info_produccion_prodvarzonasem
integer x = 247
integer y = 1840
integer width = 1847
integer height = 336
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type cbx_plantascons from checkbox within w_info_produccion_prodvarzonasem
integer x = 1262
integer y = 756
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
long backcolor = 33543637
string text = "Consolidadas"
boolean checked = true
end type

event clicked;IF This.Checked = TRUE THEN
	istr_mant.argumento[12]	=	'1'
ELSE
	istr_mant.argumento[12]	=	'0'
END IF
	
end event

type st_9 from statictext within w_info_produccion_prodvarzonasem
integer x = 343
integer y = 656
integer width = 306
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 553648127
string text = "Zona"
boolean focusrectangle = false
end type

type cbx_zonas from checkbox within w_info_produccion_prodvarzonasem
integer x = 786
integer y = 564
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
long backcolor = 33543637
string text = "Todas"
boolean checked = true
end type

event clicked;IF This.Checked THEN
//	cbx_zonascons.Enabled									=	True
	dw_zonas.Enabled											=	False
	dw_zonas.Object.zona_codigo.BackGround.Color		=	RGB(166,180,210)
	istr_mant.argumento[14]									=	'0'
//	istr_mant.argumento[20]									=	'0'
ELSE
//	cbx_zonascons.Enabled									=	False
//	cbx_zonascons.Checked									=	False
	dw_zonas.Enabled											=	True
	dw_zonas.Object.zona_codigo.BackGround.Color		=	RGB(255, 255, 255)
	dw_zonas.SetFocus()
//	istr_mant.argumento[20]									=	'0'
END IF
end event

type dw_zonas from datawindow within w_info_produccion_prodvarzonasem
integer x = 782
integer y = 644
integer width = 873
integer height = 92
integer taborder = 20
boolean bringtotop = true
boolean enabled = false
string title = "none"
string dataobject = "dddw_zonas"
boolean border = false
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event itemchanged;istr_mant.argumento[14]	=	data
istr_mant.argumento[20]	=	'0'
end event

event itemerror;RETURN 1
end event

type uo_selespecie from uo_seleccion_especie within w_info_produccion_prodvarzonasem
event destroy ( )
integer x = 786
integer y = 1292
integer height = 180
integer taborder = 70
boolean bringtotop = true
end type

on uo_selespecie.destroy
call uo_seleccion_especie::destroy
end on

type cbx_varirotula from checkbox within w_info_produccion_prodvarzonasem
integer x = 786
integer y = 1508
integer width = 640
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
string text = "Variedad Rotulada"
end type

type st_4 from statictext within w_info_produccion_prodvarzonasem
integer x = 247
integer y = 440
integer width = 1847
integer height = 832
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type uo_selproductor from uo_seleccion_varios_productores_clientes within w_info_produccion_prodvarzonasem
integer x = 795
integer y = 948
integer taborder = 40
boolean bringtotop = true
end type

event dragdrop;call super::dragdrop;uo_selproductor.Filtra(-1,-1,dw_cliente.Object.clie_Codigo[1])
end event

on uo_selproductor.destroy
call uo_seleccion_varios_productores_clientes::destroy
end on

