$PBExportHeader$w_info_cuadra_tempo_prod_embq.srw
forward
global type w_info_cuadra_tempo_prod_embq from w_para_informes
end type
type gb_5 from groupbox within w_info_cuadra_tempo_prod_embq
end type
type gb_4 from groupbox within w_info_cuadra_tempo_prod_embq
end type
type st_4 from statictext within w_info_cuadra_tempo_prod_embq
end type
type st_1 from statictext within w_info_cuadra_tempo_prod_embq
end type
type st_2 from statictext within w_info_cuadra_tempo_prod_embq
end type
type em_desde from editmask within w_info_cuadra_tempo_prod_embq
end type
type dw_cliente from datawindow within w_info_cuadra_tempo_prod_embq
end type
type st_6 from statictext within w_info_cuadra_tempo_prod_embq
end type
type dw_zona from datawindow within w_info_cuadra_tempo_prod_embq
end type
type st_3 from statictext within w_info_cuadra_tempo_prod_embq
end type
type cbx_planta from checkbox within w_info_cuadra_tempo_prod_embq
end type
type st_7 from statictext within w_info_cuadra_tempo_prod_embq
end type
type em_hasta from editmask within w_info_cuadra_tempo_prod_embq
end type
type st_8 from statictext within w_info_cuadra_tempo_prod_embq
end type
type dw_productor from datawindow within w_info_cuadra_tempo_prod_embq
end type
type cbx_productor from checkbox within w_info_cuadra_tempo_prod_embq
end type
type st_9 from statictext within w_info_cuadra_tempo_prod_embq
end type
type cbx_packing from checkbox within w_info_cuadra_tempo_prod_embq
end type
type dw_packing from datawindow within w_info_cuadra_tempo_prod_embq
end type
type gb_3 from groupbox within w_info_cuadra_tempo_prod_embq
end type
type st_5 from statictext within w_info_cuadra_tempo_prod_embq
end type
type cbx_1 from checkbox within w_info_cuadra_tempo_prod_embq
end type
type dw_pesoneto from datawindow within w_info_cuadra_tempo_prod_embq
end type
type tit_peso from statictext within w_info_cuadra_tempo_prod_embq
end type
type st_variedad from statictext within w_info_cuadra_tempo_prod_embq
end type
type dw_planta from datawindow within w_info_cuadra_tempo_prod_embq
end type
type rb_1 from radiobutton within w_info_cuadra_tempo_prod_embq
end type
type rb_2 from radiobutton within w_info_cuadra_tempo_prod_embq
end type
type st_11 from statictext within w_info_cuadra_tempo_prod_embq
end type
type st_10 from statictext within w_info_cuadra_tempo_prod_embq
end type
type rb_3 from radiobutton within w_info_cuadra_tempo_prod_embq
end type
type rb_4 from radiobutton within w_info_cuadra_tempo_prod_embq
end type
type st_12 from statictext within w_info_cuadra_tempo_prod_embq
end type
type st_14 from statictext within w_info_cuadra_tempo_prod_embq
end type
type cbx_embarque from checkbox within w_info_cuadra_tempo_prod_embq
end type
type dw_embarques from datawindow within w_info_cuadra_tempo_prod_embq
end type
type cbx_consembarque from checkbox within w_info_cuadra_tempo_prod_embq
end type
type cbx_consproductor from checkbox within w_info_cuadra_tempo_prod_embq
end type
type st_13 from statictext within w_info_cuadra_tempo_prod_embq
end type
type cbx_2 from checkbox within w_info_cuadra_tempo_prod_embq
end type
type dw_planta2 from datawindow within w_info_cuadra_tempo_prod_embq
end type
type st_15 from statictext within w_info_cuadra_tempo_prod_embq
end type
type cbx_varrot from checkbox within w_info_cuadra_tempo_prod_embq
end type
type cbx_consplanta from checkbox within w_info_cuadra_tempo_prod_embq
end type
type uo_selespecie from uo_seleccion_especie within w_info_cuadra_tempo_prod_embq
end type
type uo_selvariedad from uo_seleccion_variedad within w_info_cuadra_tempo_prod_embq
end type
end forward

global type w_info_cuadra_tempo_prod_embq from w_para_informes
integer x = 14
integer y = 32
integer width = 2633
integer height = 2412
string title = "Cuadratura Producción v/s Embarques"
boolean minbox = false
boolean maxbox = false
boolean resizable = false
windowtype windowtype = response!
string icon = "F:\Desarrollo\Producción\FrutaProcesada\Producc.ico"
gb_5 gb_5
gb_4 gb_4
st_4 st_4
st_1 st_1
st_2 st_2
em_desde em_desde
dw_cliente dw_cliente
st_6 st_6
dw_zona dw_zona
st_3 st_3
cbx_planta cbx_planta
st_7 st_7
em_hasta em_hasta
st_8 st_8
dw_productor dw_productor
cbx_productor cbx_productor
st_9 st_9
cbx_packing cbx_packing
dw_packing dw_packing
gb_3 gb_3
st_5 st_5
cbx_1 cbx_1
dw_pesoneto dw_pesoneto
tit_peso tit_peso
st_variedad st_variedad
dw_planta dw_planta
rb_1 rb_1
rb_2 rb_2
st_11 st_11
st_10 st_10
rb_3 rb_3
rb_4 rb_4
st_12 st_12
st_14 st_14
cbx_embarque cbx_embarque
dw_embarques dw_embarques
cbx_consembarque cbx_consembarque
cbx_consproductor cbx_consproductor
st_13 st_13
cbx_2 cbx_2
dw_planta2 dw_planta2
st_15 st_15
cbx_varrot cbx_varrot
cbx_consplanta cbx_consplanta
uo_selespecie uo_selespecie
uo_selvariedad uo_selvariedad
end type
global w_info_cuadra_tempo_prod_embq w_info_cuadra_tempo_prod_embq

type variables
str_busqueda istr_busq
str_mant istr_mant

DataWindowChild	idwc_zona,idwc_tipopro, idwc_cliente, idwc_planta, &
						idwc_productor, idwc_packing, idwc_pesoneto, idwc_embarque

String is_NomPlanta

uo_seleccion_especie		iuo_selespecie
uo_seleccion_variedad	iuo_selvariedad
end variables

forward prototypes
public function boolean existepacking (integer li_cliente, ref string ls_columna)
public function boolean existeespecie (integer cliente, integer especie)
public function boolean existeproductor (long ll_productor)
public function boolean existeplanta (string ls_columna)
end prototypes

public function boolean existepacking (integer li_cliente, ref string ls_columna);Integer  li_codigo

SELECT	plde_codigo
INTO    :li_Codigo
FROM	dba.plantadesp
WHERE	plde_codigo =  :ls_Columna;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de tabla Planta")
	
	RETURN False
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "Código de Planta no ha sido Definido.~r~r" + &
					"Ingrese o seleccione otro Código.")
	RETURN False
ELSE
	istr_mant.argumento[7] = ls_columna	
	RETURN True 
END IF
end function

public function boolean existeespecie (integer cliente, integer especie);String		ls_Nombre

SELECT	espe_nombre
	INTO	:ls_Nombre
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

public function boolean existeproductor (long ll_productor);String	ls_Nombre
Integer	li_cliente

li_cliente	= dw_cliente.Object.clie_Codigo[1]

SELECT	prod_nombre
	INTO	:ls_Nombre
	FROM	dba.productores as pro,dba.productoresclientes as cli
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
	istr_mant.argumento[6] = String(ll_Productor)	
	RETURN True
END IF
end function

public function boolean existeplanta (string ls_columna);Integer  li_codigo

li_codigo	=	Integer(ls_Columna)

SELECT	plde_codigo
INTO    :li_Codigo
FROM	dba.plantadesp
WHERE	plde_codigo =  :li_codigo;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de tabla Planta")
	
	RETURN False
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "Código de Planta no ha sido Definido.~r~r" + &
					"Ingrese o seleccione otro Código.")
	RETURN False
ELSE
	istr_mant.argumento[21] = ls_columna	
	RETURN True 
END IF
end function

on w_info_cuadra_tempo_prod_embq.create
int iCurrent
call super::create
this.gb_5=create gb_5
this.gb_4=create gb_4
this.st_4=create st_4
this.st_1=create st_1
this.st_2=create st_2
this.em_desde=create em_desde
this.dw_cliente=create dw_cliente
this.st_6=create st_6
this.dw_zona=create dw_zona
this.st_3=create st_3
this.cbx_planta=create cbx_planta
this.st_7=create st_7
this.em_hasta=create em_hasta
this.st_8=create st_8
this.dw_productor=create dw_productor
this.cbx_productor=create cbx_productor
this.st_9=create st_9
this.cbx_packing=create cbx_packing
this.dw_packing=create dw_packing
this.gb_3=create gb_3
this.st_5=create st_5
this.cbx_1=create cbx_1
this.dw_pesoneto=create dw_pesoneto
this.tit_peso=create tit_peso
this.st_variedad=create st_variedad
this.dw_planta=create dw_planta
this.rb_1=create rb_1
this.rb_2=create rb_2
this.st_11=create st_11
this.st_10=create st_10
this.rb_3=create rb_3
this.rb_4=create rb_4
this.st_12=create st_12
this.st_14=create st_14
this.cbx_embarque=create cbx_embarque
this.dw_embarques=create dw_embarques
this.cbx_consembarque=create cbx_consembarque
this.cbx_consproductor=create cbx_consproductor
this.st_13=create st_13
this.cbx_2=create cbx_2
this.dw_planta2=create dw_planta2
this.st_15=create st_15
this.cbx_varrot=create cbx_varrot
this.cbx_consplanta=create cbx_consplanta
this.uo_selespecie=create uo_selespecie
this.uo_selvariedad=create uo_selvariedad
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.gb_5
this.Control[iCurrent+2]=this.gb_4
this.Control[iCurrent+3]=this.st_4
this.Control[iCurrent+4]=this.st_1
this.Control[iCurrent+5]=this.st_2
this.Control[iCurrent+6]=this.em_desde
this.Control[iCurrent+7]=this.dw_cliente
this.Control[iCurrent+8]=this.st_6
this.Control[iCurrent+9]=this.dw_zona
this.Control[iCurrent+10]=this.st_3
this.Control[iCurrent+11]=this.cbx_planta
this.Control[iCurrent+12]=this.st_7
this.Control[iCurrent+13]=this.em_hasta
this.Control[iCurrent+14]=this.st_8
this.Control[iCurrent+15]=this.dw_productor
this.Control[iCurrent+16]=this.cbx_productor
this.Control[iCurrent+17]=this.st_9
this.Control[iCurrent+18]=this.cbx_packing
this.Control[iCurrent+19]=this.dw_packing
this.Control[iCurrent+20]=this.gb_3
this.Control[iCurrent+21]=this.st_5
this.Control[iCurrent+22]=this.cbx_1
this.Control[iCurrent+23]=this.dw_pesoneto
this.Control[iCurrent+24]=this.tit_peso
this.Control[iCurrent+25]=this.st_variedad
this.Control[iCurrent+26]=this.dw_planta
this.Control[iCurrent+27]=this.rb_1
this.Control[iCurrent+28]=this.rb_2
this.Control[iCurrent+29]=this.st_11
this.Control[iCurrent+30]=this.st_10
this.Control[iCurrent+31]=this.rb_3
this.Control[iCurrent+32]=this.rb_4
this.Control[iCurrent+33]=this.st_12
this.Control[iCurrent+34]=this.st_14
this.Control[iCurrent+35]=this.cbx_embarque
this.Control[iCurrent+36]=this.dw_embarques
this.Control[iCurrent+37]=this.cbx_consembarque
this.Control[iCurrent+38]=this.cbx_consproductor
this.Control[iCurrent+39]=this.st_13
this.Control[iCurrent+40]=this.cbx_2
this.Control[iCurrent+41]=this.dw_planta2
this.Control[iCurrent+42]=this.st_15
this.Control[iCurrent+43]=this.cbx_varrot
this.Control[iCurrent+44]=this.cbx_consplanta
this.Control[iCurrent+45]=this.uo_selespecie
this.Control[iCurrent+46]=this.uo_selvariedad
end on

on w_info_cuadra_tempo_prod_embq.destroy
call super::destroy
destroy(this.gb_5)
destroy(this.gb_4)
destroy(this.st_4)
destroy(this.st_1)
destroy(this.st_2)
destroy(this.em_desde)
destroy(this.dw_cliente)
destroy(this.st_6)
destroy(this.dw_zona)
destroy(this.st_3)
destroy(this.cbx_planta)
destroy(this.st_7)
destroy(this.em_hasta)
destroy(this.st_8)
destroy(this.dw_productor)
destroy(this.cbx_productor)
destroy(this.st_9)
destroy(this.cbx_packing)
destroy(this.dw_packing)
destroy(this.gb_3)
destroy(this.st_5)
destroy(this.cbx_1)
destroy(this.dw_pesoneto)
destroy(this.tit_peso)
destroy(this.st_variedad)
destroy(this.dw_planta)
destroy(this.rb_1)
destroy(this.rb_2)
destroy(this.st_11)
destroy(this.st_10)
destroy(this.rb_3)
destroy(this.rb_4)
destroy(this.st_12)
destroy(this.st_14)
destroy(this.cbx_embarque)
destroy(this.dw_embarques)
destroy(this.cbx_consembarque)
destroy(this.cbx_consproductor)
destroy(this.st_13)
destroy(this.cbx_2)
destroy(this.dw_planta2)
destroy(this.st_15)
destroy(this.cbx_varrot)
destroy(this.cbx_consplanta)
destroy(this.uo_selespecie)
destroy(this.uo_selvariedad)
end on

event open;Boolean lb_Cerrar

x	=	0
y	=	0

dw_cliente.GetChild("clie_codigo", idwc_cliente)
idwc_cliente.SetTransObject(SQLCA)
idwc_cliente.Retrieve(gi_CodExport)
dw_cliente.InsertRow(0)
dw_cliente.SetItem(1, "clie_codigo", gi_CodExport)

dw_zona.GetChild("zona_codigo", idwc_zona)
idwc_zona.SetTransObject(sqlca)
idwc_zona.Retrieve()
dw_zona.InsertRow(0)
dw_zona.SetItem(1, "zona_codigo", 2)

dw_productor.GetChild("prod_codigo", idwc_productor)
idwc_productor.SetTransObject(sqlca)
idwc_productor.Retrieve(gi_CodExport)
dw_productor.InsertRow(0)

dw_planta2.GetChild("plde_codigo", idwc_planta)
idwc_planta.SetTransObject(sqlca)
idwc_planta.Retrieve(1)
dw_planta2.InsertRow(0)
dw_planta2.Enabled			=	False

dw_packing.GetChild("plde_codigo", idwc_packing)
idwc_packing.SetTransObject(sqlca)
idwc_packing.Retrieve(2)
dw_packing.InsertRow(0)

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

dw_embarques.GetChild("embq_codigo", idwc_embarque)
idwc_embarque.SetTransObject(sqlca)
idwc_embarque.Retrieve(gi_CodExport)
dw_embarques.InsertRow(0)

dw_pesoneto.GetChild("enva_pesone", idwc_pesoneto)
idwc_pesoneto.SetTransObject(SQLCA)
idwc_pesoneto.Retrieve(0)
dw_pesoneto.InsertRow(0)
dw_pesoneto.SetItem(1, "enva_pesone", 820/100)
tit_peso.Enabled		=	False
dw_pesoneto.Enabled	=	False
dw_pesoneto.Modify("enva_pesone.BackGround.Color = " + String(RGB(166,180,210)))

em_desde.Text				=	String(RelativeDate(Today(), -365))
em_hasta.Text				=	String(Today())
istr_mant.argumento[1]	= 	String(gi_CodExport) 	// cliente
istr_mant.argumento[2]	= 	"2"							//	zona
istr_mant.argumento[3]	= 	em_desde.Text				//	fecha inicio
istr_mant.argumento[4]	=	em_hasta.Text				//	fecha final
//istr_mant.argumento[5]	=	String(gi_CodEspecie)	//	especie
istr_mant.argumento[6]  =  "-9"							//	productor
istr_mant.argumento[7]  =  ""								//	packing
istr_mant.argumento[8]  =  "1"							//	peso
//istr_mant.argumento[9]	= 	"-9" 							// variedad
istr_mant.argumento[10]	= 	"1" 							// seleccion
istr_mant.argumento[11]	= 	"1" 							// Tipo Bulto
istr_mant.argumento[12]	= 	"-9" 							// Embarque
istr_mant.argumento[21]	=	'-9'							// Planta Despacho
end event

event resize;//
end event

type st_computador from w_para_informes`st_computador within w_info_cuadra_tempo_prod_embq
end type

type st_usuario from w_para_informes`st_usuario within w_info_cuadra_tempo_prod_embq
end type

type st_temporada from w_para_informes`st_temporada within w_info_cuadra_tempo_prod_embq
end type

type p_logo from w_para_informes`p_logo within w_info_cuadra_tempo_prod_embq
end type

type st_titulo from w_para_informes`st_titulo within w_info_cuadra_tempo_prod_embq
integer width = 1847
string text = "Cuadraturas"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_cuadra_tempo_prod_embq
string tag = "Imprimir Reporte"
integer x = 2231
integer y = 1688
integer taborder = 250
string powertiptext = "Imprimir Reporte"
end type

event pb_acepta::clicked;SetPointer(Arrow!)

Integer	fila, li_cliente, li_zona, li_especie, li_varrot
String	ls_cliente, ls_zona, ls_embarque

IF istr_mant.argumento[10] = '1' THEN 
	istr_info.titulo	= 'CUADRATURA PRODUCCION V/S EMBARCADO COMERCIO EXTERIOR'
ELSE
	istr_info.titulo	= 'CUADRATURA EMBARQUES CENTRAL V/S EMBARQUES ZONAS'
END IF 	

OpenWithParm(vinf, istr_info)
IF istr_mant.argumento[10] = '1' THEN 
	IF istr_mant.argumento[11] = '1' THEN
		vinf.dw_1.DataObject = "dw_info_cuadra_tempo_prod_embq_env"
	ELSE
		vinf.dw_1.DataObject = "dw_info_cuadra_tempo_prod_embq"
	END IF
	
ELSE 
	IF istr_mant.argumento[11] = '1' THEN
		vinf.dw_1.DataObject = "dw_info_comparativo_embarquesenvase"  //
	ELSE
		vinf.dw_1.DataObject = "dw_info_comparativo_embarques"  //
	END IF
END IF

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

li_cliente 		=	Integer(istr_mant.argumento[1])
li_zona			=	Integer(istr_mant.argumento[2])
ls_embarque		=  istr_mant.argumento[12]

Select clie_nombre into :ls_cliente
from dba.clientesprod
where clie_codigo=:li_cliente;

Select zona_nombre into :ls_zona
from dba.zonas
where zona_codigo=:li_zona;

IF isnull(ls_embarque) OR ls_embarque = '' THEN
	MessageBox( "Atención", "Falta Parametro Embarque.", &
	             StopSign!, Ok!)
	dw_embarques.SetFocus()
	Return 1
END IF	

IF cbx_varrot.Checked THEN
	li_varrot	=	1
ELSE
	li_varrot	=	0
END IF

vinf.dw_1.SetTransObject(sqlca)

fila	=	vinf.dw_1.Retrieve(li_cliente, Integer(istr_mant.argumento[2]),uo_selespecie.Codigo, &
									 Long(istr_mant.argumento[6]),uo_selvariedad.Codigo,Date(istr_mant.argumento[3]),&
									 Date(istr_mant.argumento[4]),ls_embarque,Integer(istr_mant.argumento[21]),li_varrot)

IF fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
			   	"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)

ELSEIF fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", &
	             StopSign!, Ok!)

ELSE
	   F_Membrete(vinf.dw_1)
		IF istr_mant.argumento[10] = '1' THEN
			vinf.dw_1.Modify("Deszona.text = '" + ls_zona + "'")
			vinf.dw_1.Modify("Deszona2.text = '" + ls_zona + "'")
			vinf.dw_1.Modify("Descliente.text = '" + ls_cliente + "'")
		END IF		
		IF istr_mant.argumento[10] = '2' THEN
			vinf.dw_1.Modify("desde.text = '" + String(Date(istr_mant.argumento[3])) + "'")
			vinf.dw_1.Modify("hasta.text = '" + String(Date(istr_mant.argumento[4])) + "'")
		END IF				
	IF gs_Ambiente <> 'Windows' THEN
		F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
	END IF
END IF
SetPointer(Arrow!)				
end event

type pb_salir from w_para_informes`pb_salir within w_info_cuadra_tempo_prod_embq
string tag = "Salir [Cerrar Ventana Activa]"
integer x = 2226
integer y = 1976
integer taborder = 270
string powertiptext = "Salir [Cerrar Ventana Activa]"
end type

type gb_5 from groupbox within w_info_cuadra_tempo_prod_embq
integer x = 325
integer y = 448
integer width = 1691
integer height = 228
integer taborder = 10
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
end type

type gb_4 from groupbox within w_info_cuadra_tempo_prod_embq
integer x = 293
integer y = 2004
integer width = 882
integer height = 148
integer taborder = 140
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
end type

type st_4 from statictext within w_info_cuadra_tempo_prod_embq
integer x = 251
integer y = 700
integer width = 1847
integer height = 592
boolean bringtotop = true
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

type st_1 from statictext within w_info_cuadra_tempo_prod_embq
integer x = 347
integer y = 832
integer width = 462
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
boolean enabled = false
string text = "Zona"
boolean focusrectangle = false
end type

type st_2 from statictext within w_info_cuadra_tempo_prod_embq
integer x = 347
integer y = 1720
integer width = 389
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
boolean enabled = false
string text = "Fecha Inicio"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type em_desde from editmask within w_info_cuadra_tempo_prod_embq
integer x = 791
integer y = 1688
integer width = 393
integer height = 96
integer taborder = 120
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
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
end type

event modified;istr_mant.argumento[3]	=	This.Text
end event

type dw_cliente from datawindow within w_info_cuadra_tempo_prod_embq
integer x = 791
integer y = 720
integer width = 1243
integer height = 96
integer taborder = 20
boolean bringtotop = true
string dataobject = "dddw_clientesprod"
boolean border = false
boolean livescroll = true
end type

event itemchanged;String	ls_Columna[], ls_null

SetNull(ls_null)

IF ExisteCliente(Integer(data), ls_Columna[]) THEN
	istr_mant.argumento[1]	=	data
	
	dw_productor.GetChild("prod_codigo", idwc_productor)
	idwc_productor.SetTransObject(sqlca)
	idwc_productor.Retrieve(Integer(data))
	dw_productor.InsertRow(0)

ELSE
	This.SetItem(1, "clie_codigo", ls_null)
	RETURN 1
END IF
end event

event itemerror;RETURN 1
end event

type st_6 from statictext within w_info_cuadra_tempo_prod_embq
integer x = 343
integer y = 732
integer width = 233
integer height = 64
boolean bringtotop = true
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

type dw_zona from datawindow within w_info_cuadra_tempo_prod_embq
integer x = 791
integer y = 824
integer width = 878
integer height = 92
integer taborder = 30
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_zonas"
boolean border = false
boolean livescroll = true
end type

event itemchanged;istr_mant.argumento[2]	=	data

end event

event itemerror;RETURN 1
end event

type st_3 from statictext within w_info_cuadra_tempo_prod_embq
integer x = 347
integer y = 1328
integer width = 421
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

type cbx_planta from checkbox within w_info_cuadra_tempo_prod_embq
boolean visible = false
integer x = 2272
integer y = 2028
integer width = 402
integer height = 76
integer taborder = 210
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
	dw_planta.Enabled			=	False
	istr_mant.argumento[2]	=	'0'
ELSE
	dw_planta.Enabled			=	True
	dw_planta.SetFocus()
END IF
end event

type st_7 from statictext within w_info_cuadra_tempo_prod_embq
boolean visible = false
integer x = 2725
integer y = 2060
integer width = 297
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Término"
boolean focusrectangle = false
end type

type em_hasta from editmask within w_info_cuadra_tempo_prod_embq
integer x = 1563
integer y = 1688
integer width = 393
integer height = 96
integer taborder = 130
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
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
end type

event modified;istr_mant.argumento[4]	=	This.Text
end event

type st_8 from statictext within w_info_cuadra_tempo_prod_embq
integer x = 347
integer y = 1184
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
long backcolor = 33543637
string text = "Productor"
boolean focusrectangle = false
end type

type dw_productor from datawindow within w_info_cuadra_tempo_prod_embq
integer x = 791
integer y = 1176
integer width = 1138
integer height = 92
integer taborder = 70
boolean bringtotop = true
boolean enabled = false
string title = "none"
string dataobject = "dddw_productores_clientes"
boolean border = false
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event itemerror;RETURN 1
end event

event itemchanged;String	 ls_null
SetNull(ls_null)

	IF ExisteProductor(Long(data)) THEN
	istr_mant.argumento[6]	=	data	
	RETURN 0
ELSE
	This.SetItem(1, "prod_codigo", ls_null)
	RETURN 1
END IF
end event

type cbx_productor from checkbox within w_info_cuadra_tempo_prod_embq
integer x = 791
integer y = 1096
integer width = 402
integer height = 80
integer taborder = 60
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
boolean enabled = false
string text = "Todos"
boolean checked = true
end type

event clicked;IF cbx_productor.Checked = TRUE THEN
	dw_productor.Enabled  = False
//	dw_packing.Enabled    = False
//	cbx_packing.Checked   = False
	dw_productor.Reset()
	dw_productor.insertrow(0)
	istr_mant.argumento[6]	=	'-1'	
	cbx_consproductor.Enabled = True
	
	dw_productor.Reset()
	dw_productor.GetChild("prod_codigo", idwc_productor)
	idwc_productor.SetTransObject(sqlca)
	idwc_productor.Retrieve()
	dw_productor.InsertRow(0)
ELSE
	cbx_consproductor.Enabled = False
	
	dw_productor.Enabled  = True
	dw_productor.SetFocus()
	dw_productor.Reset()
	dw_productor.InsertRow(0)
//	dw_packing.Enabled    = True
//	cbx_packing.Enabled   = True
//	dw_packing.Reset()
//	dw_packing.InsertRow(0)	
END IF
	
	
	
end event

type st_9 from statictext within w_info_cuadra_tempo_prod_embq
boolean visible = false
integer x = 2313
integer y = 1944
integer width = 238
integer height = 68
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = "Packing"
alignment alignment = right!
boolean focusrectangle = false
end type

type cbx_packing from checkbox within w_info_cuadra_tempo_prod_embq
boolean visible = false
integer x = 2501
integer y = 2004
integer width = 402
integer height = 80
integer taborder = 230
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

event clicked;IF This.checked = True THEN
	dw_packing.Enabled = False
	istr_mant.argumento[7]	=	'0'
ELSE
	dw_packing.Enabled=True	
END IF	
end event

type dw_packing from datawindow within w_info_cuadra_tempo_prod_embq
boolean visible = false
integer x = 2487
integer y = 1964
integer width = 965
integer height = 100
integer taborder = 240
boolean bringtotop = true
boolean enabled = false
string title = "none"
string dataobject = "dddw_plantadesp_tipo"
boolean border = false
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event itemchanged;String	 ls_null
SetNull(ls_null)

	IF ExistePacking(Integer(istr_mant.argumento[1]),data)THEN
		istr_mant.argumento[7]	=	data		
		RETURN 0
	ELSE
		This.SetItem(1, "plde_codigo", ls_null)
		RETURN 1
	END IF
end event

type gb_3 from groupbox within w_info_cuadra_tempo_prod_embq
boolean visible = false
integer x = 2181
integer y = 1828
integer width = 1614
integer height = 280
integer taborder = 260
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
end type

type st_5 from statictext within w_info_cuadra_tempo_prod_embq
integer x = 251
integer y = 1288
integer width = 1847
integer height = 708
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

type cbx_1 from checkbox within w_info_cuadra_tempo_prod_embq
boolean visible = false
integer x = 2487
integer y = 1856
integer width = 631
integer height = 80
boolean bringtotop = true
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
	dw_pesoneto.Modify("enva_pesone.BackGround.Color = " + String(RGB(166,180,210)))
END IF

end event

type dw_pesoneto from datawindow within w_info_cuadra_tempo_prod_embq
boolean visible = false
integer x = 2670
integer y = 1760
integer width = 544
integer height = 84
integer taborder = 190
boolean bringtotop = true
string dataobject = "dddw_pesoneto"
boolean border = false
boolean livescroll = true
end type

event itemerror;RETURN 1
end event

type tit_peso from statictext within w_info_cuadra_tempo_prod_embq
boolean visible = false
integer x = 2450
integer y = 2080
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

type st_variedad from statictext within w_info_cuadra_tempo_prod_embq
integer x = 347
integer y = 1496
integer width = 279
integer height = 84
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

type dw_planta from datawindow within w_info_cuadra_tempo_prod_embq
boolean visible = false
integer x = 2487
integer y = 1772
integer width = 1001
integer height = 92
integer taborder = 220
boolean bringtotop = true
boolean enabled = false
string title = "none"
string dataobject = "dddw_plantadesp"
boolean border = false
boolean livescroll = true
end type

event itemchanged;call super::itemchanged;istr_mant.argumento[2]	=	data

end event

event itemerror;RETURN 1
end event

type rb_1 from radiobutton within w_info_cuadra_tempo_prod_embq
integer x = 521
integer y = 508
integer width = 1193
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
string text = "Producción v/s Embarques Central"
boolean checked = true
end type

event clicked;istr_mant.argumento[10]	= 	"1"

IF This.Checked THEN
	st_2.Enabled					=	False
	st_12.Enabled					=	False
	em_desde.Enabled				=	False
	em_hasta.Enabled				=	False
	cbx_consembarque.Enabled 	=	False
	dw_embarques.Enabled			=  False
	cbx_embarque.Enabled			= 	False
	cbx_consembarque.Checked 	= 	True
	cbx_embarque.Checked			=	True
	
	dw_embarques.reset()
	dw_embarques.GetChild("embq_codigo", idwc_embarque)
	idwc_embarque.SetTransObject(sqlca)
	idwc_embarque.Retrieve(gi_CodExport)
	dw_embarques.InsertRow(0)	
END IF

end event

type rb_2 from radiobutton within w_info_cuadra_tempo_prod_embq
integer x = 521
integer y = 576
integer width = 1317
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
string text = "Embarques Central v/s Embarques Zonas"
end type

event clicked;call super::clicked;istr_mant.argumento[10]	= 	"2"

IF This.Checked THEN
	st_2.Enabled					=	True
	st_12.Enabled					=	True
	em_desde.Enabled				=	True
	em_hasta.Enabled				=	True	
	cbx_consembarque.Enabled 	=	True
//		
//	istr_mant.argumento[20] = String(date('1900-01-01'))
//	istr_mant.argumento[21] = String(Today())
END IF

end event

type st_11 from statictext within w_info_cuadra_tempo_prod_embq
integer x = 251
integer y = 440
integer width = 1847
integer height = 260
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

type st_10 from statictext within w_info_cuadra_tempo_prod_embq
integer x = 251
integer y = 1996
integer width = 969
integer height = 184
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

type rb_3 from radiobutton within w_info_cuadra_tempo_prod_embq
integer x = 754
integer y = 2056
integer width = 402
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
string text = "Embalaje"
end type

event clicked;//call super::clicked;istr_mant.argumento[11]	= 	"2"
istr_mant.argumento[11]	= 	"2"
end event

type rb_4 from radiobutton within w_info_cuadra_tempo_prod_embq
integer x = 352
integer y = 2056
integer width = 320
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
string text = "Envase"
boolean checked = true
end type

event clicked;istr_mant.argumento[11]	= 	"1"
end event

type st_12 from statictext within w_info_cuadra_tempo_prod_embq
integer x = 1344
integer y = 1720
integer width = 206
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
boolean enabled = false
string text = "Hasta"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_14 from statictext within w_info_cuadra_tempo_prod_embq
integer x = 347
integer y = 1888
integer width = 311
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
string text = "Embarque"
boolean focusrectangle = false
end type

type cbx_embarque from checkbox within w_info_cuadra_tempo_prod_embq
integer x = 791
integer y = 1792
integer width = 315
integer height = 92
integer taborder = 150
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
boolean enabled = false
string text = "Todos"
boolean checked = true
end type

event clicked;IF This.Checked THEN
	dw_embarques.Enabled	=	False
	istr_mant.Argumento[12] = '-1'
	cbx_consembarque.Enabled = True
	
	dw_embarques.reset()
	dw_embarques.GetChild("embq_codigo", idwc_embarque)
	idwc_embarque.SetTransObject(sqlca)
	idwc_embarque.Retrieve(gi_CodExport)
	dw_embarques.InsertRow(0)
ELSE
	dw_embarques.Enabled	=	True
	dw_embarques.SetFocus()
	istr_mant.Argumento[12] = dw_embarques.Object.embq_codigo[1]
	cbx_consembarque.Enabled = False
END IF
end event

type dw_embarques from datawindow within w_info_cuadra_tempo_prod_embq
integer x = 791
integer y = 1880
integer width = 1024
integer height = 96
integer taborder = 200
boolean bringtotop = true
boolean enabled = false
string title = "none"
string dataobject = "dddw_embarques_cliente"
boolean border = false
boolean livescroll = true
end type

event itemchanged;	istr_mant.argumento[12] = data
end event

event losefocus;AcceptText()
end event

type cbx_consembarque from checkbox within w_info_cuadra_tempo_prod_embq
integer x = 1307
integer y = 1784
integer width = 558
integer height = 92
integer taborder = 160
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
boolean enabled = false
string text = "Consolidado"
boolean checked = true
end type

event clicked;IF This.Checked THEN
	cbx_embarque.Enabled = False
	Istr_mant.Argumento[12] = '-9'
ELSE
	cbx_embarque.Enabled = True
	Istr_mant.Argumento[12] = '-1'
END IF
end event

type cbx_consproductor from checkbox within w_info_cuadra_tempo_prod_embq
integer x = 1307
integer y = 1096
integer width = 558
integer height = 84
integer taborder = 170
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
string text = "Consolidado"
boolean checked = true
end type

event clicked;IF This.Checked THEN
	cbx_productor.Enabled = False
	Istr_mant.Argumento[6] = '-9'
ELSE
	cbx_productor.Enabled = True
	Istr_mant.Argumento[6] = '-1'
END IF

	
end event

type st_13 from statictext within w_info_cuadra_tempo_prod_embq
integer x = 347
integer y = 1016
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
long backcolor = 33543637
boolean enabled = false
string text = "Planta"
boolean focusrectangle = false
end type

type cbx_2 from checkbox within w_info_cuadra_tempo_prod_embq
integer x = 791
integer y = 924
integer width = 402
integer height = 76
integer taborder = 40
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
	dw_planta2.Enabled			=	False
	istr_mant.argumento[21]	=	'-1'
ELSE
	dw_planta2.Enabled			=	True
	dw_planta2.SetFocus()
END IF
end event

type dw_planta2 from datawindow within w_info_cuadra_tempo_prod_embq
integer x = 791
integer y = 1000
integer width = 969
integer height = 92
integer taborder = 50
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

IF ExistePlanta(data) THEN
	istr_mant.argumento[21]	=	data
ELSE
	This.SetItem(1, "plde_codigo", ls_null)
	RETURN 1
END IF
end event

event itemerror;RETURN 1
end event

type st_15 from statictext within w_info_cuadra_tempo_prod_embq
integer x = 1225
integer y = 1996
integer width = 873
integer height = 184
boolean bringtotop = true
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

type cbx_varrot from checkbox within w_info_cuadra_tempo_prod_embq
integer x = 1307
integer y = 2044
integer width = 663
integer height = 80
integer taborder = 180
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
string text = "Variedad Rotulada  "
end type

type cbx_consplanta from checkbox within w_info_cuadra_tempo_prod_embq
integer x = 1307
integer y = 924
integer width = 558
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
string text = "Consolidado"
boolean checked = true
end type

event clicked;IF This.Checked THEN
	cbx_2.Enabled = False
	Istr_mant.Argumento[21] = '-9'
ELSE
	cbx_2.Enabled = True
	Istr_mant.Argumento[21] = '-1'
END IF

	
end event

type uo_selespecie from uo_seleccion_especie within w_info_cuadra_tempo_prod_embq
event destroy ( )
integer x = 791
integer y = 1308
integer height = 180
integer taborder = 270
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
		uo_selvariedad.dw_Seleccion.Enabled		=	False
		
	CASE ELSE
		uo_selvariedad.Filtra(This.Codigo)

END CHOOSE
end event

type uo_selvariedad from uo_seleccion_variedad within w_info_cuadra_tempo_prod_embq
integer x = 791
integer y = 1488
integer taborder = 260
boolean bringtotop = true
end type

on uo_selvariedad.destroy
call uo_seleccion_variedad::destroy
end on

