$PBExportHeader$w_info_embaladas_vs_despachadas.srw
forward
global type w_info_embaladas_vs_despachadas from w_para_informes
end type
type st_4 from statictext within w_info_embaladas_vs_despachadas
end type
type st_1 from statictext within w_info_embaladas_vs_despachadas
end type
type st_2 from statictext within w_info_embaladas_vs_despachadas
end type
type em_desde from editmask within w_info_embaladas_vs_despachadas
end type
type dw_cliente from datawindow within w_info_embaladas_vs_despachadas
end type
type st_6 from statictext within w_info_embaladas_vs_despachadas
end type
type dw_zona from datawindow within w_info_embaladas_vs_despachadas
end type
type st_3 from statictext within w_info_embaladas_vs_despachadas
end type
type cbx_planta from checkbox within w_info_embaladas_vs_despachadas
end type
type st_7 from statictext within w_info_embaladas_vs_despachadas
end type
type em_hasta from editmask within w_info_embaladas_vs_despachadas
end type
type st_8 from statictext within w_info_embaladas_vs_despachadas
end type
type dw_productor from datawindow within w_info_embaladas_vs_despachadas
end type
type cbx_productor from checkbox within w_info_embaladas_vs_despachadas
end type
type st_9 from statictext within w_info_embaladas_vs_despachadas
end type
type cbx_packing from checkbox within w_info_embaladas_vs_despachadas
end type
type dw_packing from datawindow within w_info_embaladas_vs_despachadas
end type
type gb_3 from groupbox within w_info_embaladas_vs_despachadas
end type
type cbx_1 from checkbox within w_info_embaladas_vs_despachadas
end type
type dw_pesoneto from datawindow within w_info_embaladas_vs_despachadas
end type
type tit_peso from statictext within w_info_embaladas_vs_despachadas
end type
type st_variedad from statictext within w_info_embaladas_vs_despachadas
end type
type dw_planta from datawindow within w_info_embaladas_vs_despachadas
end type
type st_12 from statictext within w_info_embaladas_vs_despachadas
end type
type st_14 from statictext within w_info_embaladas_vs_despachadas
end type
type cbx_consproductor from checkbox within w_info_embaladas_vs_despachadas
end type
type st_13 from statictext within w_info_embaladas_vs_despachadas
end type
type cbx_2 from checkbox within w_info_embaladas_vs_despachadas
end type
type dw_planta2 from datawindow within w_info_embaladas_vs_despachadas
end type
type cbx_consplanta from checkbox within w_info_embaladas_vs_despachadas
end type
type uo_selespecie from uo_seleccion_especie within w_info_embaladas_vs_despachadas
end type
type uo_selvariedad from uo_seleccion_variedad within w_info_embaladas_vs_despachadas
end type
type gb_4 from groupbox within w_info_embaladas_vs_despachadas
end type
type st_5 from statictext within w_info_embaladas_vs_despachadas
end type
type dw_operaciones from datawindow within w_info_embaladas_vs_despachadas
end type
type cbx_operacion from checkbox within w_info_embaladas_vs_despachadas
end type
type cbx_operacioncons from checkbox within w_info_embaladas_vs_despachadas
end type
type cbx_3 from checkbox within w_info_embaladas_vs_despachadas
end type
end forward

global type w_info_embaladas_vs_despachadas from w_para_informes
integer x = 14
integer y = 32
integer width = 2519
integer height = 1860
string title = "Cuadratura Producción v/s Embarques"
boolean minbox = false
boolean maxbox = false
boolean resizable = false
windowtype windowtype = response!
string icon = "F:\Desarrollo\Producción\FrutaProcesada\Producc.ico"
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
cbx_1 cbx_1
dw_pesoneto dw_pesoneto
tit_peso tit_peso
st_variedad st_variedad
dw_planta dw_planta
st_12 st_12
st_14 st_14
cbx_consproductor cbx_consproductor
st_13 st_13
cbx_2 cbx_2
dw_planta2 dw_planta2
cbx_consplanta cbx_consplanta
uo_selespecie uo_selespecie
uo_selvariedad uo_selvariedad
gb_4 gb_4
st_5 st_5
dw_operaciones dw_operaciones
cbx_operacion cbx_operacion
cbx_operacioncons cbx_operacioncons
cbx_3 cbx_3
end type
global w_info_embaladas_vs_despachadas w_info_embaladas_vs_despachadas

type variables
str_busqueda istr_busq
str_mant istr_mant

DataWindowChild	idwc_zona,idwc_tipopro, idwc_cliente, idwc_planta, &
						idwc_productor, idwc_packing, idwc_pesoneto, idwc_embarque, idwc_operaciones

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

on w_info_embaladas_vs_despachadas.create
int iCurrent
call super::create
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
this.cbx_1=create cbx_1
this.dw_pesoneto=create dw_pesoneto
this.tit_peso=create tit_peso
this.st_variedad=create st_variedad
this.dw_planta=create dw_planta
this.st_12=create st_12
this.st_14=create st_14
this.cbx_consproductor=create cbx_consproductor
this.st_13=create st_13
this.cbx_2=create cbx_2
this.dw_planta2=create dw_planta2
this.cbx_consplanta=create cbx_consplanta
this.uo_selespecie=create uo_selespecie
this.uo_selvariedad=create uo_selvariedad
this.gb_4=create gb_4
this.st_5=create st_5
this.dw_operaciones=create dw_operaciones
this.cbx_operacion=create cbx_operacion
this.cbx_operacioncons=create cbx_operacioncons
this.cbx_3=create cbx_3
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_4
this.Control[iCurrent+2]=this.st_1
this.Control[iCurrent+3]=this.st_2
this.Control[iCurrent+4]=this.em_desde
this.Control[iCurrent+5]=this.dw_cliente
this.Control[iCurrent+6]=this.st_6
this.Control[iCurrent+7]=this.dw_zona
this.Control[iCurrent+8]=this.st_3
this.Control[iCurrent+9]=this.cbx_planta
this.Control[iCurrent+10]=this.st_7
this.Control[iCurrent+11]=this.em_hasta
this.Control[iCurrent+12]=this.st_8
this.Control[iCurrent+13]=this.dw_productor
this.Control[iCurrent+14]=this.cbx_productor
this.Control[iCurrent+15]=this.st_9
this.Control[iCurrent+16]=this.cbx_packing
this.Control[iCurrent+17]=this.dw_packing
this.Control[iCurrent+18]=this.gb_3
this.Control[iCurrent+19]=this.cbx_1
this.Control[iCurrent+20]=this.dw_pesoneto
this.Control[iCurrent+21]=this.tit_peso
this.Control[iCurrent+22]=this.st_variedad
this.Control[iCurrent+23]=this.dw_planta
this.Control[iCurrent+24]=this.st_12
this.Control[iCurrent+25]=this.st_14
this.Control[iCurrent+26]=this.cbx_consproductor
this.Control[iCurrent+27]=this.st_13
this.Control[iCurrent+28]=this.cbx_2
this.Control[iCurrent+29]=this.dw_planta2
this.Control[iCurrent+30]=this.cbx_consplanta
this.Control[iCurrent+31]=this.uo_selespecie
this.Control[iCurrent+32]=this.uo_selvariedad
this.Control[iCurrent+33]=this.gb_4
this.Control[iCurrent+34]=this.st_5
this.Control[iCurrent+35]=this.dw_operaciones
this.Control[iCurrent+36]=this.cbx_operacion
this.Control[iCurrent+37]=this.cbx_operacioncons
this.Control[iCurrent+38]=this.cbx_3
end on

on w_info_embaladas_vs_despachadas.destroy
call super::destroy
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
destroy(this.cbx_1)
destroy(this.dw_pesoneto)
destroy(this.tit_peso)
destroy(this.st_variedad)
destroy(this.dw_planta)
destroy(this.st_12)
destroy(this.st_14)
destroy(this.cbx_consproductor)
destroy(this.st_13)
destroy(this.cbx_2)
destroy(this.dw_planta2)
destroy(this.cbx_consplanta)
destroy(this.uo_selespecie)
destroy(this.uo_selvariedad)
destroy(this.gb_4)
destroy(this.st_5)
destroy(this.dw_operaciones)
destroy(this.cbx_operacion)
destroy(this.cbx_operacioncons)
destroy(this.cbx_3)
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

dw_operaciones.GetChild("oper_codigo", idwc_operaciones)
idwc_operaciones.SetTransObject(SQLCA)
idwc_operaciones.Retrieve(gi_CodExport)
dw_operaciones.InsertRow(0)

dw_operaciones.Enabled										=	False
dw_operaciones.Object.oper_codigo.BackGround.Color	=	RGB(192, 192, 192)

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
istr_mant.argumento[33]	= 	"-1" 							// Operacion
end event

type st_computador from w_para_informes`st_computador within w_info_embaladas_vs_despachadas
end type

type st_usuario from w_para_informes`st_usuario within w_info_embaladas_vs_despachadas
end type

type st_temporada from w_para_informes`st_temporada within w_info_embaladas_vs_despachadas
end type

type p_logo from w_para_informes`p_logo within w_info_embaladas_vs_despachadas
end type

type st_titulo from w_para_informes`st_titulo within w_info_embaladas_vs_despachadas
integer width = 1847
string text = "Producción v/s Embarques Central"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_embaladas_vs_despachadas
string tag = "Imprimir Reporte"
integer x = 2213
integer y = 968
integer taborder = 250
string powertiptext = "Imprimir Reporte"
end type

event pb_acepta::clicked;SetPointer(Arrow!)

Integer	fila, li_cliente, li_zona, li_especie, li_varrot, li_operacion
String	ls_cliente, ls_zona, ls_embarque

istr_info.titulo	= 'CUADRATURA PRODUCCION V/S EMBARCADO COMERCIO EXTERIOR'

OpenWithParm(vinf, istr_info)

vinf.dw_1.DataObject = "dw_info_embaladasvsdespachadas"  //
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

IF cbx_operacioncons.Checked THEN
	li_operacion	=	-9
ELSEIF cbx_operacion.Checked THEN	
	li_operacion	=	-1
ELSE	
	li_operacion	=	dw_operaciones.Object.oper_codigo[1]
END IF	

vinf.dw_1.SetTransObject(sqlca)

fila	=	vinf.dw_1.Retrieve(li_cliente, Integer(istr_mant.argumento[2]),uo_selespecie.Codigo, &
									 Long(istr_mant.argumento[6]),uo_selvariedad.Codigo,Date(istr_mant.argumento[3]),&
									 Date(istr_mant.argumento[4]),li_operacion,Integer(istr_mant.argumento[21]))

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

type pb_salir from w_para_informes`pb_salir within w_info_embaladas_vs_despachadas
string tag = "Salir [Cerrar Ventana Activa]"
integer x = 2213
integer y = 1248
integer taborder = 270
string powertiptext = "Salir [Cerrar Ventana Activa]"
end type

type st_4 from statictext within w_info_embaladas_vs_despachadas
integer x = 251
integer y = 440
integer width = 1847
integer height = 432
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

type st_1 from statictext within w_info_embaladas_vs_despachadas
integer x = 347
integer y = 588
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
long backcolor = 553648127
boolean enabled = false
string text = "Zona"
boolean focusrectangle = false
end type

type st_2 from statictext within w_info_embaladas_vs_despachadas
integer x = 347
integer y = 1340
integer width = 389
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 553648127
boolean enabled = false
string text = "Inicio"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type em_desde from editmask within w_info_embaladas_vs_despachadas
integer x = 791
integer y = 1308
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
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
end type

event modified;istr_mant.argumento[3]	=	This.Text
end event

type dw_cliente from datawindow within w_info_embaladas_vs_despachadas
integer x = 791
integer y = 472
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

type st_6 from statictext within w_info_embaladas_vs_despachadas
integer x = 347
integer y = 484
integer width = 233
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

type dw_zona from datawindow within w_info_embaladas_vs_despachadas
integer x = 791
integer y = 576
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

type st_3 from statictext within w_info_embaladas_vs_despachadas
integer x = 347
integer y = 908
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

type cbx_planta from checkbox within w_info_embaladas_vs_despachadas
boolean visible = false
integer x = 2085
integer y = 1732
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

type st_7 from statictext within w_info_embaladas_vs_despachadas
boolean visible = false
integer x = 2702
integer y = 1832
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

type em_hasta from editmask within w_info_embaladas_vs_despachadas
integer x = 1563
integer y = 1308
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
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
end type

event modified;istr_mant.argumento[4]	=	This.Text
end event

type st_8 from statictext within w_info_embaladas_vs_despachadas
integer x = 347
integer y = 768
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

type dw_productor from datawindow within w_info_embaladas_vs_despachadas
integer x = 791
integer y = 760
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

type cbx_productor from checkbox within w_info_embaladas_vs_despachadas
integer x = 791
integer y = 684
integer width = 402
integer height = 76
integer taborder = 60
boolean bringtotop = true
integer textsize = -8
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

type st_9 from statictext within w_info_embaladas_vs_despachadas
boolean visible = false
integer x = 2126
integer y = 1648
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

type cbx_packing from checkbox within w_info_embaladas_vs_despachadas
boolean visible = false
integer x = 2478
integer y = 1776
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

type dw_packing from datawindow within w_info_embaladas_vs_despachadas
boolean visible = false
integer x = 2464
integer y = 1736
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

type gb_3 from groupbox within w_info_embaladas_vs_despachadas
boolean visible = false
integer x = 2414
integer y = 1424
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

type cbx_1 from checkbox within w_info_embaladas_vs_despachadas
boolean visible = false
integer x = 2464
integer y = 1628
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

type dw_pesoneto from datawindow within w_info_embaladas_vs_despachadas
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

type tit_peso from statictext within w_info_embaladas_vs_despachadas
boolean visible = false
integer x = 2263
integer y = 1784
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

type st_variedad from statictext within w_info_embaladas_vs_despachadas
integer x = 347
integer y = 1076
integer width = 279
integer height = 84
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 553648127
string text = "Variedad"
boolean focusrectangle = false
end type

type dw_planta from datawindow within w_info_embaladas_vs_despachadas
boolean visible = false
integer x = 2464
integer y = 1544
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

type st_12 from statictext within w_info_embaladas_vs_despachadas
integer x = 1344
integer y = 1340
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

type st_14 from statictext within w_info_embaladas_vs_despachadas
integer x = 347
integer y = 1536
integer width = 315
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 553648127
string text = "Operación"
boolean focusrectangle = false
end type

type cbx_consproductor from checkbox within w_info_embaladas_vs_despachadas
integer x = 1307
integer y = 684
integer width = 558
integer height = 80
integer taborder = 170
boolean bringtotop = true
integer textsize = -8
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

type st_13 from statictext within w_info_embaladas_vs_despachadas
boolean visible = false
integer x = 576
integer y = 1900
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
long backcolor = 12632256
boolean enabled = false
string text = "Planta"
boolean focusrectangle = false
end type

type cbx_2 from checkbox within w_info_embaladas_vs_despachadas
boolean visible = false
integer x = 1019
integer y = 1808
integer width = 402
integer height = 76
integer taborder = 40
boolean bringtotop = true
integer textsize = -8
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
	dw_planta2.Enabled			=	False
	istr_mant.argumento[21]	=	'-1'
ELSE
	dw_planta2.Enabled			=	True
	dw_planta2.SetFocus()
END IF
end event

type dw_planta2 from datawindow within w_info_embaladas_vs_despachadas
boolean visible = false
integer x = 1019
integer y = 1884
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

type cbx_consplanta from checkbox within w_info_embaladas_vs_despachadas
boolean visible = false
integer x = 1536
integer y = 1808
integer width = 558
integer height = 80
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
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

type uo_selespecie from uo_seleccion_especie within w_info_embaladas_vs_despachadas
event destroy ( )
integer x = 791
integer y = 888
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

type uo_selvariedad from uo_seleccion_variedad within w_info_embaladas_vs_despachadas
integer x = 791
integer y = 1068
integer taborder = 260
boolean bringtotop = true
end type

on uo_selvariedad.destroy
call uo_seleccion_variedad::destroy
end on

type gb_4 from groupbox within w_info_embaladas_vs_despachadas
integer x = 288
integer y = 1244
integer width = 1774
integer height = 204
integer taborder = 230
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 553648127
string text = "Fecha Embalaje"
end type

type st_5 from statictext within w_info_embaladas_vs_despachadas
integer x = 251
integer y = 872
integer width = 1847
integer height = 788
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

type dw_operaciones from datawindow within w_info_embaladas_vs_despachadas
integer x = 791
integer y = 1524
integer width = 974
integer height = 92
integer taborder = 180
boolean bringtotop = true
boolean enabled = false
string title = "none"
string dataobject = "dddw_operaciones"
boolean border = false
boolean livescroll = true
end type

event itemchanged;istr_mant.argumento[33]	=	data

end event

event itemerror;RETURN 1
end event

event losefocus;AcceptText()
end event

type cbx_operacion from checkbox within w_info_embaladas_vs_despachadas
integer x = 791
integer y = 1472
integer width = 379
integer height = 56
integer taborder = 160
boolean bringtotop = true
integer textsize = -8
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
	cbx_operacioncons.Enabled									=	True
	dw_operaciones.Enabled											=	False
	dw_operaciones.Object.oper_codigo.BackGround.Color	=	RGB(192, 192, 192)
	istr_mant.argumento[33]										=	'0'
ELSE
	cbx_operacioncons.Enabled									=	False
	cbx_operacioncons.Checked									=	False
	dw_operaciones.Enabled											=	True
	dw_operaciones.Object.oper_codigo.BackGround.Color	=	RGB(255, 255, 255)
	dw_operaciones.SetFocus()
END IF

	
end event

type cbx_operacioncons from checkbox within w_info_embaladas_vs_despachadas
integer x = 1271
integer y = 1472
integer width = 471
integer height = 56
integer taborder = 150
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
string text = "Consolidados"
boolean checked = true
end type

event clicked;IF This.Checked = TRUE THEN
	istr_mant.argumento[23]	=	'1'
	cbx_operacion.Enabled	=	False
	cbx_operacion.Checked	=	True

ELSE
	istr_mant.argumento[23]	=	'0'
	istr_mant.argumento[33]	=	'0'
	cbx_operacion.Enabled	=	True

END IF
	
end event

type cbx_3 from checkbox within w_info_embaladas_vs_despachadas
integer x = 1650
integer y = 580
integer width = 293
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
string text = "Todas"
boolean righttoleft = true
end type

event clicked;istr_mant.argumento[2] = '-1'
end event

