$PBExportHeader$w_info_produccion_historicapack.srw
forward
global type w_info_produccion_historicapack from w_para_informes
end type
type st_4 from statictext within w_info_produccion_historicapack
end type
type st_1 from statictext within w_info_produccion_historicapack
end type
type st_2 from statictext within w_info_produccion_historicapack
end type
type em_desde from editmask within w_info_produccion_historicapack
end type
type dw_cliente from datawindow within w_info_produccion_historicapack
end type
type st_6 from statictext within w_info_produccion_historicapack
end type
type dw_planta from datawindow within w_info_produccion_historicapack
end type
type st_3 from statictext within w_info_produccion_historicapack
end type
type cbx_planta from checkbox within w_info_produccion_historicapack
end type
type st_7 from statictext within w_info_produccion_historicapack
end type
type em_hasta from editmask within w_info_produccion_historicapack
end type
type st_8 from statictext within w_info_produccion_historicapack
end type
type dw_productor from datawindow within w_info_produccion_historicapack
end type
type cbx_productor from checkbox within w_info_produccion_historicapack
end type
type st_9 from statictext within w_info_produccion_historicapack
end type
type cbx_packing from checkbox within w_info_produccion_historicapack
end type
type dw_packing from datawindow within w_info_produccion_historicapack
end type
type gb_3 from groupbox within w_info_produccion_historicapack
end type
type st_5 from statictext within w_info_produccion_historicapack
end type
type cbx_peso from checkbox within w_info_produccion_historicapack
end type
type dw_pesoneto from datawindow within w_info_produccion_historicapack
end type
type tit_peso from statictext within w_info_produccion_historicapack
end type
type st_variedad from statictext within w_info_produccion_historicapack
end type
type rb_embalaje from radiobutton within w_info_produccion_historicapack
end type
type rb_productor from radiobutton within w_info_produccion_historicapack
end type
type rb_variedad from radiobutton within w_info_produccion_historicapack
end type
type gb_5 from groupbox within w_info_produccion_historicapack
end type
type gb_15 from groupbox within w_info_produccion_historicapack
end type
type st_10 from statictext within w_info_produccion_historicapack
end type
type cbx_variedadrelacionada from checkbox within w_info_produccion_historicapack
end type
type cbx_packingcons from checkbox within w_info_produccion_historicapack
end type
type uo_selespecie from uo_seleccion_especie within w_info_produccion_historicapack
end type
type uo_selvariedad from uo_seleccion_variedad within w_info_produccion_historicapack
end type
end forward

global type w_info_produccion_historicapack from w_para_informes
integer x = 14
integer y = 32
integer width = 2510
integer height = 2480
string title = "INFORME PRODUCCION HISTORICA POR PACKING"
boolean minbox = false
boolean maxbox = false
boolean resizable = false
windowtype windowtype = response!
string icon = "F:\Desarrollo\Producción\FrutaProcesada\Producc.ico"
boolean center = true
st_4 st_4
st_1 st_1
st_2 st_2
em_desde em_desde
dw_cliente dw_cliente
st_6 st_6
dw_planta dw_planta
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
cbx_peso cbx_peso
dw_pesoneto dw_pesoneto
tit_peso tit_peso
st_variedad st_variedad
rb_embalaje rb_embalaje
rb_productor rb_productor
rb_variedad rb_variedad
gb_5 gb_5
gb_15 gb_15
st_10 st_10
cbx_variedadrelacionada cbx_variedadrelacionada
cbx_packingcons cbx_packingcons
uo_selespecie uo_selespecie
uo_selvariedad uo_selvariedad
end type
global w_info_produccion_historicapack w_info_produccion_historicapack

type variables
str_busqueda istr_busq
str_mant istr_mant

DataWindowChild	idwc_tipopro, idwc_cliente, idwc_planta, idwc_especie, idwc_productor, idwc_packing, idwc_pesoneto

String is_NomPlanta, is_packing

uo_seleccion_especie		iuo_selespecie
uo_seleccion_variedad	iuo_selvariedad
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

public function boolean existepacking (integer li_planta);Integer  li_codigo

SELECT plde_codigo, plde_nombre
INTO   :li_Codigo,:is_packing
FROM	dba.plantadesp
WHERE	plde_codigo =  :li_planta;
	
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
	MessageBox("Atención", "Código de Productor no ha sido Definido o no pertenece a este cliente.~r~r" + &
					"Ingrese o seleccione otro Código.")
	RETURN False
ELSE
	istr_mant.argumento[6] = String(ll_Productor)	
	RETURN True
END IF
end function

on w_info_produccion_historicapack.create
int iCurrent
call super::create
this.st_4=create st_4
this.st_1=create st_1
this.st_2=create st_2
this.em_desde=create em_desde
this.dw_cliente=create dw_cliente
this.st_6=create st_6
this.dw_planta=create dw_planta
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
this.cbx_peso=create cbx_peso
this.dw_pesoneto=create dw_pesoneto
this.tit_peso=create tit_peso
this.st_variedad=create st_variedad
this.rb_embalaje=create rb_embalaje
this.rb_productor=create rb_productor
this.rb_variedad=create rb_variedad
this.gb_5=create gb_5
this.gb_15=create gb_15
this.st_10=create st_10
this.cbx_variedadrelacionada=create cbx_variedadrelacionada
this.cbx_packingcons=create cbx_packingcons
this.uo_selespecie=create uo_selespecie
this.uo_selvariedad=create uo_selvariedad
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_4
this.Control[iCurrent+2]=this.st_1
this.Control[iCurrent+3]=this.st_2
this.Control[iCurrent+4]=this.em_desde
this.Control[iCurrent+5]=this.dw_cliente
this.Control[iCurrent+6]=this.st_6
this.Control[iCurrent+7]=this.dw_planta
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
this.Control[iCurrent+19]=this.st_5
this.Control[iCurrent+20]=this.cbx_peso
this.Control[iCurrent+21]=this.dw_pesoneto
this.Control[iCurrent+22]=this.tit_peso
this.Control[iCurrent+23]=this.st_variedad
this.Control[iCurrent+24]=this.rb_embalaje
this.Control[iCurrent+25]=this.rb_productor
this.Control[iCurrent+26]=this.rb_variedad
this.Control[iCurrent+27]=this.gb_5
this.Control[iCurrent+28]=this.gb_15
this.Control[iCurrent+29]=this.st_10
this.Control[iCurrent+30]=this.cbx_variedadrelacionada
this.Control[iCurrent+31]=this.cbx_packingcons
this.Control[iCurrent+32]=this.uo_selespecie
this.Control[iCurrent+33]=this.uo_selvariedad
end on

on w_info_produccion_historicapack.destroy
call super::destroy
destroy(this.st_4)
destroy(this.st_1)
destroy(this.st_2)
destroy(this.em_desde)
destroy(this.dw_cliente)
destroy(this.st_6)
destroy(this.dw_planta)
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
destroy(this.cbx_peso)
destroy(this.dw_pesoneto)
destroy(this.tit_peso)
destroy(this.st_variedad)
destroy(this.rb_embalaje)
destroy(this.rb_productor)
destroy(this.rb_variedad)
destroy(this.gb_5)
destroy(this.gb_15)
destroy(this.st_10)
destroy(this.cbx_variedadrelacionada)
destroy(this.cbx_packingcons)
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

dw_planta.GetChild("plde_codigo", idwc_planta)
idwc_planta.SetTransObject(sqlca)
idwc_planta.Retrieve(1)
dw_planta.InsertRow(0)

dw_productor.GetChild("prod_codigo", idwc_productor)
idwc_productor.SetTransObject(sqlca)
idwc_productor.Retrieve(gi_CodExport)
dw_productor.InsertRow(0)

dw_packing.GetChild("plde_codigo", idwc_packing)
idwc_packing.SetTransObject(sqlca)
idwc_packing.Retrieve(2)
dw_packing.InsertRow(0)

// uo_seleccion_especie
IF IsNull(uo_selespecie.Codigo) THEN lb_Cerrar = True
IF lb_Cerrar THEN
	Close(This)
ELSE
	uo_selespecie.Seleccion(True,True)
END IF

// uo_seleccion_variedad
IF IsNull(uo_selvariedad.Codigo) THEN lb_Cerrar = True
IF lb_Cerrar THEN
	Close(This)
ELSE
	uo_selvariedad.Seleccion(True,True)
END IF

dw_pesoneto.GetChild("enva_pesone", idwc_pesoneto)
idwc_pesoneto.SetTransObject(SQLCA)
idwc_pesoneto.Retrieve()
dw_pesoneto.InsertRow(0)
dw_pesoneto.SetItem(1, "enva_pesone", 8.20)
tit_peso.Enabled		=	False
dw_pesoneto.Enabled	=	False
dw_pesoneto.Modify("enva_pesone.BackGround.Color = " + String(RGB(166,180,210)))

em_desde.Text				=	String(RelativeDate(Today(), -365))
em_hasta.Text				=	String(Today())
istr_mant.argumento[1]	= 	String(gi_CodExport) 	// cliente
istr_mant.argumento[2]	= 	"0"							//	planta
istr_mant.argumento[3]	= 	em_desde.Text				//	fecha inicio
istr_mant.argumento[4]	=	em_hasta.Text				//	fecha final
istr_mant.argumento[6]  =  "0"							//	productor
istr_mant.argumento[7]  =  "0"							//	packing
istr_mant.argumento[8]  =  "1"							//	peso
end event

type st_computador from w_para_informes`st_computador within w_info_produccion_historicapack
end type

type st_usuario from w_para_informes`st_usuario within w_info_produccion_historicapack
end type

type st_temporada from w_para_informes`st_temporada within w_info_produccion_historicapack
end type

type p_logo from w_para_informes`p_logo within w_info_produccion_historicapack
end type

type st_titulo from w_para_informes`st_titulo within w_info_produccion_historicapack
integer width = 1847
string text = "Informe Histórico de Producción por Packing"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_produccion_historicapack
string tag = "Imprimir Reporte"
integer x = 2194
integer y = 1692
integer taborder = 150
string powertiptext = "Imprimir Reporte"
end type

event pb_acepta::clicked;SetPointer(Arrow!)

Integer	fila, li_cliente, li_planta
Date		ld_desde, ld_hasta
String	texto_desde, texto_hasta, texto_fecha, ls_nroguia, ls_cajas, ls_packing

istr_info.titulo	= 'INFORME DE PRODUCCION HISTORICA POR PACKING'
OpenWithParm(vinf, istr_info)

IF cbx_variedadrelacionada.Checked THEN
	IF rb_embalaje.checked THEN
	   vinf.dw_1.DataObject = "dw_info_prod_historica_varela"
	ELSEIF rb_productor.checked THEN
		vinf.dw_1.DataObject = "dw_info_prod_hist_varela_prod"
	ELSE
		vinf.dw_1.DataObject = "dw_info_prod_hist_varela_var"
	END IF
ELSE
	IF rb_embalaje.checked THEN
		vinf.dw_1.DataObject = "dw_info_produc_historica"
	ELSEIF rb_productor.checked THEN
		vinf.dw_1.DataObject = "dw_info_produc_historica_prd"
	ELSE
		vinf.dw_1.DataObject = "dw_info_produc_historica_var"
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
li_planta		=	Integer(istr_mant.argumento[2])
ld_desde			=	Date(istr_mant.argumento[3])
ld_hasta			=	Date(istr_mant.argumento[4])
texto_desde		=  f_fecha_texto(String(ld_desde), 1)
texto_hasta		=	f_fecha_texto(String(ld_hasta), 1)
texto_fecha		=	"Desde El :  " + texto_desde + "   Hasta El :  " + texto_hasta
ls_nroguia		=	"Todas"

IF cbx_packingcons.Checked	THEN
	ls_packing	= 'Consolidados'
ELSEIF cbx_packing.Checked THEN
	ls_packing  = 'Todos'
ELSE
	ls_packing	=	String(Integer(istr_mant.argumento[7]),'####') + ' ' + is_packing
END IF

IF cbx_peso.Checked=False THEN
	ls_cajas = "Reales"
	istr_mant.argumento[8]	=	"1"
ELSE
	istr_mant.argumento[8]	=	String(dw_pesoneto.Object.enva_pesone[1])
	ls_cajas						=	"Base " + istr_mant.argumento[8] 
END IF

vinf.dw_1.SetTransObject(sqlca)

fila	=	vinf.dw_1.Retrieve(li_cliente, li_planta, ld_desde, ld_hasta, uo_selespecie.Codigo,&
									 0, Long(istr_mant.argumento[6]),Integer(istr_mant.argumento[7]),&
									 Dec(istr_mant.argumento[8]),uo_selvariedad.Codigo)

IF fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
			   	"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)

ELSEIF fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", &
	             StopSign!, Ok!)

ELSE
	   F_Membrete(vinf.dw_1)
      IF cbx_variedadrelacionada.Checked=False THEN vinf.dw_1.Object.titulo_informe.text = 'Producción Historica por Packing'
		vinf.dw_1.Modify("guia.text = '" + ls_nroguia + "'")
		vinf.dw_1.Modify("fechas.text = '" + texto_fecha + "'")
		vinf.dw_1.Modify("Cajas.text = '" + ls_cajas + "'")
		vinf.dw_1.Modify("packing.text = '" + ls_packing + "'")
	IF gs_Ambiente <> 'Windows' THEN
		F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
	END IF
END IF

SetPointer(Arrow!)				
end event

type pb_salir from w_para_informes`pb_salir within w_info_produccion_historicapack
string tag = "Salir [Cerrar Ventana Activa]"
integer x = 2194
integer y = 1980
integer taborder = 170
string powertiptext = "Salir [Cerrar Ventana Activa]"
end type

type st_4 from statictext within w_info_produccion_historicapack
integer x = 238
integer y = 448
integer width = 1847
integer height = 692
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

type st_1 from statictext within w_info_produccion_historicapack
integer x = 334
integer y = 652
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
string text = "Planta"
boolean focusrectangle = false
end type

type st_2 from statictext within w_info_produccion_historicapack
integer x = 334
integer y = 1548
integer width = 425
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 553648127
string text = "Fecha Inicio"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type em_desde from editmask within w_info_produccion_historicapack
integer x = 777
integer y = 1532
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

event modified;istr_mant.argumento[3]	=	This.Text
end event

type dw_cliente from datawindow within w_info_produccion_historicapack
integer x = 777
integer y = 480
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

type st_6 from statictext within w_info_produccion_historicapack
integer x = 334
integer y = 488
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

type dw_planta from datawindow within w_info_produccion_historicapack
integer x = 777
integer y = 656
integer width = 969
integer height = 92
integer taborder = 40
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
ELSE
	This.SetItem(1, "plde_codigo", ls_null)
	RETURN 1
END IF
end event

event itemerror;RETURN 1
end event

type st_3 from statictext within w_info_produccion_historicapack
integer x = 334
integer y = 1240
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

type cbx_planta from checkbox within w_info_produccion_historicapack
integer x = 777
integer y = 580
integer width = 402
integer height = 76
integer taborder = 30
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
	dw_planta.Enabled			=	False
	istr_mant.argumento[2]	=	'0'
ELSE
	dw_planta.Enabled			=	True
	dw_planta.SetFocus()
END IF
end event

type st_7 from statictext within w_info_produccion_historicapack
integer x = 1211
integer y = 1548
integer width = 297
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 553648127
string text = "Término"
boolean focusrectangle = false
end type

type em_hasta from editmask within w_info_produccion_historicapack
integer x = 1545
integer y = 1532
integer width = 393
integer height = 96
integer taborder = 140
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

type st_8 from statictext within w_info_produccion_historicapack
integer x = 334
integer y = 844
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

type dw_productor from datawindow within w_info_produccion_historicapack
integer x = 777
integer y = 836
integer width = 978
integer height = 92
integer taborder = 60
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

type cbx_productor from checkbox within w_info_produccion_historicapack
integer x = 777
integer y = 756
integer width = 402
integer height = 80
integer taborder = 50
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

event clicked;IF cbx_productor.Checked = TRUE THEN
	dw_productor.Enabled  = False
	dw_packing.Enabled    = False
	cbx_packing.Checked   = False
	dw_productor.Reset()
	dw_productor.insertrow(0)
	istr_mant.argumento[6]	=	'0'
ELSE
	dw_productor.Enabled  = True
	dw_productor.SetFocus()
	dw_productor.Reset()
	dw_productor.InsertRow(0)
	dw_packing.Enabled    = True
	cbx_packing.Enabled   = True
	dw_packing.Reset()
	dw_packing.InsertRow(0)	
END IF
	
end event

type st_9 from statictext within w_info_produccion_historicapack
integer x = 334
integer y = 1012
integer width = 238
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
string text = "Packing"
alignment alignment = right!
boolean focusrectangle = false
end type

type cbx_packing from checkbox within w_info_produccion_historicapack
integer x = 777
integer y = 932
integer width = 402
integer height = 80
integer taborder = 70
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
	cbx_packingcons.Enabled									=	True
	dw_packing.Enabled										=	False
	istr_mant.argumento[7]	                        =	'0'
ELSE
	cbx_packingcons.Enabled									=	False
	cbx_packingcons.Checked									=	False
	dw_packing.Enabled										=	True
END IF
end event

type dw_packing from datawindow within w_info_produccion_historicapack
integer x = 777
integer y = 1012
integer width = 965
integer height = 100
integer taborder = 80
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

	IF ExistePacking(Integer(data))THEN
		istr_mant.argumento[7]	=	data		
		RETURN 0
	ELSE
		This.SetItem(1, "plde_codigo", ls_null)
		RETURN 1
	END IF
end event

type gb_3 from groupbox within w_info_produccion_historicapack
integer x = 283
integer y = 1620
integer width = 1742
integer height = 192
integer taborder = 160
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 553648127
end type

type st_5 from statictext within w_info_produccion_historicapack
integer x = 238
integer y = 1140
integer width = 1847
integer height = 700
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

type cbx_peso from checkbox within w_info_produccion_historicapack
integer x = 329
integer y = 1684
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

type dw_pesoneto from datawindow within w_info_produccion_historicapack
integer x = 1294
integer y = 1680
integer width = 686
integer height = 100
integer taborder = 20
boolean bringtotop = true
string dataobject = "dddw_pesoneto"
boolean border = false
boolean livescroll = true
end type

event itemerror;RETURN 1
end event

type tit_peso from statictext within w_info_produccion_historicapack
integer x = 1102
integer y = 1692
integer width = 183
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

type st_variedad from statictext within w_info_produccion_historicapack
integer x = 334
integer y = 1432
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

type rb_embalaje from radiobutton within w_info_produccion_historicapack
integer x = 370
integer y = 1916
integer width = 503
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
string text = "Por Embalaje"
boolean checked = true
end type

type rb_productor from radiobutton within w_info_produccion_historicapack
integer x = 910
integer y = 1916
integer width = 512
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
string text = "Por Productor"
end type

type rb_variedad from radiobutton within w_info_produccion_historicapack
integer x = 1449
integer y = 1916
integer width = 503
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
string text = "Por Variedad"
end type

type gb_5 from groupbox within w_info_produccion_historicapack
integer x = 283
integer y = 1848
integer width = 1742
integer height = 168
integer taborder = 160
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 553648127
string text = "Orden"
end type

type gb_15 from groupbox within w_info_produccion_historicapack
integer x = 283
integer y = 2008
integer width = 1742
integer height = 168
integer taborder = 170
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 553648127
end type

type st_10 from statictext within w_info_produccion_historicapack
integer x = 238
integer y = 1840
integer width = 1847
integer height = 372
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

type cbx_variedadrelacionada from checkbox within w_info_produccion_historicapack
integer x = 786
integer y = 2068
integer width = 805
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
string text = "Variedad Relacionada"
end type

event clicked;IF This.Checked THEN
	uo_Selvariedad.cbx_todos.checked	= True
	uo_Selvariedad.Enabled				=	False
	gb_5.Enabled							=	False	
ELSE
	uo_Selvariedad.Enabled				=	True	
	gb_5.Enabled							=	True	
END IF
end event

type cbx_packingcons from checkbox within w_info_produccion_historicapack
integer x = 1294
integer y = 932
integer width = 443
integer height = 80
integer taborder = 70
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

event clicked;IF This.Checked = TRUE THEN
	istr_mant.argumento[7]	=	'-9'
ELSE
	istr_mant.argumento[7]	=	'0'
END IF

end event

type uo_selespecie from uo_seleccion_especie within w_info_produccion_historicapack
event destroy ( )
integer x = 777
integer y = 1152
integer height = 180
integer taborder = 70
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

type uo_selvariedad from uo_seleccion_variedad within w_info_produccion_historicapack
event destroy ( )
integer x = 777
integer y = 1336
integer taborder = 80
boolean bringtotop = true
end type

on uo_selvariedad.destroy
call uo_seleccion_variedad::destroy
end on

