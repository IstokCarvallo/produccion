$PBExportHeader$w_info_recepciones_guias.srw
forward
global type w_info_recepciones_guias from w_para_informes
end type
type st_4 from statictext within w_info_recepciones_guias
end type
type st_1 from statictext within w_info_recepciones_guias
end type
type st_2 from statictext within w_info_recepciones_guias
end type
type em_desde from editmask within w_info_recepciones_guias
end type
type dw_cliente from datawindow within w_info_recepciones_guias
end type
type st_6 from statictext within w_info_recepciones_guias
end type
type dw_planta from datawindow within w_info_recepciones_guias
end type
type st_3 from statictext within w_info_recepciones_guias
end type
type cbx_planta from checkbox within w_info_recepciones_guias
end type
type st_7 from statictext within w_info_recepciones_guias
end type
type em_hasta from editmask within w_info_recepciones_guias
end type
type st_8 from statictext within w_info_recepciones_guias
end type
type dw_productor from datawindow within w_info_recepciones_guias
end type
type cbx_productor from checkbox within w_info_recepciones_guias
end type
type st_9 from statictext within w_info_recepciones_guias
end type
type cbx_packing from checkbox within w_info_recepciones_guias
end type
type dw_packing from datawindow within w_info_recepciones_guias
end type
type gb_3 from groupbox within w_info_recepciones_guias
end type
type st_5 from statictext within w_info_recepciones_guias
end type
type cbx_1 from checkbox within w_info_recepciones_guias
end type
type dw_pesoneto from datawindow within w_info_recepciones_guias
end type
type tit_peso from statictext within w_info_recepciones_guias
end type
type cbx_productorcons from checkbox within w_info_recepciones_guias
end type
type st_16 from statictext within w_info_recepciones_guias
end type
type rb_recepciones from radiobutton within w_info_recepciones_guias
end type
type rb_despachos from radiobutton within w_info_recepciones_guias
end type
type st_variedad from statictext within w_info_recepciones_guias
end type
type st_embalaje from statictext within w_info_recepciones_guias
end type
type em_embalaje from editmask within w_info_recepciones_guias
end type
type cb_buscaembalaje from commandbutton within w_info_recepciones_guias
end type
type cbx_embalaje from checkbox within w_info_recepciones_guias
end type
type cbx_consembalaje from checkbox within w_info_recepciones_guias
end type
type uo_selespecie from uo_seleccion_especie within w_info_recepciones_guias
end type
type uo_selvariedad from uo_seleccion_variedad within w_info_recepciones_guias
end type
type cbx_varirotula from checkbox within w_info_recepciones_guias
end type
type st_15 from statictext within w_info_recepciones_guias
end type
end forward

global type w_info_recepciones_guias from w_para_informes
integer x = 0
integer y = 0
integer width = 2601
integer height = 2456
string title = "RECEPCIONES POR GUIAS"
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
cbx_1 cbx_1
dw_pesoneto dw_pesoneto
tit_peso tit_peso
cbx_productorcons cbx_productorcons
st_16 st_16
rb_recepciones rb_recepciones
rb_despachos rb_despachos
st_variedad st_variedad
st_embalaje st_embalaje
em_embalaje em_embalaje
cb_buscaembalaje cb_buscaembalaje
cbx_embalaje cbx_embalaje
cbx_consembalaje cbx_consembalaje
uo_selespecie uo_selespecie
uo_selvariedad uo_selvariedad
cbx_varirotula cbx_varirotula
st_15 st_15
end type
global w_info_recepciones_guias w_info_recepciones_guias

type variables
str_busqueda istr_busq
str_mant istr_mant

DataWindowChild	idwc_tipopro, idwc_cliente, idwc_planta, &
						idwc_productor, idwc_packing, idwc_pesoneto

String is_NomPlanta

uo_seleccion_especie		iuo_selespecie
uo_seleccion_variedad	iuo_selvariedad
end variables

forward prototypes
public function boolean existepacking (integer li_cliente, ref string ls_columna)
public function boolean existeproductor (integer li_cliente, long ll_productor)
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

public function boolean existeproductor (integer li_cliente, long ll_productor);String	ls_Nombre

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

on w_info_recepciones_guias.create
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
this.cbx_1=create cbx_1
this.dw_pesoneto=create dw_pesoneto
this.tit_peso=create tit_peso
this.cbx_productorcons=create cbx_productorcons
this.st_16=create st_16
this.rb_recepciones=create rb_recepciones
this.rb_despachos=create rb_despachos
this.st_variedad=create st_variedad
this.st_embalaje=create st_embalaje
this.em_embalaje=create em_embalaje
this.cb_buscaembalaje=create cb_buscaembalaje
this.cbx_embalaje=create cbx_embalaje
this.cbx_consembalaje=create cbx_consembalaje
this.uo_selespecie=create uo_selespecie
this.uo_selvariedad=create uo_selvariedad
this.cbx_varirotula=create cbx_varirotula
this.st_15=create st_15
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
this.Control[iCurrent+20]=this.cbx_1
this.Control[iCurrent+21]=this.dw_pesoneto
this.Control[iCurrent+22]=this.tit_peso
this.Control[iCurrent+23]=this.cbx_productorcons
this.Control[iCurrent+24]=this.st_16
this.Control[iCurrent+25]=this.rb_recepciones
this.Control[iCurrent+26]=this.rb_despachos
this.Control[iCurrent+27]=this.st_variedad
this.Control[iCurrent+28]=this.st_embalaje
this.Control[iCurrent+29]=this.em_embalaje
this.Control[iCurrent+30]=this.cb_buscaembalaje
this.Control[iCurrent+31]=this.cbx_embalaje
this.Control[iCurrent+32]=this.cbx_consembalaje
this.Control[iCurrent+33]=this.uo_selespecie
this.Control[iCurrent+34]=this.uo_selvariedad
this.Control[iCurrent+35]=this.cbx_varirotula
this.Control[iCurrent+36]=this.st_15
end on

on w_info_recepciones_guias.destroy
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
destroy(this.cbx_1)
destroy(this.dw_pesoneto)
destroy(this.tit_peso)
destroy(this.cbx_productorcons)
destroy(this.st_16)
destroy(this.rb_recepciones)
destroy(this.rb_despachos)
destroy(this.st_variedad)
destroy(this.st_embalaje)
destroy(this.em_embalaje)
destroy(this.cb_buscaembalaje)
destroy(this.cbx_embalaje)
destroy(this.cbx_consembalaje)
destroy(this.uo_selespecie)
destroy(this.uo_selvariedad)
destroy(this.cbx_varirotula)
destroy(this.st_15)
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

dw_pesoneto.GetChild("enva_pesone", idwc_pesoneto)
idwc_pesoneto.SetTransObject(SQLCA)
idwc_pesoneto.Retrieve(0)
dw_pesoneto.InsertRow(0)
dw_pesoneto.SetItem(1, "enva_pesone", 820/100)
tit_peso.Enabled		=	False
dw_pesoneto.Enabled	=	False
dw_pesoneto.Modify("enva_pesone.BackGround.Color = " + String(RGB(166,180,210)))

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

em_desde.Text				=	String(RelativeDate(Today(), -365))
em_hasta.Text				=	String(Today())
istr_mant.argumento[1]	= 	String(gi_CodExport) 	// cliente
istr_mant.argumento[2]	= 	"-1"							//	planta
istr_mant.argumento[3]	= 	em_desde.Text				//	fecha inicio
istr_mant.argumento[4]	=	em_hasta.Text				//	fecha final
//istr_mant.argumento[5]	=	"-9"							//	especie
istr_mant.argumento[6]  =  "-9"							//	productor
istr_mant.argumento[7]  =  "-1"							//	packing
istr_mant.argumento[8]  =  "1"							//	peso
//istr_mant.argumento[9]	= 	"-9" 							// variedad
istr_mant.argumento[10]	= 	"-9" 							// Embalaje
end event

event resize;//
end event

type st_computador from w_para_informes`st_computador within w_info_recepciones_guias
end type

type st_usuario from w_para_informes`st_usuario within w_info_recepciones_guias
end type

type st_temporada from w_para_informes`st_temporada within w_info_recepciones_guias
end type

type p_logo from w_para_informes`p_logo within w_info_recepciones_guias
end type

type st_titulo from w_para_informes`st_titulo within w_info_recepciones_guias
integer width = 1847
string text = "Informe por Guías"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_recepciones_guias
integer x = 2199
integer y = 1652
integer taborder = 150
end type

event pb_acepta::clicked;SetPointer(Arrow!)

Integer	fila, li_cliente, li_planta, li_packing,li_varirotula
Date		ld_desde, ld_hasta
String	texto_desde, texto_hasta, texto_fecha, ls_cajas,Is_Embalaje
Long		ll_productor

istr_info.titulo	= 'INFORME MOVIMIENTOS POR GUIAS'

OpenWithParm(vinf, istr_info)

IF rb_Recepciones. Checked THEN
   vinf.dw_1.DataObject = "dw_info_recepciones_guias"
ELSE
	vinf.dw_1.DataObject = "dw_info_despachos_guias"
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

IF cbx_varirotula.Checked THEN
	li_varirotula = 1
ELSE
	li_varirotula = 0
END IF

li_cliente 		=	Integer(istr_mant.argumento[1])
li_planta		=	Integer(istr_mant.argumento[2])
ld_desde			=	Date(istr_mant.argumento[3])
ld_hasta			=	Date(istr_mant.argumento[4])
ll_productor	=	Long(istr_mant.argumento[6])
li_packing		=  Integer(istr_mant.argumento[7])
Is_Embalaje    =  istr_mant.argumento[10] 

texto_desde		=  f_fecha_texto(String(ld_desde), 1)
texto_hasta		=	f_fecha_texto(String(ld_hasta), 1)
texto_fecha		=	"Desde El :  " + texto_desde + "   Hasta El :  " + texto_hasta


vinf.dw_1.SetTransObject(sqlca)

fila	=	vinf.dw_1.Retrieve(li_cliente, li_planta, ld_desde, ld_hasta, uo_selespecie.Codigo, &
                            ll_productor, li_packing, uo_selvariedad.Codigo, Is_Embalaje, li_varirotula)
									 
IF fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
			   	"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)

ELSEIF fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", &
	             StopSign!, Ok!)

ELSE
	   F_Membrete(vinf.dw_1)
		vinf.dw_1.Modify("Fechas.text = '" + texto_fecha + "'")
	IF gs_Ambiente <> 'Windows' THEN
		F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
	END IF
END IF
SetPointer(Arrow!)				
end event

type pb_salir from w_para_informes`pb_salir within w_info_recepciones_guias
integer x = 2199
integer y = 1932
integer taborder = 170
end type

type st_4 from statictext within w_info_recepciones_guias
integer x = 251
integer y = 600
integer width = 1847
integer height = 760
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

type st_1 from statictext within w_info_recepciones_guias
integer x = 347
integer y = 804
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

type st_2 from statictext within w_info_recepciones_guias
integer x = 672
integer y = 2008
integer width = 215
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
string text = "Desde"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type em_desde from editmask within w_info_recepciones_guias
integer x = 549
integer y = 2092
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

type dw_cliente from datawindow within w_info_recepciones_guias
integer x = 791
integer y = 628
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
	dw_productor.GetChild("prod_codigo", idwc_productor)
	idwc_productor.SetTransObject(sqlca)
	idwc_productor.Retrieve(Integer(data))
	dw_productor.InsertRow(0)
	idwc_packing.Retrieve(2)
ELSE
	This.SetItem(1, "clie_codigo", ls_null)
	RETURN 1
END IF
end event

event itemerror;RETURN 1
end event

type st_6 from statictext within w_info_recepciones_guias
integer x = 347
integer y = 636
integer width = 306
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

type dw_planta from datawindow within w_info_recepciones_guias
integer x = 791
integer y = 808
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

type st_3 from statictext within w_info_recepciones_guias
integer x = 347
integer y = 1492
integer width = 306
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

type cbx_planta from checkbox within w_info_recepciones_guias
integer x = 791
integer y = 732
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
	istr_mant.argumento[2]	=	'-1'
ELSE
	dw_planta.Enabled			=	True
	dw_planta.SetFocus()
END IF
end event

type st_7 from statictext within w_info_recepciones_guias
integer x = 1454
integer y = 2008
integer width = 187
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
string text = "Hasta"
boolean focusrectangle = false
end type

type em_hasta from editmask within w_info_recepciones_guias
integer x = 1330
integer y = 2088
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

type st_8 from statictext within w_info_recepciones_guias
integer x = 347
integer y = 1020
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

type dw_productor from datawindow within w_info_recepciones_guias
integer x = 791
integer y = 1016
integer width = 974
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

event itemchanged;istr_mant.argumento[6]	=	data	

end event

type cbx_productor from checkbox within w_info_recepciones_guias
integer x = 791
integer y = 932
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
boolean enabled = false
string text = "Todos"
boolean checked = true
end type

event clicked;IF This.Checked = TRUE THEN
	dw_productor.Enabled  = False
	dw_productor.Reset()
	dw_productor.insertrow(0)
	istr_mant.argumento[6]	=	'-1'
ELSE
	dw_productor.Enabled  = True
	dw_productor.SetFocus()
	dw_productor.Reset()
	dw_productor.InsertRow(0)
END IF
	
end event

type st_9 from statictext within w_info_recepciones_guias
integer x = 347
integer y = 1208
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
string text = "Packing"
boolean focusrectangle = false
end type

type cbx_packing from checkbox within w_info_recepciones_guias
integer x = 791
integer y = 1128
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

event clicked;IF This.checked = True THEN
	dw_packing.Enabled = False
	istr_mant.argumento[7]	=	'-1'
ELSE
	dw_packing.Enabled=True	
END IF	
end event

type dw_packing from datawindow within w_info_recepciones_guias
integer x = 791
integer y = 1208
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

	IF ExistePacking(Integer(istr_mant.argumento[1]),data)THEN
		istr_mant.argumento[7]	=	data		
		RETURN 0
	ELSE
		This.SetItem(1, "plde_codigo", ls_null)
		RETURN 1
	END IF
end event

type gb_3 from groupbox within w_info_recepciones_guias
boolean visible = false
integer x = 430
integer y = 2352
integer width = 1614
integer height = 280
integer taborder = 160
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
end type

type st_5 from statictext within w_info_recepciones_guias
integer x = 251
integer y = 1360
integer width = 1847
integer height = 628
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

type cbx_1 from checkbox within w_info_recepciones_guias
boolean visible = false
integer x = 873
integer y = 2408
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

type dw_pesoneto from datawindow within w_info_recepciones_guias
boolean visible = false
integer x = 1015
integer y = 2496
integer width = 544
integer height = 84
integer taborder = 20
boolean bringtotop = true
string dataobject = "dddw_pesoneto"
boolean border = false
boolean livescroll = true
end type

event itemerror;RETURN 1
end event

type tit_peso from statictext within w_info_recepciones_guias
boolean visible = false
integer x = 823
integer y = 2512
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

type cbx_productorcons from checkbox within w_info_recepciones_guias
integer x = 1285
integer y = 932
integer width = 507
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
string text = "Consolidados"
boolean checked = true
end type

event clicked;IF This.Checked THEN
	dw_planta.Enabled			=	False
	cbx_productor.Enabled	=	False
	istr_mant.argumento[6]	=	'-9'
ELSE
	istr_mant.argumento[6]	=	'-1'	
	cbx_productor.Enabled			=	True
	cbx_productor.SetFocus()
END IF
end event

type st_16 from statictext within w_info_recepciones_guias
integer x = 251
integer y = 440
integer width = 1847
integer height = 156
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

type rb_recepciones from radiobutton within w_info_recepciones_guias
integer x = 677
integer y = 488
integer width = 512
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
string text = "Recepciones"
boolean checked = true
end type

event clicked;IF This.Checked THEN
	cbx_packing.Enabled	=	True
END IF

end event

type rb_despachos from radiobutton within w_info_recepciones_guias
integer x = 1248
integer y = 488
integer width = 576
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
string text = "Despachos"
end type

event clicked;//IF This.Checked THEN
//	cbx_packing.Enabled	=	False
//	dw_Packing.Enabled	=	False
//	istr_mant.argumento[7]	=	'-1'
//   cbx_packing.Checked	=	True
//END IF
end event

type st_variedad from statictext within w_info_recepciones_guias
integer x = 347
integer y = 1676
integer width = 306
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

type st_embalaje from statictext within w_info_recepciones_guias
integer x = 347
integer y = 1876
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
boolean enabled = false
string text = "Embalaje"
boolean focusrectangle = false
end type

type em_embalaje from editmask within w_info_recepciones_guias
integer x = 791
integer y = 1872
integer width = 261
integer height = 84
integer taborder = 110
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
maskdatatype maskdatatype = stringmask!
string mask = "xxxxxxxxxx"
end type

event modified;Integer		li_cliente
String		ls_embalaje, ls_Nombre

li_cliente	=	Integer(istr_mant.argumento[1]) // Cliente
ls_embalaje	=	This.Text

SELECT	emba_nombre
	INTO	:ls_Nombre
	FROM	dba.embalajesprod
	WHERE	clie_codigo	=	:li_cliente
	AND	emba_codigo	=	:ls_embalaje;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de tabla Embalajes")
	This.SetFocus()
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "Código de Embalaje no ha sido Definido.~r~r" + &
		"Ingrese o seleccione otro Código.")
	This.SetFocus()
ELSE
	istr_mant.argumento[10]	=	ls_embalaje
END IF
end event

type cb_buscaembalaje from commandbutton within w_info_recepciones_guias
integer x = 1065
integer y = 1872
integer width = 96
integer height = 84
integer taborder = 120
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

event clicked;
Str_busqueda	lstr_busq

lstr_busq.argum[1]	=	istr_mant.argumento[1] // Cliente

OpenWithParm(w_busc_embalajesprod, lstr_busq)

lstr_busq	       = Message.PowerObjectParm

IF lstr_busq.argum[2] = "" THEN
	em_embalaje.SetFocus()
ELSE
	em_embalaje.Text			=	lstr_busq.argum[2]
	istr_mant.argumento[10]	=	lstr_busq.argum[2]
END IF
end event

type cbx_embalaje from checkbox within w_info_recepciones_guias
integer x = 791
integer y = 1788
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
long backcolor = 33543637
string text = "Todos"
boolean checked = true
end type

event clicked;IF This.Checked THEN
	cbx_consembalaje.Enabled	=	True
	em_embalaje.Enabled			=	False
	cb_buscaembalaje.Enabled	=	False
	em_embalaje.Text				=	''
	istr_mant.argumento[10]		=	'-1'
ELSE
	cbx_consembalaje.Enabled	=	False
	cbx_consembalaje.Checked	=	False
	em_embalaje.Enabled			=	True
	cb_buscaembalaje.Enabled	=	True
END IF
end event

type cbx_consembalaje from checkbox within w_info_recepciones_guias
integer x = 1285
integer y = 1788
integer width = 507
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
string text = "Consolidados"
boolean checked = true
end type

event clicked;IF This.Checked = TRUE THEN
	istr_mant.argumento[10]	=	'-9'
	cbx_embalaje.Enabled	=	False
	cbx_embalaje.Checked	=	True
ELSE
	istr_mant.argumento[10]	=	'-1'
	cbx_embalaje.Enabled	=	True

END IF

end event

type uo_selespecie from uo_seleccion_especie within w_info_recepciones_guias
event destroy ( )
integer x = 795
integer y = 1396
integer height = 180
integer taborder = 240
boolean bringtotop = true
end type

on uo_selespecie.destroy
call uo_seleccion_especie::destroy
end on

event ue_cambio;call super::ue_cambio;IF IsNull(This.Codigo) THEN RETURN

CHOOSE CASE This.Codigo
	CASE -1, -9
		
	CASE ELSE
		uo_selvariedad.Filtra(This.Codigo)

END CHOOSE
end event

type uo_selvariedad from uo_seleccion_variedad within w_info_recepciones_guias
event destroy ( )
integer x = 795
integer y = 1580
integer taborder = 260
boolean bringtotop = true
end type

on uo_selvariedad.destroy
call uo_seleccion_variedad::destroy
end on

type cbx_varirotula from checkbox within w_info_recepciones_guias
integer x = 1705
integer y = 1672
integer width = 357
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
string text = "Rotulada"
end type

type st_15 from statictext within w_info_recepciones_guias
integer x = 251
integer y = 1988
integer width = 1847
integer height = 224
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

