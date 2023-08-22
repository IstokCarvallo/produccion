$PBExportHeader$w_info_produccionpuchosdiasrecep.srw
forward
global type w_info_produccionpuchosdiasrecep from w_para_informes
end type
type st_4 from statictext within w_info_produccionpuchosdiasrecep
end type
type dw_cliente from datawindow within w_info_produccionpuchosdiasrecep
end type
type st_6 from statictext within w_info_produccionpuchosdiasrecep
end type
type st_3 from statictext within w_info_produccionpuchosdiasrecep
end type
type st_variedad from statictext within w_info_produccionpuchosdiasrecep
end type
type st_1 from statictext within w_info_produccionpuchosdiasrecep
end type
type cbx_planta from checkbox within w_info_produccionpuchosdiasrecep
end type
type cbx_plantascons from checkbox within w_info_produccionpuchosdiasrecep
end type
type dw_planta from datawindow within w_info_produccionpuchosdiasrecep
end type
type cbx_peso from checkbox within w_info_produccionpuchosdiasrecep
end type
type tit_peso from statictext within w_info_produccionpuchosdiasrecep
end type
type dw_pesoneto from datawindow within w_info_produccionpuchosdiasrecep
end type
type gb_3 from groupbox within w_info_produccionpuchosdiasrecep
end type
type st_5 from statictext within w_info_produccionpuchosdiasrecep
end type
type st_17 from statictext within w_info_produccionpuchosdiasrecep
end type
type cbx_productor from checkbox within w_info_produccionpuchosdiasrecep
end type
type cbx_productorcons from checkbox within w_info_produccionpuchosdiasrecep
end type
type dw_productor from datawindow within w_info_produccionpuchosdiasrecep
end type
type st_18 from statictext within w_info_produccionpuchosdiasrecep
end type
type st_19 from statictext within w_info_produccionpuchosdiasrecep
end type
type cbx_embalajecons from checkbox within w_info_produccionpuchosdiasrecep
end type
type cbx_embalaje from checkbox within w_info_produccionpuchosdiasrecep
end type
type em_embalaje from editmask within w_info_produccionpuchosdiasrecep
end type
type cb_buscaembalaje from commandbutton within w_info_produccionpuchosdiasrecep
end type
type st_calidad from statictext within w_info_produccionpuchosdiasrecep
end type
type em_calidad from editmask within w_info_produccionpuchosdiasrecep
end type
type cbx_calidad from checkbox within w_info_produccionpuchosdiasrecep
end type
type cbx_calidadcons from checkbox within w_info_produccionpuchosdiasrecep
end type
type uo_selvariedad from uo_seleccion_variedad within w_info_produccionpuchosdiasrecep
end type
type uo_selespecie from uo_seleccion_especie within w_info_produccionpuchosdiasrecep
end type
type cbx_varirotula from checkbox within w_info_produccionpuchosdiasrecep
end type
type st_2 from statictext within w_info_produccionpuchosdiasrecep
end type
type em_desde from editmask within w_info_produccionpuchosdiasrecep
end type
type st_7 from statictext within w_info_produccionpuchosdiasrecep
end type
type em_hasta from editmask within w_info_produccionpuchosdiasrecep
end type
type st_11 from statictext within w_info_produccionpuchosdiasrecep
end type
type st_14 from statictext within w_info_produccionpuchosdiasrecep
end type
type rb_apallet from radiobutton within w_info_produccionpuchosdiasrecep
end type
type rb_pucho from radiobutton within w_info_produccionpuchosdiasrecep
end type
type st_8 from statictext within w_info_produccionpuchosdiasrecep
end type
type cbx_consrecep from checkbox within w_info_produccionpuchosdiasrecep
end type
type gb_4 from groupbox within w_info_produccionpuchosdiasrecep
end type
end forward

global type w_info_produccionpuchosdiasrecep from w_para_informes
integer x = 14
integer y = 32
integer width = 2555
integer height = 2312
string title = "Recepción Pallet/Pucho por Fecha"
boolean minbox = false
boolean maxbox = false
boolean resizable = false
windowtype windowtype = response!
string icon = "F:\Desarrollo\Producción\FrutaProcesada\Producc.ico"
st_4 st_4
dw_cliente dw_cliente
st_6 st_6
st_3 st_3
st_variedad st_variedad
st_1 st_1
cbx_planta cbx_planta
cbx_plantascons cbx_plantascons
dw_planta dw_planta
cbx_peso cbx_peso
tit_peso tit_peso
dw_pesoneto dw_pesoneto
gb_3 gb_3
st_5 st_5
st_17 st_17
cbx_productor cbx_productor
cbx_productorcons cbx_productorcons
dw_productor dw_productor
st_18 st_18
st_19 st_19
cbx_embalajecons cbx_embalajecons
cbx_embalaje cbx_embalaje
em_embalaje em_embalaje
cb_buscaembalaje cb_buscaembalaje
st_calidad st_calidad
em_calidad em_calidad
cbx_calidad cbx_calidad
cbx_calidadcons cbx_calidadcons
uo_selvariedad uo_selvariedad
uo_selespecie uo_selespecie
cbx_varirotula cbx_varirotula
st_2 st_2
em_desde em_desde
st_7 st_7
em_hasta em_hasta
st_11 st_11
st_14 st_14
rb_apallet rb_apallet
rb_pucho rb_pucho
st_8 st_8
cbx_consrecep cbx_consrecep
gb_4 gb_4
end type
global w_info_produccionpuchosdiasrecep w_info_produccionpuchosdiasrecep

type variables
str_busqueda istr_busq
str_mant istr_mant

DataWindowChild	idwc_tipopro, idwc_cliente, idwc_planta, idwc_productor,&
						idwc_packing, idwc_pesoneto, idwc_zonas, idwc_embalajes

String is_NomPlanta
uo_seleccion_especie		iuo_selespecie
uo_seleccion_variedad	iuo_selvariedad
uo_calibre								iuo_calibre
end variables

forward prototypes
public function boolean existepacking (integer li_planta)
public function boolean existeproductor (long ll_productor)
end prototypes

public function boolean existepacking (integer li_planta);Integer  li_codigo

SELECT	plde_codigo
INTO    :li_Codigo
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

on w_info_produccionpuchosdiasrecep.create
int iCurrent
call super::create
this.st_4=create st_4
this.dw_cliente=create dw_cliente
this.st_6=create st_6
this.st_3=create st_3
this.st_variedad=create st_variedad
this.st_1=create st_1
this.cbx_planta=create cbx_planta
this.cbx_plantascons=create cbx_plantascons
this.dw_planta=create dw_planta
this.cbx_peso=create cbx_peso
this.tit_peso=create tit_peso
this.dw_pesoneto=create dw_pesoneto
this.gb_3=create gb_3
this.st_5=create st_5
this.st_17=create st_17
this.cbx_productor=create cbx_productor
this.cbx_productorcons=create cbx_productorcons
this.dw_productor=create dw_productor
this.st_18=create st_18
this.st_19=create st_19
this.cbx_embalajecons=create cbx_embalajecons
this.cbx_embalaje=create cbx_embalaje
this.em_embalaje=create em_embalaje
this.cb_buscaembalaje=create cb_buscaembalaje
this.st_calidad=create st_calidad
this.em_calidad=create em_calidad
this.cbx_calidad=create cbx_calidad
this.cbx_calidadcons=create cbx_calidadcons
this.uo_selvariedad=create uo_selvariedad
this.uo_selespecie=create uo_selespecie
this.cbx_varirotula=create cbx_varirotula
this.st_2=create st_2
this.em_desde=create em_desde
this.st_7=create st_7
this.em_hasta=create em_hasta
this.st_11=create st_11
this.st_14=create st_14
this.rb_apallet=create rb_apallet
this.rb_pucho=create rb_pucho
this.st_8=create st_8
this.cbx_consrecep=create cbx_consrecep
this.gb_4=create gb_4
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_4
this.Control[iCurrent+2]=this.dw_cliente
this.Control[iCurrent+3]=this.st_6
this.Control[iCurrent+4]=this.st_3
this.Control[iCurrent+5]=this.st_variedad
this.Control[iCurrent+6]=this.st_1
this.Control[iCurrent+7]=this.cbx_planta
this.Control[iCurrent+8]=this.cbx_plantascons
this.Control[iCurrent+9]=this.dw_planta
this.Control[iCurrent+10]=this.cbx_peso
this.Control[iCurrent+11]=this.tit_peso
this.Control[iCurrent+12]=this.dw_pesoneto
this.Control[iCurrent+13]=this.gb_3
this.Control[iCurrent+14]=this.st_5
this.Control[iCurrent+15]=this.st_17
this.Control[iCurrent+16]=this.cbx_productor
this.Control[iCurrent+17]=this.cbx_productorcons
this.Control[iCurrent+18]=this.dw_productor
this.Control[iCurrent+19]=this.st_18
this.Control[iCurrent+20]=this.st_19
this.Control[iCurrent+21]=this.cbx_embalajecons
this.Control[iCurrent+22]=this.cbx_embalaje
this.Control[iCurrent+23]=this.em_embalaje
this.Control[iCurrent+24]=this.cb_buscaembalaje
this.Control[iCurrent+25]=this.st_calidad
this.Control[iCurrent+26]=this.em_calidad
this.Control[iCurrent+27]=this.cbx_calidad
this.Control[iCurrent+28]=this.cbx_calidadcons
this.Control[iCurrent+29]=this.uo_selvariedad
this.Control[iCurrent+30]=this.uo_selespecie
this.Control[iCurrent+31]=this.cbx_varirotula
this.Control[iCurrent+32]=this.st_2
this.Control[iCurrent+33]=this.em_desde
this.Control[iCurrent+34]=this.st_7
this.Control[iCurrent+35]=this.em_hasta
this.Control[iCurrent+36]=this.st_11
this.Control[iCurrent+37]=this.st_14
this.Control[iCurrent+38]=this.rb_apallet
this.Control[iCurrent+39]=this.rb_pucho
this.Control[iCurrent+40]=this.st_8
this.Control[iCurrent+41]=this.cbx_consrecep
this.Control[iCurrent+42]=this.gb_4
end on

on w_info_produccionpuchosdiasrecep.destroy
call super::destroy
destroy(this.st_4)
destroy(this.dw_cliente)
destroy(this.st_6)
destroy(this.st_3)
destroy(this.st_variedad)
destroy(this.st_1)
destroy(this.cbx_planta)
destroy(this.cbx_plantascons)
destroy(this.dw_planta)
destroy(this.cbx_peso)
destroy(this.tit_peso)
destroy(this.dw_pesoneto)
destroy(this.gb_3)
destroy(this.st_5)
destroy(this.st_17)
destroy(this.cbx_productor)
destroy(this.cbx_productorcons)
destroy(this.dw_productor)
destroy(this.st_18)
destroy(this.st_19)
destroy(this.cbx_embalajecons)
destroy(this.cbx_embalaje)
destroy(this.em_embalaje)
destroy(this.cb_buscaembalaje)
destroy(this.st_calidad)
destroy(this.em_calidad)
destroy(this.cbx_calidad)
destroy(this.cbx_calidadcons)
destroy(this.uo_selvariedad)
destroy(this.uo_selespecie)
destroy(this.cbx_varirotula)
destroy(this.st_2)
destroy(this.em_desde)
destroy(this.st_7)
destroy(this.em_hasta)
destroy(this.st_11)
destroy(this.st_14)
destroy(this.rb_apallet)
destroy(this.rb_pucho)
destroy(this.st_8)
destroy(this.cbx_consrecep)
destroy(this.gb_4)
end on

event open;Boolean	lb_Cerrar

x	=	0
y	=	0

iuo_calibre   				=	Create uo_calibre

dw_cliente.GetChild("clie_codigo", idwc_cliente)
idwc_cliente.SetTransObject(SQLCA)
idwc_cliente.Retrieve()
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
istr_mant.argumento[2]	= 	"-9"							//	planta
istr_mant.argumento[6]  =  "-9"							//	Productor
istr_mant.argumento[8]  =  "1"							//	peso
istr_mant.argumento[16]	= 	"-Z" 							// embalajes
istr_mant.argumento[18]	= 	"-Z" 							// calidad
istr_mant.argumento[33]	=	'2'							// puchos

end event

event resize;//
end event

type st_computador from w_para_informes`st_computador within w_info_produccionpuchosdiasrecep
end type

type st_usuario from w_para_informes`st_usuario within w_info_produccionpuchosdiasrecep
end type

type st_temporada from w_para_informes`st_temporada within w_info_produccionpuchosdiasrecep
end type

type p_logo from w_para_informes`p_logo within w_info_produccionpuchosdiasrecep
end type

type st_titulo from w_para_informes`st_titulo within w_info_produccionpuchosdiasrecep
integer width = 1847
string text = "Recepción Pallet/Pucho por Fecha"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_produccionpuchosdiasrecep
integer x = 2194
integer y = 1696
integer taborder = 240
end type

event pb_acepta::clicked;SetPointer(Arrow!)
 
Integer	ll_Fila, li_cliente, li_planta, li_especie, li_semana, li_NroSemana,&
			li_tipo = 1, li_variedad, li_varirotula, li_consrecep
Long		ll_semana_ano,ll_productor
Date		ld_desde, ld_hasta, ld_FechaInicio, ld_Fecha
String	ls_cajas, ls_especie, ls_planta, ls_productor, ls_encabezado, ls_embalaje, ls_calidad, &
         ls_variedad, ls_embalajenom, ls_calidadnom,texto_desde, texto_hasta, texto_fecha
istr_info.titulo	= 'PRODUCCION PALLET Y PUCHOS POR FECHAS'

OpenWithParm(vinf, istr_info)
vinf.dw_1.DataObject = "dw_info_produccionpuchosdiasrecep"

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
ll_productor	=	Long(istr_mant.argumento[6])
ls_embalaje		=	istr_mant.argumento[16]
ls_calidad		=	istr_mant.argumento[18]
texto_desde		=  f_fecha_texto(String(date(em_desde.text)), 1)
texto_hasta		=	f_fecha_texto(String(date(em_hasta.text)), 1)
texto_fecha		=	"Desde El :  " + texto_desde + "   Hasta El :  " + texto_hasta

IF cbx_peso.Checked=False THEN
	ls_cajas = "Bulto"
	istr_mant.argumento[8]	=	"1"
ELSE
	istr_mant.argumento[8]	=	String(dw_pesoneto.Object.enva_pesone[1])
	ls_cajas = "Base " + istr_mant.argumento[8] 
END IF

IF cbx_consrecep.Checked THEN
	li_consrecep = 1
ELSE
	li_consrecep = 0
END IF

ls_especie	= String(uo_selespecie.Codigo,'00')+" "+uo_selespecie.Nombre
ls_variedad = String(uo_selvariedad.Codigo,'00')+" "+uo_selvariedad.Nombre

vinf.dw_1.SetTransObject(sqlca)

ll_Fila	=	vinf.dw_1.Retrieve(li_cliente, li_planta,uo_selespecie.Codigo, &
										 Dec(istr_mant.argumento[8]),ll_productor, &
										 uo_selvariedad.Codigo,li_varirotula,ls_calidad, &
										 ls_embalaje,date(em_desde.text),date(em_hasta.text),Integer(istr_mant.argumento[33]),&
										 li_consrecep)

IF ll_Fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
			   	"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)

ELSEIF ll_Fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", &
	             StopSign!, Ok!)

ELSE
	F_Membrete(vinf.dw_1)
	vinf.dw_1.Modify("base.text = '" + ls_cajas + "'")
	vinf.dw_1.Modify("fechas.text = '" + texto_fecha + "'")
	IF gs_Ambiente <> 'Windows' THEN
		F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
	END IF
END IF

SetPointer(Arrow!)
end event

type pb_salir from w_para_informes`pb_salir within w_info_produccionpuchosdiasrecep
integer x = 2194
integer y = 1984
integer taborder = 250
end type

type st_4 from statictext within w_info_produccionpuchosdiasrecep
integer x = 247
integer y = 440
integer width = 1847
integer height = 340
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

type dw_cliente from datawindow within w_info_produccionpuchosdiasrecep
integer x = 786
integer y = 484
integer width = 1143
integer height = 120
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

type st_6 from statictext within w_info_produccionpuchosdiasrecep
integer x = 343
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
long backcolor = 33543637
string text = "Cliente"
boolean focusrectangle = false
end type

type st_3 from statictext within w_info_produccionpuchosdiasrecep
integer x = 343
integer y = 840
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

type st_variedad from statictext within w_info_produccionpuchosdiasrecep
integer x = 343
integer y = 1008
integer width = 279
integer height = 96
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

type st_1 from statictext within w_info_produccionpuchosdiasrecep
integer x = 343
integer y = 656
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
string text = "Planta"
boolean focusrectangle = false
end type

type cbx_planta from checkbox within w_info_produccionpuchosdiasrecep
integer x = 786
integer y = 580
integer width = 402
integer height = 76
integer taborder = 20
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
	dw_planta.Object.plde_codigo.BackGround.Color	=	RGB(192, 192, 192)
	istr_mant.argumento[2]									=	'-1'
ELSE
	cbx_plantascons.Enabled									=	False
	cbx_plantascons.Checked									=	False
	dw_planta.Enabled											=	True
	dw_planta.Object.plde_codigo.BackGround.Color	=	RGB(255, 255, 255)
	dw_planta.SetFocus()

END IF
end event

type cbx_plantascons from checkbox within w_info_produccionpuchosdiasrecep
integer x = 1262
integer y = 580
integer width = 471
integer height = 80
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
string text = "Consolidadas"
boolean checked = true
end type

event clicked;IF This.Checked = TRUE THEN
	istr_mant.argumento[2]	=	'-9'
ELSE
	istr_mant.argumento[2]	=	'-1'
END IF

end event

type dw_planta from datawindow within w_info_produccionpuchosdiasrecep
integer x = 786
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

type cbx_peso from checkbox within w_info_produccionpuchosdiasrecep
integer x = 384
integer y = 1800
integer width = 631
integer height = 80
integer taborder = 180
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

type tit_peso from statictext within w_info_produccionpuchosdiasrecep
integer x = 1111
integer y = 1808
integer width = 160
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

type dw_pesoneto from datawindow within w_info_produccionpuchosdiasrecep
integer x = 1280
integer y = 1796
integer width = 695
integer height = 92
integer taborder = 200
boolean bringtotop = true
string dataobject = "dddw_pesoneto"
boolean border = false
boolean livescroll = true
end type

event itemerror;RETURN 1
end event

type gb_3 from groupbox within w_info_produccionpuchosdiasrecep
integer x = 320
integer y = 1736
integer width = 1705
integer height = 176
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
end type

type st_5 from statictext within w_info_produccionpuchosdiasrecep
integer x = 247
integer y = 780
integer width = 1847
integer height = 948
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

type st_17 from statictext within w_info_produccionpuchosdiasrecep
integer x = 247
integer y = 1728
integer width = 1847
integer height = 208
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

type cbx_productor from checkbox within w_info_produccionpuchosdiasrecep
integer x = 786
integer y = 1152
integer width = 402
integer height = 76
integer taborder = 80
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
	cbx_productorcons.Enabled									=	True
	dw_productor.Enabled											=	False
	dw_productor.Object.prod_codigo.BackGround.Color	=	RGB(192, 192, 192)
	istr_mant.argumento[6]									=	'-1'
ELSE
	cbx_productorcons.Enabled									=	False
	cbx_productorcons.Checked									=	False
	dw_productor.Enabled											=	True
	dw_productor.Object.prod_codigo.BackGround.Color	=	RGB(255, 255, 255)
	dw_productor.SetFocus()
END IF
end event

type cbx_productorcons from checkbox within w_info_produccionpuchosdiasrecep
integer x = 1262
integer y = 1152
integer width = 471
integer height = 80
integer taborder = 90
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
	istr_mant.argumento[6]	=	'-9'
ELSE
	istr_mant.argumento[6]	=	'-1'
END IF


end event

type dw_productor from datawindow within w_info_produccionpuchosdiasrecep
integer x = 782
integer y = 1232
integer width = 974
integer height = 92
integer taborder = 100
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_productores_clientes"
boolean border = false
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

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

event itemerror;RETURN 1
end event

type st_18 from statictext within w_info_produccionpuchosdiasrecep
integer x = 343
integer y = 1240
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

type st_19 from statictext within w_info_produccionpuchosdiasrecep
integer x = 343
integer y = 1424
integer width = 315
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
string text = "Embalajes"
boolean focusrectangle = false
end type

type cbx_embalajecons from checkbox within w_info_produccionpuchosdiasrecep
integer x = 1262
integer y = 1336
integer width = 471
integer height = 80
integer taborder = 120
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
	istr_mant.argumento[16]	=	'-Z'
ELSE
	istr_mant.argumento[16]	=	'Z'
END IF

end event

type cbx_embalaje from checkbox within w_info_produccionpuchosdiasrecep
integer x = 786
integer y = 1336
integer width = 402
integer height = 80
integer taborder = 110
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
	cbx_embalajecons.Enabled	=	True
	em_embalaje.Enabled			=	False
	cb_buscaembalaje.Enabled	=	False
	em_embalaje.Text				=	''
	istr_mant.argumento[16]		=	'Z'
ELSE
	cbx_embalajecons.Enabled	=	False
	cbx_embalajecons.Checked	=	False
	em_embalaje.Enabled			=	True
	cb_buscaembalaje.Enabled	=	True
END IF

end event

type em_embalaje from editmask within w_info_produccionpuchosdiasrecep
integer x = 786
integer y = 1416
integer width = 297
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
	istr_mant.argumento[16]	=	ls_embalaje
END IF

end event

type cb_buscaembalaje from commandbutton within w_info_produccionpuchosdiasrecep
integer x = 1106
integer y = 1424
integer width = 96
integer height = 84
integer taborder = 140
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

lstr_busq.argum[1]	=	istr_mant.argumento[1] // Cliente

OpenWithParm(w_busc_embalajesprod, lstr_busq)

lstr_busq	       = Message.PowerObjectParm

IF lstr_busq.argum[2] = "" THEN
	em_embalaje.SetFocus()
ELSE
	em_embalaje.Text			=	lstr_busq.argum[2]
	istr_mant.argumento[16]	=	lstr_busq.argum[2]
END IF

end event

type st_calidad from statictext within w_info_produccionpuchosdiasrecep
integer x = 343
integer y = 1612
integer width = 256
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
string text = "Calidad"
boolean focusrectangle = false
end type

type em_calidad from editmask within w_info_produccionpuchosdiasrecep
integer x = 786
integer y = 1600
integer width = 297
integer height = 96
integer taborder = 170
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
textcase textcase = upper!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = stringmask!
string mask = "xxx"
end type

event modified;Integer	li_especie, li_variedad, li_cantid
String	ls_calibre

li_especie	=	Integer(istr_mant.argumento[5]) // Especie
li_variedad	=	Integer(istr_mant.argumento[13]) // Variedad
ls_calibre	=	This.Text

IF This.Text <> '' THEN
	
	li_especie	=	Integer(uo_selespecie.Codigo) // Especie
	li_variedad	=	Integer(uo_selvariedad.Codigo) // Variedad
	ls_calibre	=	This.Text
	
	IF NOT iuo_calibre.existe(li_especie,li_variedad,ls_calibre,True,sqlca) THEN
		em_calidad.Text = ''
		This.SetFocus()
		Return 1
	ELSE	
		istr_mant.argumento[18]	 = iuo_calibre.calibre
		em_calidad.Text			 = iuo_calibre.calibre
		Return 1
	END IF	
END IF	



end event

type cbx_calidad from checkbox within w_info_produccionpuchosdiasrecep
integer x = 786
integer y = 1524
integer width = 297
integer height = 76
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
string text = "Todas"
boolean checked = true
end type

event clicked;IF This.Checked THEN
	cbx_calidadcons.Enabled	=	True
	em_calidad.Text			=	''
	em_calidad.Enabled		=	False
	istr_mant.argumento[18]	=	'Z'
ELSE
	cbx_calidadcons.Enabled	=	False
	cbx_calidadcons.Checked	=	False
	em_calidad.Enabled		=	True
	em_calidad.SetFocus()
END IF

end event

type cbx_calidadcons from checkbox within w_info_produccionpuchosdiasrecep
integer x = 1262
integer y = 1524
integer width = 471
integer height = 80
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
string text = "Consolidadas"
boolean checked = true
end type

event clicked;IF This.Checked = TRUE THEN
	istr_mant.argumento[18]	=	'-Z'
ELSE
	istr_mant.argumento[18]	=	'Z'
END IF

end event

type uo_selvariedad from uo_seleccion_variedad within w_info_produccionpuchosdiasrecep
integer x = 786
integer y = 976
integer taborder = 60
boolean bringtotop = true
end type

on uo_selvariedad.destroy
call uo_seleccion_variedad::destroy
end on

type uo_selespecie from uo_seleccion_especie within w_info_produccionpuchosdiasrecep
integer x = 786
integer y = 792
integer height = 180
integer taborder = 50
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

type cbx_varirotula from checkbox within w_info_produccionpuchosdiasrecep
integer x = 1687
integer y = 1068
integer width = 366
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
string text = "Rotulada"
end type

type st_2 from statictext within w_info_produccionpuchosdiasrecep
integer x = 311
integer y = 1968
integer width = 517
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
string text = "Recepción Inicio "
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type em_desde from editmask within w_info_produccionpuchosdiasrecep
integer x = 809
integer y = 1952
integer width = 393
integer height = 96
integer taborder = 210
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

type st_7 from statictext within w_info_produccionpuchosdiasrecep
integer x = 1280
integer y = 1968
integer width = 297
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
string text = "Término"
boolean focusrectangle = false
end type

type em_hasta from editmask within w_info_produccionpuchosdiasrecep
integer x = 1568
integer y = 1952
integer width = 393
integer height = 96
integer taborder = 220
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

type st_11 from statictext within w_info_produccionpuchosdiasrecep
integer x = 247
integer y = 1936
integer width = 1847
integer height = 132
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

type st_14 from statictext within w_info_produccionpuchosdiasrecep
integer x = 247
integer y = 2068
integer width = 841
integer height = 112
integer taborder = 230
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

type rb_apallet from radiobutton within w_info_produccionpuchosdiasrecep
integer x = 302
integer y = 2096
integer width = 283
integer height = 80
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
string text = "Pallets"
end type

event clicked;istr_mant.argumento[33]	=	'1'
end event

type rb_pucho from radiobutton within w_info_produccionpuchosdiasrecep
integer x = 731
integer y = 2096
integer width = 302
integer height = 80
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
string text = "Puchos"
boolean checked = true
end type

event clicked;istr_mant.argumento[33]	=	'2'
end event

type st_8 from statictext within w_info_produccionpuchosdiasrecep
integer x = 1088
integer y = 2068
integer width = 1006
integer height = 112
integer taborder = 230
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

type cbx_consrecep from checkbox within w_info_produccionpuchosdiasrecep
integer x = 1202
integer y = 2092
integer width = 850
integer height = 80
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
string text = "Consolida Nro.Recepción  "
end type

type gb_4 from groupbox within w_info_produccionpuchosdiasrecep
boolean visible = false
integer x = 343
integer y = 1836
integer width = 1614
integer height = 316
integer taborder = 190
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
end type

