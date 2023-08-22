$PBExportHeader$w_info_produccion_packprodzonaembj.srw
forward
global type w_info_produccion_packprodzonaembj from w_para_informes
end type
type st_4 from statictext within w_info_produccion_packprodzonaembj
end type
type dw_cliente from datawindow within w_info_produccion_packprodzonaembj
end type
type st_6 from statictext within w_info_produccion_packprodzonaembj
end type
type st_8 from statictext within w_info_produccion_packprodzonaembj
end type
type st_5 from statictext within w_info_produccion_packprodzonaembj
end type
type cbx_peso from checkbox within w_info_produccion_packprodzonaembj
end type
type dw_pesoneto from datawindow within w_info_produccion_packprodzonaembj
end type
type tit_peso from statictext within w_info_produccion_packprodzonaembj
end type
type st_7 from statictext within w_info_produccion_packprodzonaembj
end type
type st_9 from statictext within w_info_produccion_packprodzonaembj
end type
type cbx_zonas from checkbox within w_info_produccion_packprodzonaembj
end type
type dw_zonas from datawindow within w_info_produccion_packprodzonaembj
end type
type st_1 from statictext within w_info_produccion_packprodzonaembj
end type
type cbx_tipoproductor from checkbox within w_info_produccion_packprodzonaembj
end type
type dw_tipoprodu from datawindow within w_info_produccion_packprodzonaembj
end type
type st_2 from statictext within w_info_produccion_packprodzonaembj
end type
type em_fech_ini from editmask within w_info_produccion_packprodzonaembj
end type
type st_3 from statictext within w_info_produccion_packprodzonaembj
end type
type em_fech_fin from editmask within w_info_produccion_packprodzonaembj
end type
type st_10 from statictext within w_info_produccion_packprodzonaembj
end type
type st_variedad from statictext within w_info_produccion_packprodzonaembj
end type
type st_calidad from statictext within w_info_produccion_packprodzonaembj
end type
type em_calidad from editmask within w_info_produccion_packprodzonaembj
end type
type cbx_calidad from checkbox within w_info_produccion_packprodzonaembj
end type
type cbx_conscalidad from checkbox within w_info_produccion_packprodzonaembj
end type
type st_11 from statictext within w_info_produccion_packprodzonaembj
end type
type dw_packing from datawindow within w_info_produccion_packprodzonaembj
end type
type cbx_packing from checkbox within w_info_produccion_packprodzonaembj
end type
type cbx_packingcons from checkbox within w_info_produccion_packprodzonaembj
end type
type uo_selespecie from uo_seleccion_especie within w_info_produccion_packprodzonaembj
end type
type uo_selvariedad from uo_seleccion_variedad within w_info_produccion_packprodzonaembj
end type
type cbx_varirotula from checkbox within w_info_produccion_packprodzonaembj
end type
type uo_selproductor from uo_seleccion_varios_productores_clientes within w_info_produccion_packprodzonaembj
end type
end forward

global type w_info_produccion_packprodzonaembj from w_para_informes
integer x = 14
integer y = 32
integer width = 2642
integer height = 2424
string title = "Producción Packing/Productor/Variedad/Zonas/Semanas"
boolean minbox = false
boolean maxbox = false
boolean resizable = false
windowtype windowtype = response!
string icon = "F:\Desarrollo\Producción\FrutaProcesada\Producc.ico"
event ue_imprimeresu ( )
st_4 st_4
dw_cliente dw_cliente
st_6 st_6
st_8 st_8
st_5 st_5
cbx_peso cbx_peso
dw_pesoneto dw_pesoneto
tit_peso tit_peso
st_7 st_7
st_9 st_9
cbx_zonas cbx_zonas
dw_zonas dw_zonas
st_1 st_1
cbx_tipoproductor cbx_tipoproductor
dw_tipoprodu dw_tipoprodu
st_2 st_2
em_fech_ini em_fech_ini
st_3 st_3
em_fech_fin em_fech_fin
st_10 st_10
st_variedad st_variedad
st_calidad st_calidad
em_calidad em_calidad
cbx_calidad cbx_calidad
cbx_conscalidad cbx_conscalidad
st_11 st_11
dw_packing dw_packing
cbx_packing cbx_packing
cbx_packingcons cbx_packingcons
uo_selespecie uo_selespecie
uo_selvariedad uo_selvariedad
cbx_varirotula cbx_varirotula
uo_selproductor uo_selproductor
end type
global w_info_produccion_packprodzonaembj w_info_produccion_packprodzonaembj

type variables
str_busqueda istr_busq
str_mant istr_mant

DataWindowChild	idwc_tipopro, idwc_cliente, idwc_planta, idwc_zonas, &
						idwc_productor, idwc_packing, idwc_pesoneto, idwc_recibidor
						
String is_NomPlanta

uo_plantadesp      			iuo_plantadesp     
uo_Productores    			iuo_Productores  
uo_tipoproductor  			iuo_tipoproductor
uo_seleccion_especie		iuo_selespecie
uo_seleccion_variedad	iuo_selvariedad
uo_calibre					iuo_calibre
end variables

forward prototypes
public function boolean existeespecie (integer cliente, integer especie)
public function boolean existeproductor (integer li_cliente, long ll_productor)
end prototypes

public function boolean existeespecie (integer cliente, integer especie);String		ls_Nombre

SELECT	espe_nombre
	INTO	:ls_Nombre
	FROM	dbo.especies
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

public function boolean existeproductor (integer li_cliente, long ll_productor);String	ls_Nombre

SELECT	prod_nombre
	INTO	:ls_Nombre
	FROM	dbo.productores
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

on w_info_produccion_packprodzonaembj.create
int iCurrent
call super::create
this.st_4=create st_4
this.dw_cliente=create dw_cliente
this.st_6=create st_6
this.st_8=create st_8
this.st_5=create st_5
this.cbx_peso=create cbx_peso
this.dw_pesoneto=create dw_pesoneto
this.tit_peso=create tit_peso
this.st_7=create st_7
this.st_9=create st_9
this.cbx_zonas=create cbx_zonas
this.dw_zonas=create dw_zonas
this.st_1=create st_1
this.cbx_tipoproductor=create cbx_tipoproductor
this.dw_tipoprodu=create dw_tipoprodu
this.st_2=create st_2
this.em_fech_ini=create em_fech_ini
this.st_3=create st_3
this.em_fech_fin=create em_fech_fin
this.st_10=create st_10
this.st_variedad=create st_variedad
this.st_calidad=create st_calidad
this.em_calidad=create em_calidad
this.cbx_calidad=create cbx_calidad
this.cbx_conscalidad=create cbx_conscalidad
this.st_11=create st_11
this.dw_packing=create dw_packing
this.cbx_packing=create cbx_packing
this.cbx_packingcons=create cbx_packingcons
this.uo_selespecie=create uo_selespecie
this.uo_selvariedad=create uo_selvariedad
this.cbx_varirotula=create cbx_varirotula
this.uo_selproductor=create uo_selproductor
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_4
this.Control[iCurrent+2]=this.dw_cliente
this.Control[iCurrent+3]=this.st_6
this.Control[iCurrent+4]=this.st_8
this.Control[iCurrent+5]=this.st_5
this.Control[iCurrent+6]=this.cbx_peso
this.Control[iCurrent+7]=this.dw_pesoneto
this.Control[iCurrent+8]=this.tit_peso
this.Control[iCurrent+9]=this.st_7
this.Control[iCurrent+10]=this.st_9
this.Control[iCurrent+11]=this.cbx_zonas
this.Control[iCurrent+12]=this.dw_zonas
this.Control[iCurrent+13]=this.st_1
this.Control[iCurrent+14]=this.cbx_tipoproductor
this.Control[iCurrent+15]=this.dw_tipoprodu
this.Control[iCurrent+16]=this.st_2
this.Control[iCurrent+17]=this.em_fech_ini
this.Control[iCurrent+18]=this.st_3
this.Control[iCurrent+19]=this.em_fech_fin
this.Control[iCurrent+20]=this.st_10
this.Control[iCurrent+21]=this.st_variedad
this.Control[iCurrent+22]=this.st_calidad
this.Control[iCurrent+23]=this.em_calidad
this.Control[iCurrent+24]=this.cbx_calidad
this.Control[iCurrent+25]=this.cbx_conscalidad
this.Control[iCurrent+26]=this.st_11
this.Control[iCurrent+27]=this.dw_packing
this.Control[iCurrent+28]=this.cbx_packing
this.Control[iCurrent+29]=this.cbx_packingcons
this.Control[iCurrent+30]=this.uo_selespecie
this.Control[iCurrent+31]=this.uo_selvariedad
this.Control[iCurrent+32]=this.cbx_varirotula
this.Control[iCurrent+33]=this.uo_selproductor
end on

on w_info_produccion_packprodzonaembj.destroy
call super::destroy
destroy(this.st_4)
destroy(this.dw_cliente)
destroy(this.st_6)
destroy(this.st_8)
destroy(this.st_5)
destroy(this.cbx_peso)
destroy(this.dw_pesoneto)
destroy(this.tit_peso)
destroy(this.st_7)
destroy(this.st_9)
destroy(this.cbx_zonas)
destroy(this.dw_zonas)
destroy(this.st_1)
destroy(this.cbx_tipoproductor)
destroy(this.dw_tipoprodu)
destroy(this.st_2)
destroy(this.em_fech_ini)
destroy(this.st_3)
destroy(this.em_fech_fin)
destroy(this.st_10)
destroy(this.st_variedad)
destroy(this.st_calidad)
destroy(this.em_calidad)
destroy(this.cbx_calidad)
destroy(this.cbx_conscalidad)
destroy(this.st_11)
destroy(this.dw_packing)
destroy(this.cbx_packing)
destroy(this.cbx_packingcons)
destroy(this.uo_selespecie)
destroy(this.uo_selvariedad)
destroy(this.cbx_varirotula)
destroy(this.uo_selproductor)
end on

event open;call super::open;Boolean lb_Cerrar

x	=	0
y	=	0

iuo_plantadesp = create uo_plantadesp

dw_cliente.GetChild("clie_codigo", idwc_cliente)
idwc_cliente.SetTransObject(SQLCA)
idwc_cliente.Retrieve(gi_CodExport)
dw_cliente.InsertRow(0)
dw_cliente.SetItem(1, "clie_codigo", gi_CodExport)

dw_zonas.GetChild("zona_codigo", idwc_zonas)
idwc_zonas.SetTransObject(sqlca)
idwc_zonas.Retrieve(-1)
dw_zonas.InsertRow(0)

iuo_calibre   						=	Create uo_calibre

dw_packing.GetChild("plde_codigo", idwc_packing)
idwc_packing.SetTransObject(sqlca)
idwc_packing.Retrieve(2)
dw_packing.InsertRow(0)

dw_tipoprodu.GetChild("tipr_codigo", idwc_tipopro)
idwc_tipopro.SetTransObject(sqlca)
idwc_tipopro.Retrieve(1)
dw_tipoprodu.InsertRow(0)
//dw_tipoprodu.SetItem(1, "tipr_codigo", 1)

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

em_fech_ini.Text			=	String(Relativedate(Today(), -365))
em_fech_fin.Text			=	String(Today())

istr_mant.argumento[1]	= 	String(gi_CodExport) 							// cliente
istr_mant.argumento[2]	=	"-1"													// Zona  
istr_mant.argumento[3]	=	"-1"													// Tipo Productor
istr_mant.argumento[4]  =  "-9"													//	productor
istr_mant.argumento[5]  =  "1"													//	peso
istr_mant.argumento[6]  =  String(Relativedate(Today(), -365))			//	fecha inicial
istr_mant.argumento[7]  =  String(Today())									//	fecha final
istr_mant.argumento[8]  =  "-1"                                      // Packing
istr_mant.argumento[12]	= 	"-9" 													// Calibre

dw_zonas.Object.zona_codigo.BackGround.Color		=	RGB(166,180,210)
dw_tipoprodu.Object.tipr_codigo.BackGround.Color	=	RGB(166,180,210)
dw_packing.Object.plde_codigo.BackGround.Color	    =	RGB(166,180,210)
end event

type pb_excel from w_para_informes`pb_excel within w_info_produccion_packprodzonaembj
end type

type st_computador from w_para_informes`st_computador within w_info_produccion_packprodzonaembj
end type

type st_usuario from w_para_informes`st_usuario within w_info_produccion_packprodzonaembj
end type

type st_temporada from w_para_informes`st_temporada within w_info_produccion_packprodzonaembj
end type

type p_logo from w_para_informes`p_logo within w_info_produccion_packprodzonaembj
end type

type st_titulo from w_para_informes`st_titulo within w_info_produccion_packprodzonaembj
integer width = 1847
string text = "Producción Packing/Productor//Embalaje/Zonas"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_produccion_packprodzonaembj
string tag = "Imprimir Reporte"
integer x = 2194
integer y = 1744
integer taborder = 120
end type

event pb_acepta::clicked;SetPointer(Arrow!)
 
Integer	ll_Fila, li_cliente, li_tipo = 1, li_tipoprodu, li_zona, li_packing, li_varirotula
Date		ld_desde, ld_hasta
String	ls_cajas, ls_productor, ls_encabezado, ls_tipoprodu,ls_Calibre,ls_packing, ls_lista
Long		ll_productor

istr_info.titulo	= 'PRODUCCION POR PACKING/PRODUCTOR/EMBALAJE/ZONAS'
OpenWithParm(vinf, istr_info)
vinf.dw_1.DataObject = "dw_info_packprodzonaembj_enc"
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
li_zona	 		=	Integer(istr_mant.argumento[2])
li_tipoprodu	=	Integer(istr_mant.argumento[3])
li_packing     =  Integer(istr_mant.argumento[8])
ls_Calibre     =  istr_mant.argumento[12]

IF cbx_peso.Checked	=	False THEN
	ls_cajas = "Bulto"
	istr_mant.argumento[5]	=	"1"
ELSE
	istr_mant.argumento[5]	=	String(dw_pesoneto.Object.enva_pesone[1])
	ls_cajas						=	"Base " + istr_mant.argumento[5] 
END IF

ld_Desde	=	Date(istr_mant.argumento[6])
ld_Hasta	=	Date(istr_mant.argumento[7])

/*
productor
*/
ls_lista = uo_selproductor.Lista

IF ls_lista = '-9' THEN
	ls_productor = 'Consolidados'
ELSE
	IF ls_lista = '-1' THEN
		ls_productor = 'Todos'
	ELSE
		SELECT prod_nombre INTO:ls_productor
		FROM dbo.productores
		WHERE prod_codigo=:ls_lista;
		ls_productor = String(ls_lista,'00000')+" "+ls_productor
		
		IF ls_productor = '00000' THEN
			ls_productor = ls_lista
		END IF	
		
	END IF
END IF

IF cbx_packingcons.checked THEN
	ls_packing = 'Consolidados'
ELSE
	IF cbx_packing.checked THEN
		ls_packing = 'Todos'
	ELSE
		SELECT plde_nombre INTO:ls_packing
		FROM dbo.plantadesp
		WHERE plde_codigo=:li_packing;
		ls_packing = String(li_packing,'0000')+" "+ls_packing
	END IF
END IF

IF cbx_tipoproductor.checked THEN
		ls_tipoprodu = 'Todos'
ELSE
		SELECT tipr_nombre INTO:ls_tipoprodu
		FROM dbo.tipoproduc
		WHERE tipr_codigo=:li_tipoprodu;
		ls_tipoprodu = String(li_tipoprodu,'000')+" "+ls_tipoprodu
END IF

IF cbx_varirotula.Checked THEN
	li_varirotula = 1
ELSE
	li_varirotula = 0
END IF
	
vinf.dw_1.SetTransObject(sqlca)

ll_Fila	=	vinf.dw_1.Retrieve(li_cliente, li_zona, li_tipoprodu, ld_desde, ld_hasta, & 
										 Dec(istr_mant.argumento[5]), uo_selespecie.Codigo,&
										 uo_selvariedad.Codigo, ls_Calibre, li_packing, li_varirotula, ls_lista)

IF ll_Fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
			   	"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)

ELSEIF ll_Fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", &
	             StopSign!, Ok!)

ELSE
	F_Membrete(vinf.dw_1)

	vinf.dw_1.Modify("base.text = '" + ls_cajas + "'")
	vinf.dw_1.Modify("tipoprodu.text = '" + ls_tipoprodu + "'")
	vinf.dw_1.Modify("productor.text = '" + ls_productor + "'")
	vinf.dw_1.Modify("desde.text = '" + istr_mant.argumento[6] + "'")
	vinf.dw_1.Modify("hasta.text = '" + istr_mant.argumento[7] + "'")	
	IF gs_Ambiente <> 'Windows' THEN
		F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
	END IF
END IF

SetPointer(Arrow!)
end event

type pb_salir from w_para_informes`pb_salir within w_info_produccion_packprodzonaembj
string tag = "Salir [Cerrar Ventana Activa]"
integer x = 2194
integer y = 2032
integer taborder = 130
string powertiptext = "Salir [Cerrar Ventana Activa]"
end type

type st_4 from statictext within w_info_produccion_packprodzonaembj
integer x = 238
integer y = 440
integer width = 1847
integer height = 944
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16711680
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type dw_cliente from datawindow within w_info_produccion_packprodzonaembj
integer x = 818
integer y = 452
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
	
	uo_selproductor.Filtra(Integer(istr_mant.argumento[2]),Integer(istr_mant.argumento[3]),Integer(data))

ELSE
	This.SetItem(1, "clie_codigo", ls_null)
	RETURN 1
END IF
end event

event itemerror;RETURN 1
end event

type st_6 from statictext within w_info_produccion_packprodzonaembj
integer x = 338
integer y = 472
integer width = 306
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
string text = "Cliente"
boolean focusrectangle = false
end type

type st_8 from statictext within w_info_produccion_packprodzonaembj
integer x = 338
integer y = 1024
integer width = 306
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

type st_5 from statictext within w_info_produccion_packprodzonaembj
integer x = 238
integer y = 1380
integer width = 1847
integer height = 728
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16711680
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type cbx_peso from checkbox within w_info_produccion_packprodzonaembj
integer x = 379
integer y = 1964
integer width = 631
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

type dw_pesoneto from datawindow within w_info_produccion_packprodzonaembj
integer x = 1275
integer y = 1960
integer width = 695
integer height = 92
integer taborder = 90
boolean bringtotop = true
string dataobject = "dddw_pesoneto"
boolean border = false
boolean livescroll = true
end type

event itemerror;RETURN 1
end event

type tit_peso from statictext within w_info_produccion_packprodzonaembj
integer x = 1106
integer y = 1972
integer width = 160
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
string text = "Peso"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_7 from statictext within w_info_produccion_packprodzonaembj
integer x = 242
integer y = 2108
integer width = 1847
integer height = 144
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16711680
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_9 from statictext within w_info_produccion_packprodzonaembj
integer x = 338
integer y = 644
integer width = 306
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
string text = "Zona"
boolean focusrectangle = false
end type

type cbx_zonas from checkbox within w_info_produccion_packprodzonaembj
integer x = 818
integer y = 544
integer width = 402
integer height = 76
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Todas"
boolean checked = true
end type

event clicked;IF This.Checked THEN
	dw_zonas.Enabled											=	False
	dw_zonas.Object.zona_codigo.BackGround.Color		=	RGB(166,180,210)
	istr_mant.argumento[2]	=	'-1'
	
	IF istr_mant.argumento[3]	=	"-1"	THEN
		uo_selproductor.Filtra(-1,integer(istr_mant.argumento[3]),dw_cliente.Object.clie_Codigo[1])
		uo_selproductor.dw_Seleccion.Enabled		=	True
	END IF	
	
ELSE
	dw_zonas.Enabled											=	True
	dw_zonas.Object.zona_codigo.BackGround.Color		=	RGB(255, 255, 255)
	dw_zonas.SetFocus()
END IF
end event

type dw_zonas from datawindow within w_info_produccion_packprodzonaembj
integer x = 818
integer y = 620
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

event itemchanged;//Integer li_nula
//SetNulL(li_nula)
//iuo_zonas = create uo_zonas
//
//IF iuo_zonas.Existe(Integer(Data),True,Sqlca) THEN
istr_mant.argumento[2]	=	data
//
//	IF istr_mant.argumento[3]	=	"-1"	THEN
//		uo_selproductor.Filtra(Integer(istr_mant.argumento[2]),-1)
//		uo_selproductor.dw_Seleccion.Enabled		=	True
//	ELSE	
//		uo_selproductor.Filtra(Integer(istr_mant.argumento[2]),Integer(istr_mant.argumento[3]))
//		uo_selproductor.dw_Seleccion.Enabled		=	True
//	END IF	
//ELSE
//	This.SetItem(1,"zona_codigo",li_nula)
//END IF
end event

event itemerror;RETURN 1
end event

type st_1 from statictext within w_info_produccion_packprodzonaembj
integer x = 338
integer y = 804
integer width = 475
integer height = 76
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
string text = "Tipo Productor"
boolean focusrectangle = false
end type

type cbx_tipoproductor from checkbox within w_info_produccion_packprodzonaembj
integer x = 818
integer y = 712
integer width = 402
integer height = 76
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
	dw_tipoprodu.Enabled			= 	False
	dw_tipoprodu.Object.tipr_codigo.BackGround.Color		=	RGB(166,180,210)
	istr_mant.argumento[3] =	"-1"

	IF istr_mant.argumento[2]	=	"-1"	THEN
		uo_selproductor.Filtra(-1,Integer(istr_mant.argumento[2]),dw_cliente.Object.clie_Codigo[1])
		uo_selproductor.dw_Seleccion.Enabled		=	True
	END IF	
	
ELSE
	dw_tipoprodu.Enabled			= 	True
	dw_tipoprodu.Object.tipr_codigo.BackGround.Color		=	RGB(255, 255, 255)
	dw_tipoprodu.SetFocus()
END IF

end event

type dw_tipoprodu from datawindow within w_info_produccion_packprodzonaembj
integer x = 818
integer y = 788
integer width = 873
integer height = 92
integer taborder = 30
boolean bringtotop = true
boolean enabled = false
string dataobject = "dddw_tipoproduc"
boolean border = false
boolean livescroll = true
end type

event itemchanged;Integer li_nula
SetNull(li_nula)
iuo_tipoproductor = Create uo_tipoproductor
IF iuo_tipoproductor.Existe(Integer(Data),True,Sqlca) THEN
	istr_mant.argumento[3]=data
	
	IF istr_mant.argumento[2]	=	"-1"	THEN
		uo_selproductor.Filtra(-1,Integer(Data),dw_cliente.Object.clie_Codigo[1])
		uo_selproductor.dw_Seleccion.Enabled		=	True
	ELSE	
		uo_selproductor.Filtra(Integer(istr_mant.argumento[2]),Integer(Data),dw_cliente.Object.clie_Codigo[1])
		uo_selproductor.dw_Seleccion.Enabled		=	True
	END IF	
	
ELSE
	This.SetItem(1,"prod_codigo",li_nula)
END IF

end event

event itemerror;RETURN 1
end event

type st_2 from statictext within w_info_produccion_packprodzonaembj
integer x = 393
integer y = 2156
integer width = 229
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
string text = "Inicio"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type em_fech_ini from editmask within w_info_produccion_packprodzonaembj
integer x = 704
integer y = 2128
integer width = 434
integer height = 92
integer taborder = 100
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
long backcolor = 16777215
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
end type

event modified;istr_mant.argumento[6]=this.text
end event

type st_3 from statictext within w_info_produccion_packprodzonaembj
integer x = 1207
integer y = 2156
integer width = 283
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
string text = "Término"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type em_fech_fin from editmask within w_info_produccion_packprodzonaembj
integer x = 1509
integer y = 2128
integer width = 434
integer height = 92
integer taborder = 110
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
long backcolor = 16777215
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
end type

event modified;istr_mant.argumento[7]=this.text
end event

type st_10 from statictext within w_info_produccion_packprodzonaembj
integer x = 338
integer y = 1480
integer width = 421
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

type st_variedad from statictext within w_info_produccion_packprodzonaembj
integer x = 338
integer y = 1668
integer width = 306
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

type st_calidad from statictext within w_info_produccion_packprodzonaembj
integer x = 338
integer y = 1860
integer width = 256
integer height = 76
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
string text = "Calidad"
boolean focusrectangle = false
end type

type em_calidad from editmask within w_info_produccion_packprodzonaembj
integer x = 818
integer y = 1848
integer width = 261
integer height = 96
integer taborder = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
boolean enabled = false
alignment alignment = center!
textcase textcase = upper!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = stringmask!
string mask = "xxx"
end type

event modified;Integer	li_cliente,	li_especie, li_variedad, li_cantid
String	ls_calibre

IF This.Text <> '' THEN
	li_cliente	=	Integer(istr_mant.argumento[1])  // Cliente
	li_especie	=	Integer(uo_selespecie.Codigo) // Especie
	li_variedad	=	Integer(uo_selvariedad.Codigo) // Variedad
	ls_calibre	=	This.Text
	
	IF NOT iuo_calibre.existe(li_especie,li_variedad,ls_calibre,True,sqlca) THEN
		em_calidad.Text = ''
		This.SetFocus()
		Return 1
	ELSE	
		istr_mant.argumento[12]	 = iuo_calibre.calibre
		em_calidad.Text			 = iuo_calibre.calibre
		Return 1
	END IF	
END IF	


end event

type cbx_calidad from checkbox within w_info_produccion_packprodzonaembj
integer x = 818
integer y = 1764
integer width = 402
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
string text = "Todas"
boolean checked = true
end type

event clicked;IF This.Checked THEN
	cbx_conscalidad.Enabled	=	True
	em_calidad.Text			=	''
	em_calidad.Enabled		=	False
	istr_mant.argumento[12]		=	'-1'
ELSE
	cbx_conscalidad.Enabled	=	False
	cbx_conscalidad.Checked	=	False
	em_calidad.Enabled		=	True
	em_calidad.SetFocus()
END IF


end event

type cbx_conscalidad from checkbox within w_info_produccion_packprodzonaembj
integer x = 1312
integer y = 1764
integer width = 530
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
string text = "Consolidadas"
boolean checked = true
end type

event clicked;IF This.Checked = TRUE THEN
	istr_mant.argumento[12]	=	'-9'
ELSE
	istr_mant.argumento[12]	=	'-1'
END IF

end event

type st_11 from statictext within w_info_produccion_packprodzonaembj
integer x = 338
integer y = 1268
integer width = 306
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
string text = "Packing"
boolean focusrectangle = false
end type

type dw_packing from datawindow within w_info_produccion_packprodzonaembj
integer x = 818
integer y = 1268
integer width = 1038
integer height = 92
integer taborder = 50
boolean bringtotop = true
boolean enabled = false
string title = "none"
string dataobject = "dddw_plantadesp_tipo"
boolean border = false
boolean livescroll = true
end type

event itemchanged;String	 ls_null
SetNull(ls_null)

IF iuo_plantadesp.existepacking(Integer(data),True,Sqlca) THEN
	istr_mant.argumento[8]	=	data
	RETURN 0
ELSE
	This.SetItem(1, "plde_codigo", ls_null)
	RETURN 1
END IF
end event

type cbx_packing from checkbox within w_info_produccion_packprodzonaembj
integer x = 818
integer y = 1192
integer width = 402
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
string text = "Todos"
boolean checked = true
end type

event clicked;Integer li_nula
SetNull(li_nula)
IF This.Checked THEN
	dw_packing.SetItem(1,"plde_codigo",li_nula)
	cbx_packingcons.Enabled								=	True
	dw_packing.Enabled										=	False
	dw_packing.Object.plde_codigo.BackGround.Color	=	RGB(166,180,210)
	istr_mant.argumento[8]									   =	'-1'
ELSE
	cbx_packingcons.Enabled									=	False
	cbx_packingcons.Checked									=	False
	dw_packing.Enabled										=	True
	dw_packing.Object.plde_codigo.BackGround.Color	=	RGB(255, 255, 255)
	dw_packing.SetFocus()
END IF
end event

type cbx_packingcons from checkbox within w_info_produccion_packprodzonaembj
integer x = 1312
integer y = 1192
integer width = 530
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
string text = "Consolidados"
boolean checked = true
end type

event clicked;IF This.Checked = TRUE THEN
	istr_mant.argumento[8]	=	'-9'
ELSE
	istr_mant.argumento[8]	=	'-1'
END IF
	
end event

type uo_selespecie from uo_seleccion_especie within w_info_produccion_packprodzonaembj
event destroy ( )
integer x = 818
integer y = 1392
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

type uo_selvariedad from uo_seleccion_variedad within w_info_produccion_packprodzonaembj
event destroy ( )
integer x = 818
integer y = 1576
integer taborder = 80
boolean bringtotop = true
end type

on uo_selvariedad.destroy
call uo_seleccion_variedad::destroy
end on

type cbx_varirotula from checkbox within w_info_produccion_packprodzonaembj
integer x = 1728
integer y = 1608
integer width = 334
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
string text = "Var. Rot."
end type

type uo_selproductor from uo_seleccion_varios_productores_clientes within w_info_produccion_packprodzonaembj
event destroy ( )
integer x = 818
integer y = 904
integer width = 914
integer height = 276
integer taborder = 150
boolean bringtotop = true
end type

on uo_selproductor.destroy
call uo_seleccion_varios_productores_clientes::destroy
end on

event dragdrop;call super::dragdrop;uo_selproductor.Filtra(-1,-1,dw_cliente.Object.clie_Codigo[1])
end event

