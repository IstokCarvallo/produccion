$PBExportHeader$w_info_pallet_recepcion_interplanta.srw
forward
global type w_info_pallet_recepcion_interplanta from w_para_informes
end type
type st_4 from statictext within w_info_pallet_recepcion_interplanta
end type
type st_1 from statictext within w_info_pallet_recepcion_interplanta
end type
type st_2 from statictext within w_info_pallet_recepcion_interplanta
end type
type em_desde from editmask within w_info_pallet_recepcion_interplanta
end type
type dw_cliente from datawindow within w_info_pallet_recepcion_interplanta
end type
type st_6 from statictext within w_info_pallet_recepcion_interplanta
end type
type dw_planta from datawindow within w_info_pallet_recepcion_interplanta
end type
type st_3 from statictext within w_info_pallet_recepcion_interplanta
end type
type st_7 from statictext within w_info_pallet_recepcion_interplanta
end type
type em_hasta from editmask within w_info_pallet_recepcion_interplanta
end type
type st_9 from statictext within w_info_pallet_recepcion_interplanta
end type
type cbx_packing from checkbox within w_info_pallet_recepcion_interplanta
end type
type dw_packing from datawindow within w_info_pallet_recepcion_interplanta
end type
type gb_3 from groupbox within w_info_pallet_recepcion_interplanta
end type
type st_5 from statictext within w_info_pallet_recepcion_interplanta
end type
type cbx_1 from checkbox within w_info_pallet_recepcion_interplanta
end type
type dw_pesoneto from datawindow within w_info_pallet_recepcion_interplanta
end type
type tit_peso from statictext within w_info_pallet_recepcion_interplanta
end type
type st_8 from statictext within w_info_pallet_recepcion_interplanta
end type
type cbx_2 from checkbox within w_info_pallet_recepcion_interplanta
end type
type cbx_3 from checkbox within w_info_pallet_recepcion_interplanta
end type
type cbx_5 from checkbox within w_info_pallet_recepcion_interplanta
end type
type cbx_6 from checkbox within w_info_pallet_recepcion_interplanta
end type
type cbx_7 from checkbox within w_info_pallet_recepcion_interplanta
end type
type cbx_8 from checkbox within w_info_pallet_recepcion_interplanta
end type
type cbx_9 from checkbox within w_info_pallet_recepcion_interplanta
end type
type st_10 from statictext within w_info_pallet_recepcion_interplanta
end type
type cbx_10 from checkbox within w_info_pallet_recepcion_interplanta
end type
type cbx_11 from checkbox within w_info_pallet_recepcion_interplanta
end type
type cbx_12 from checkbox within w_info_pallet_recepcion_interplanta
end type
type cbx_13 from checkbox within w_info_pallet_recepcion_interplanta
end type
type cbx_15 from checkbox within w_info_pallet_recepcion_interplanta
end type
type uo_selespecie from uo_seleccion_especie within w_info_pallet_recepcion_interplanta
end type
type cbx_varirotula from checkbox within w_info_pallet_recepcion_interplanta
end type
end forward

global type w_info_pallet_recepcion_interplanta from w_para_informes
integer x = 14
integer y = 32
integer width = 2907
integer height = 2496
string title = "Pallets Recepción Interplantas"
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
st_7 st_7
em_hasta em_hasta
st_9 st_9
cbx_packing cbx_packing
dw_packing dw_packing
gb_3 gb_3
st_5 st_5
cbx_1 cbx_1
dw_pesoneto dw_pesoneto
tit_peso tit_peso
st_8 st_8
cbx_2 cbx_2
cbx_3 cbx_3
cbx_5 cbx_5
cbx_6 cbx_6
cbx_7 cbx_7
cbx_8 cbx_8
cbx_9 cbx_9
st_10 st_10
cbx_10 cbx_10
cbx_11 cbx_11
cbx_12 cbx_12
cbx_13 cbx_13
cbx_15 cbx_15
uo_selespecie uo_selespecie
cbx_varirotula cbx_varirotula
end type
global w_info_pallet_recepcion_interplanta w_info_pallet_recepcion_interplanta

type variables
str_busqueda istr_busq
str_mant istr_mant

DataWindowChild	idwc_tipopro, idwc_cliente, idwc_planta, idwc_productor, idwc_packing, idwc_pesoneto

String is_NomPlanta

uo_seleccion_especie		iuo_selespecie
end variables

forward prototypes
public function boolean existepacking (integer li_planta)
public function boolean existeproductor (long ll_productor)
end prototypes

public function boolean existepacking (integer li_planta);Integer  li_codigo

SELECT	plde_codigo
INTO    :li_Codigo
FROM	dbo.plantadesp
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
	istr_mant.argumento[6] = String(ll_Productor)	
	RETURN True
END IF
end function

on w_info_pallet_recepcion_interplanta.create
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
this.st_7=create st_7
this.em_hasta=create em_hasta
this.st_9=create st_9
this.cbx_packing=create cbx_packing
this.dw_packing=create dw_packing
this.gb_3=create gb_3
this.st_5=create st_5
this.cbx_1=create cbx_1
this.dw_pesoneto=create dw_pesoneto
this.tit_peso=create tit_peso
this.st_8=create st_8
this.cbx_2=create cbx_2
this.cbx_3=create cbx_3
this.cbx_5=create cbx_5
this.cbx_6=create cbx_6
this.cbx_7=create cbx_7
this.cbx_8=create cbx_8
this.cbx_9=create cbx_9
this.st_10=create st_10
this.cbx_10=create cbx_10
this.cbx_11=create cbx_11
this.cbx_12=create cbx_12
this.cbx_13=create cbx_13
this.cbx_15=create cbx_15
this.uo_selespecie=create uo_selespecie
this.cbx_varirotula=create cbx_varirotula
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_4
this.Control[iCurrent+2]=this.st_1
this.Control[iCurrent+3]=this.st_2
this.Control[iCurrent+4]=this.em_desde
this.Control[iCurrent+5]=this.dw_cliente
this.Control[iCurrent+6]=this.st_6
this.Control[iCurrent+7]=this.dw_planta
this.Control[iCurrent+8]=this.st_3
this.Control[iCurrent+9]=this.st_7
this.Control[iCurrent+10]=this.em_hasta
this.Control[iCurrent+11]=this.st_9
this.Control[iCurrent+12]=this.cbx_packing
this.Control[iCurrent+13]=this.dw_packing
this.Control[iCurrent+14]=this.gb_3
this.Control[iCurrent+15]=this.st_5
this.Control[iCurrent+16]=this.cbx_1
this.Control[iCurrent+17]=this.dw_pesoneto
this.Control[iCurrent+18]=this.tit_peso
this.Control[iCurrent+19]=this.st_8
this.Control[iCurrent+20]=this.cbx_2
this.Control[iCurrent+21]=this.cbx_3
this.Control[iCurrent+22]=this.cbx_5
this.Control[iCurrent+23]=this.cbx_6
this.Control[iCurrent+24]=this.cbx_7
this.Control[iCurrent+25]=this.cbx_8
this.Control[iCurrent+26]=this.cbx_9
this.Control[iCurrent+27]=this.st_10
this.Control[iCurrent+28]=this.cbx_10
this.Control[iCurrent+29]=this.cbx_11
this.Control[iCurrent+30]=this.cbx_12
this.Control[iCurrent+31]=this.cbx_13
this.Control[iCurrent+32]=this.cbx_15
this.Control[iCurrent+33]=this.uo_selespecie
this.Control[iCurrent+34]=this.cbx_varirotula
end on

on w_info_pallet_recepcion_interplanta.destroy
call super::destroy
destroy(this.st_4)
destroy(this.st_1)
destroy(this.st_2)
destroy(this.em_desde)
destroy(this.dw_cliente)
destroy(this.st_6)
destroy(this.dw_planta)
destroy(this.st_3)
destroy(this.st_7)
destroy(this.em_hasta)
destroy(this.st_9)
destroy(this.cbx_packing)
destroy(this.dw_packing)
destroy(this.gb_3)
destroy(this.st_5)
destroy(this.cbx_1)
destroy(this.dw_pesoneto)
destroy(this.tit_peso)
destroy(this.st_8)
destroy(this.cbx_2)
destroy(this.cbx_3)
destroy(this.cbx_5)
destroy(this.cbx_6)
destroy(this.cbx_7)
destroy(this.cbx_8)
destroy(this.cbx_9)
destroy(this.st_10)
destroy(this.cbx_10)
destroy(this.cbx_11)
destroy(this.cbx_12)
destroy(this.cbx_13)
destroy(this.cbx_15)
destroy(this.uo_selespecie)
destroy(this.cbx_varirotula)
end on

event open;Boolean lb_Cerrar

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
dw_planta.SetItem(1, "plde_codigo", gi_CodPlanta)

dw_packing.GetChild("plde_codigo", idwc_packing)
idwc_packing.SetTransObject(sqlca)
idwc_packing.Retrieve(1)
dw_packing.InsertRow(0)

// iuo_selespecie = Create uo_seleccion_especie
IF IsNull(uo_selespecie.Codigo) THEN lb_Cerrar = True
IF lb_Cerrar THEN
	Close(This)
ELSE
	uo_selespecie.Seleccion(True,True)
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
istr_mant.argumento[2]	= 	String(gi_CodPlanta)		//	planta
istr_mant.argumento[3]	= 	em_desde.Text				//	fecha inicio
istr_mant.argumento[4]	=	em_hasta.Text				//	fecha final
//istr_mant.argumento[5]	=	"-9"							//	especie
istr_mant.argumento[7]  =  "-9"							//	packing
istr_mant.argumento[8]  =  "1"							//	peso
end event

event resize;//
end event

type pb_excel from w_para_informes`pb_excel within w_info_pallet_recepcion_interplanta
end type

type st_computador from w_para_informes`st_computador within w_info_pallet_recepcion_interplanta
end type

type st_usuario from w_para_informes`st_usuario within w_info_pallet_recepcion_interplanta
end type

type st_temporada from w_para_informes`st_temporada within w_info_pallet_recepcion_interplanta
end type

type p_logo from w_para_informes`p_logo within w_info_pallet_recepcion_interplanta
string picturename = "\Desarrollo 17\Imagenes\Logos\RBlanco.jpg"
end type

type st_titulo from w_para_informes`st_titulo within w_info_pallet_recepcion_interplanta
integer width = 2085
string text = "Pallets Recepción Interplanta"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_pallet_recepcion_interplanta
string tag = "Imprimir Reporte"
integer x = 2441
integer y = 1400
integer taborder = 80
end type

event pb_acepta::clicked;SetPointer(Arrow!)

Integer	fila, li_cliente, li_planta, li_varirotula, &
         li_consopal,li_consocar,li_consofrec,li_consonrec,li_consogdes,li_consofemb, &
			li_consoemb,li_consovar,li_consoeti,li_consoprd,li_consocal
Date		ld_desde, ld_hasta
String	texto_desde, texto_hasta, texto_fecha, ls_nroguia, ls_cajas

istr_info.titulo	= 'PALLETS RECEPCIONES INTERPLANTAS'

OpenWithParm(vinf, istr_info)
vinf.dw_1.DataObject = "dw_info_pallet_recepcion_interplanta"

/*
Especies
*/
IF IsNull(uo_selespecie.Codigo)THEN
	MessageBox("Atención","Debe Seleccionar una Especie Previamente",Exclamation!)
	uo_selespecie.dw_Seleccion.SetFocus()
	RETURN
END IF

li_cliente 		=	Integer(istr_mant.argumento[1])
li_planta			=	Integer(istr_mant.argumento[2])
ld_desde			=	Date(istr_mant.argumento[3])
ld_hasta			=	Date(istr_mant.argumento[4])
texto_desde		=  f_fecha_texto(String(ld_desde), 1)
texto_hasta		=	f_fecha_texto(String(ld_hasta), 1)
texto_fecha		=	"Desde el  " + texto_desde + "  Hasta el  " + texto_hasta
ls_nroguia		=	"Todas"

IF cbx_1.Checked=False THEN
	ls_cajas = "Reales"
	istr_mant.argumento[8]	=	"1"
ELSE
	istr_mant.argumento[8]	=	String(dw_pesoneto.Object.enva_pesone[1])
	ls_cajas = "Base " + istr_mant.argumento[8] 
END IF

IF cbx_2.Checked THEN
	li_consopal = 1
ELSE
	li_consopal = 0
END IF
IF cbx_5.Checked THEN
	li_consocar = 1
ELSE
	li_consocar = 0
END IF
IF cbx_6.Checked THEN
	li_consofrec = 1
ELSE
	li_consofrec = 0
END IF
IF cbx_7.Checked THEN
	li_consonrec = 1
ELSE
	li_consonrec = 0
END IF
IF cbx_8.Checked THEN
	li_consogdes = 1
ELSE
	li_consogdes = 0
END IF
IF cbx_9.Checked THEN
	li_consofemb = 1
ELSE
	li_consofemb = 0
END IF
IF cbx_10.Checked THEN
	li_consoemb = 1
ELSE
	li_consoemb = 0
END IF
IF cbx_11.Checked THEN
	li_consovar = 1
ELSE
	li_consovar = 0
END IF
IF cbx_12.Checked THEN
	li_consoeti = 1
ELSE
	li_consoeti = 0
END IF
IF cbx_13.Checked THEN
	li_consoprd = 1
ELSE
	li_consoprd = 0
END IF
IF cbx_15.Checked THEN
	li_consocal = 1
ELSE
	li_consocal = 0
END IF

IF cbx_varirotula.Checked THEN
	li_varirotula = 1
ELSE
	li_varirotula = 0
END IF

vinf.dw_1.SetTransObject(sqlca)

fila	=	vinf.dw_1.Retrieve(li_cliente, li_planta, ld_desde, ld_hasta, uo_selespecie.Codigo, &
									 Integer(istr_mant.argumento[7]), li_consopal,li_consocar, &
									 li_consofrec,li_consonrec,li_consogdes,li_consofemb, li_consoemb, &
									 li_consovar,li_consoeti,li_consoprd,li_consocal,li_varirotula)
IF fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
			   	"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)

ELSEIF fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", &
	             StopSign!, Ok!)

ELSE
	   F_Membrete(vinf.dw_1)
		vinf.dw_1.Modify("fechas.text = '" + texto_fecha + "'")
	IF gs_Ambiente <> 'Windows' THEN
		F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
	END IF
END IF
SetPointer(Arrow!)				
end event

type pb_salir from w_para_informes`pb_salir within w_info_pallet_recepcion_interplanta
string tag = "Salir [Cerrar Ventana Activa]"
integer x = 2441
integer y = 1688
integer taborder = 90
string powertiptext = "Salir [Cerrar Ventana Activa]"
end type

type st_4 from statictext within w_info_pallet_recepcion_interplanta
integer x = 251
integer y = 440
integer width = 2085
integer height = 508
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

type st_1 from statictext within w_info_pallet_recepcion_interplanta
integer x = 315
integer y = 608
integer width = 462
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
string text = "Planta"
boolean focusrectangle = false
end type

type st_2 from statictext within w_info_pallet_recepcion_interplanta
integer x = 320
integer y = 1280
integer width = 425
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
string text = "Fecha Inicio"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type em_desde from editmask within w_info_pallet_recepcion_interplanta
integer x = 791
integer y = 1264
integer width = 393
integer height = 96
integer taborder = 60
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

event modified;istr_mant.argumento[3]	=	This.Text
end event

type dw_cliente from datawindow within w_info_pallet_recepcion_interplanta
integer x = 786
integer y = 488
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
ELSE
	This.SetItem(1, "clie_codigo", ls_null)
	RETURN 1
END IF
end event

event itemerror;RETURN 1
end event

type st_6 from statictext within w_info_pallet_recepcion_interplanta
integer x = 315
integer y = 496
integer width = 233
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

type dw_planta from datawindow within w_info_pallet_recepcion_interplanta
integer x = 786
integer y = 612
integer width = 969
integer height = 92
integer taborder = 20
boolean bringtotop = true
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

type st_3 from statictext within w_info_pallet_recepcion_interplanta
integer x = 320
integer y = 1064
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

type st_7 from statictext within w_info_pallet_recepcion_interplanta
integer x = 1358
integer y = 1280
integer width = 297
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
boolean focusrectangle = false
end type

type em_hasta from editmask within w_info_pallet_recepcion_interplanta
integer x = 1673
integer y = 1264
integer width = 393
integer height = 96
integer taborder = 70
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

event modified;istr_mant.argumento[4]	=	This.Text
end event

type st_9 from statictext within w_info_pallet_recepcion_interplanta
integer x = 315
integer y = 828
integer width = 416
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
string text = "Planta Origen"
alignment alignment = right!
boolean focusrectangle = false
end type

type cbx_packing from checkbox within w_info_pallet_recepcion_interplanta
integer x = 786
integer y = 748
integer width = 402
integer height = 80
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
boolean enabled = false
string text = "Todas"
boolean checked = true
end type

event clicked;IF This.checked = True THEN
	dw_packing.Enabled = False
	istr_mant.argumento[7]	=	'0'
ELSE
	dw_packing.Enabled=True	
END IF	
end event

type dw_packing from datawindow within w_info_pallet_recepcion_interplanta
integer x = 786
integer y = 828
integer width = 965
integer height = 100
integer taborder = 40
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

type gb_3 from groupbox within w_info_pallet_recepcion_interplanta
boolean visible = false
integer x = 1001
integer y = 2356
integer width = 1614
integer height = 160
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
end type

type st_5 from statictext within w_info_pallet_recepcion_interplanta
integer x = 251
integer y = 952
integer width = 2085
integer height = 460
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

type cbx_1 from checkbox within w_info_pallet_recepcion_interplanta
boolean visible = false
integer x = 1435
integer y = 2212
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

type dw_pesoneto from datawindow within w_info_pallet_recepcion_interplanta
boolean visible = false
integer x = 1577
integer y = 2300
integer width = 544
integer height = 84
boolean bringtotop = true
string dataobject = "dddw_pesoneto"
boolean border = false
boolean livescroll = true
end type

event itemerror;RETURN 1
end event

type tit_peso from statictext within w_info_pallet_recepcion_interplanta
boolean visible = false
integer x = 1385
integer y = 2316
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

type st_8 from statictext within w_info_pallet_recepcion_interplanta
integer x = 251
integer y = 1412
integer width = 1042
integer height = 564
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

type cbx_2 from checkbox within w_info_pallet_recepcion_interplanta
integer x = 329
integer y = 1444
integer width = 690
integer height = 80
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
string text = "Consolida Pallets"
boolean checked = true
end type

type cbx_3 from checkbox within w_info_pallet_recepcion_interplanta
integer x = 1271
integer y = 748
integer width = 471
integer height = 80
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

event clicked;IF This.checked = True THEN
	cbx_packing.Checked		=	True
	cbx_packing.Enabled		=	False
	dw_packing.Enabled 		= 	False
	istr_mant.argumento[7]	=	'-9'
ELSE
	cbx_packing.Checked		=	True
	cbx_packing.Enabled		=	True
	istr_mant.argumento[7]	=	'0'
END IF	
end event

type cbx_5 from checkbox within w_info_pallet_recepcion_interplanta
integer x = 329
integer y = 1516
integer width = 850
integer height = 80
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
string text = "Consolida Características  "
boolean checked = true
end type

type cbx_6 from checkbox within w_info_pallet_recepcion_interplanta
integer x = 329
integer y = 1592
integer width = 910
integer height = 80
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
string text = "Consolida Fecha Recepción "
boolean checked = true
end type

type cbx_7 from checkbox within w_info_pallet_recepcion_interplanta
integer x = 329
integer y = 1672
integer width = 960
integer height = 80
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
string text = "Consolida Número Recepción "
boolean checked = true
end type

type cbx_8 from checkbox within w_info_pallet_recepcion_interplanta
integer x = 329
integer y = 1744
integer width = 864
integer height = 80
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
string text = "Consolida Guía Despacho  "
boolean checked = true
end type

type cbx_9 from checkbox within w_info_pallet_recepcion_interplanta
integer x = 329
integer y = 1820
integer width = 910
integer height = 80
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
string text = "Consolida Fecha Embalaje"
boolean checked = true
end type

type st_10 from statictext within w_info_pallet_recepcion_interplanta
integer x = 1294
integer y = 1412
integer width = 1042
integer height = 564
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

type cbx_10 from checkbox within w_info_pallet_recepcion_interplanta
integer x = 1344
integer y = 1444
integer width = 690
integer height = 80
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
string text = "Consolida Embalaje"
boolean checked = true
end type

type cbx_11 from checkbox within w_info_pallet_recepcion_interplanta
integer x = 1344
integer y = 1520
integer width = 850
integer height = 76
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
string text = "Consolida Variedad"
boolean checked = true
end type

type cbx_12 from checkbox within w_info_pallet_recepcion_interplanta
integer x = 1344
integer y = 1592
integer width = 910
integer height = 80
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
string text = "Consolida Etiqueta "
boolean checked = true
end type

type cbx_13 from checkbox within w_info_pallet_recepcion_interplanta
integer x = 1344
integer y = 1672
integer width = 960
integer height = 80
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
string text = "Consolida Código Productor "
boolean checked = true
end type

type cbx_15 from checkbox within w_info_pallet_recepcion_interplanta
integer x = 1344
integer y = 1744
integer width = 910
integer height = 80
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
string text = "Consolida Calibre"
boolean checked = true
end type

type uo_selespecie from uo_seleccion_especie within w_info_pallet_recepcion_interplanta
integer x = 791
integer y = 980
integer height = 180
integer taborder = 240
boolean bringtotop = true
end type

on uo_selespecie.destroy
call uo_seleccion_especie::destroy
end on

type cbx_varirotula from checkbox within w_info_pallet_recepcion_interplanta
integer x = 791
integer y = 1164
integer width = 626
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
string text = "Variedad Rotulada"
end type

