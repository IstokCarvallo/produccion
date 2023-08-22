$PBExportHeader$w_info_consultarecepciondespacking.srw
forward
global type w_info_consultarecepciondespacking from w_para_informes
end type
type st_4 from statictext within w_info_consultarecepciondespacking
end type
type st_1 from statictext within w_info_consultarecepciondespacking
end type
type st_2 from statictext within w_info_consultarecepciondespacking
end type
type em_desde from editmask within w_info_consultarecepciondespacking
end type
type dw_cliente from datawindow within w_info_consultarecepciondespacking
end type
type st_6 from statictext within w_info_consultarecepciondespacking
end type
type dw_planta from datawindow within w_info_consultarecepciondespacking
end type
type st_7 from statictext within w_info_consultarecepciondespacking
end type
type em_hasta from editmask within w_info_consultarecepciondespacking
end type
type st_8 from statictext within w_info_consultarecepciondespacking
end type
type dw_productor from datawindow within w_info_consultarecepciondespacking
end type
type cbx_productor from checkbox within w_info_consultarecepciondespacking
end type
type st_9 from statictext within w_info_consultarecepciondespacking
end type
type cbx_packing from checkbox within w_info_consultarecepciondespacking
end type
type dw_packing from datawindow within w_info_consultarecepciondespacking
end type
type st_5 from statictext within w_info_consultarecepciondespacking
end type
type st_11 from statictext within w_info_consultarecepciondespacking
end type
type st_13 from statictext within w_info_consultarecepciondespacking
end type
type st_3 from statictext within w_info_consultarecepciondespacking
end type
type cbx_guias from checkbox within w_info_consultarecepciondespacking
end type
type em_guia from editmask within w_info_consultarecepciondespacking
end type
type cbx_porcaja from checkbox within w_info_consultarecepciondespacking
end type
type st_10 from statictext within w_info_consultarecepciondespacking
end type
type uo_selvariedad from uo_seleccion_variedad within w_info_consultarecepciondespacking
end type
type st_variedad from statictext within w_info_consultarecepciondespacking
end type
type cbx_varirotula from checkbox within w_info_consultarecepciondespacking
end type
type st_calidad from statictext within w_info_consultarecepciondespacking
end type
type em_calidad from editmask within w_info_consultarecepciondespacking
end type
type cbx_calidad from checkbox within w_info_consultarecepciondespacking
end type
type st_embalaje from statictext within w_info_consultarecepciondespacking
end type
type em_embalaje from editmask within w_info_consultarecepciondespacking
end type
type cbx_embalaje from checkbox within w_info_consultarecepciondespacking
end type
type st_12 from statictext within w_info_consultarecepciondespacking
end type
type st_especie from statictext within w_info_consultarecepciondespacking
end type
type uo_selespecie from uo_seleccion_especie within w_info_consultarecepciondespacking
end type
type st_14 from statictext within w_info_consultarecepciondespacking
end type
type cbx_orden from checkbox within w_info_consultarecepciondespacking
end type
type st_15 from statictext within w_info_consultarecepciondespacking
end type
type em_orden from editmask within w_info_consultarecepciondespacking
end type
type st_16 from statictext within w_info_consultarecepciondespacking
end type
type cbx_categorias from checkbox within w_info_consultarecepciondespacking
end type
type dw_categorias from datawindow within w_info_consultarecepciondespacking
end type
end forward

global type w_info_consultarecepciondespacking from w_para_informes
integer x = 14
integer y = 32
integer width = 2661
integer height = 2544
string title = "Consulta Pallet Recepcionados desde Origen"
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
st_7 st_7
em_hasta em_hasta
st_8 st_8
dw_productor dw_productor
cbx_productor cbx_productor
st_9 st_9
cbx_packing cbx_packing
dw_packing dw_packing
st_5 st_5
st_11 st_11
st_13 st_13
st_3 st_3
cbx_guias cbx_guias
em_guia em_guia
cbx_porcaja cbx_porcaja
st_10 st_10
uo_selvariedad uo_selvariedad
st_variedad st_variedad
cbx_varirotula cbx_varirotula
st_calidad st_calidad
em_calidad em_calidad
cbx_calidad cbx_calidad
st_embalaje st_embalaje
em_embalaje em_embalaje
cbx_embalaje cbx_embalaje
st_12 st_12
st_especie st_especie
uo_selespecie uo_selespecie
st_14 st_14
cbx_orden cbx_orden
st_15 st_15
em_orden em_orden
st_16 st_16
cbx_categorias cbx_categorias
dw_categorias dw_categorias
end type
global w_info_consultarecepciondespacking w_info_consultarecepciondespacking

type variables
str_busqueda istr_busq
str_mant istr_mant

DataWindowChild	idwc_tipopro, idwc_cliente, idwc_planta, idwc_productor, idwc_packing,&
						idwc_pesoneto, idwc_fruta, idwc_categorias

uo_seleccion_especie		iuo_selespecie
uo_seleccion_variedad	iuo_selvariedad
uo_spro_ordenproceso	iuo_spro_ordenproceso
uo_calibre								iuo_calibre

String 	is_NomPlanta
Integer	ii_variable

end variables

forward prototypes
public function boolean existeespecie (integer especie)
public function boolean existeproductor (long ll_productor)
public function boolean existepacking (integer li_planta)
public function string buscdescfruta (integer codigo)
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

public function string buscdescfruta (integer codigo);String	ls_descri


  SELECT frre_descri  
    INTO :ls_descri  
    FROM dba.recfruprocee as re, dba.frutarecibida as fr 
   WHERE re.frre_codigo = fr.frre_codigo and  
         re.frre_codigo = :codigo ;

		
RETURN ls_descri

end function

on w_info_consultarecepciondespacking.create
int iCurrent
call super::create
this.st_4=create st_4
this.st_1=create st_1
this.st_2=create st_2
this.em_desde=create em_desde
this.dw_cliente=create dw_cliente
this.st_6=create st_6
this.dw_planta=create dw_planta
this.st_7=create st_7
this.em_hasta=create em_hasta
this.st_8=create st_8
this.dw_productor=create dw_productor
this.cbx_productor=create cbx_productor
this.st_9=create st_9
this.cbx_packing=create cbx_packing
this.dw_packing=create dw_packing
this.st_5=create st_5
this.st_11=create st_11
this.st_13=create st_13
this.st_3=create st_3
this.cbx_guias=create cbx_guias
this.em_guia=create em_guia
this.cbx_porcaja=create cbx_porcaja
this.st_10=create st_10
this.uo_selvariedad=create uo_selvariedad
this.st_variedad=create st_variedad
this.cbx_varirotula=create cbx_varirotula
this.st_calidad=create st_calidad
this.em_calidad=create em_calidad
this.cbx_calidad=create cbx_calidad
this.st_embalaje=create st_embalaje
this.em_embalaje=create em_embalaje
this.cbx_embalaje=create cbx_embalaje
this.st_12=create st_12
this.st_especie=create st_especie
this.uo_selespecie=create uo_selespecie
this.st_14=create st_14
this.cbx_orden=create cbx_orden
this.st_15=create st_15
this.em_orden=create em_orden
this.st_16=create st_16
this.cbx_categorias=create cbx_categorias
this.dw_categorias=create dw_categorias
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_4
this.Control[iCurrent+2]=this.st_1
this.Control[iCurrent+3]=this.st_2
this.Control[iCurrent+4]=this.em_desde
this.Control[iCurrent+5]=this.dw_cliente
this.Control[iCurrent+6]=this.st_6
this.Control[iCurrent+7]=this.dw_planta
this.Control[iCurrent+8]=this.st_7
this.Control[iCurrent+9]=this.em_hasta
this.Control[iCurrent+10]=this.st_8
this.Control[iCurrent+11]=this.dw_productor
this.Control[iCurrent+12]=this.cbx_productor
this.Control[iCurrent+13]=this.st_9
this.Control[iCurrent+14]=this.cbx_packing
this.Control[iCurrent+15]=this.dw_packing
this.Control[iCurrent+16]=this.st_5
this.Control[iCurrent+17]=this.st_11
this.Control[iCurrent+18]=this.st_13
this.Control[iCurrent+19]=this.st_3
this.Control[iCurrent+20]=this.cbx_guias
this.Control[iCurrent+21]=this.em_guia
this.Control[iCurrent+22]=this.cbx_porcaja
this.Control[iCurrent+23]=this.st_10
this.Control[iCurrent+24]=this.uo_selvariedad
this.Control[iCurrent+25]=this.st_variedad
this.Control[iCurrent+26]=this.cbx_varirotula
this.Control[iCurrent+27]=this.st_calidad
this.Control[iCurrent+28]=this.em_calidad
this.Control[iCurrent+29]=this.cbx_calidad
this.Control[iCurrent+30]=this.st_embalaje
this.Control[iCurrent+31]=this.em_embalaje
this.Control[iCurrent+32]=this.cbx_embalaje
this.Control[iCurrent+33]=this.st_12
this.Control[iCurrent+34]=this.st_especie
this.Control[iCurrent+35]=this.uo_selespecie
this.Control[iCurrent+36]=this.st_14
this.Control[iCurrent+37]=this.cbx_orden
this.Control[iCurrent+38]=this.st_15
this.Control[iCurrent+39]=this.em_orden
this.Control[iCurrent+40]=this.st_16
this.Control[iCurrent+41]=this.cbx_categorias
this.Control[iCurrent+42]=this.dw_categorias
end on

on w_info_consultarecepciondespacking.destroy
call super::destroy
destroy(this.st_4)
destroy(this.st_1)
destroy(this.st_2)
destroy(this.em_desde)
destroy(this.dw_cliente)
destroy(this.st_6)
destroy(this.dw_planta)
destroy(this.st_7)
destroy(this.em_hasta)
destroy(this.st_8)
destroy(this.dw_productor)
destroy(this.cbx_productor)
destroy(this.st_9)
destroy(this.cbx_packing)
destroy(this.dw_packing)
destroy(this.st_5)
destroy(this.st_11)
destroy(this.st_13)
destroy(this.st_3)
destroy(this.cbx_guias)
destroy(this.em_guia)
destroy(this.cbx_porcaja)
destroy(this.st_10)
destroy(this.uo_selvariedad)
destroy(this.st_variedad)
destroy(this.cbx_varirotula)
destroy(this.st_calidad)
destroy(this.em_calidad)
destroy(this.cbx_calidad)
destroy(this.st_embalaje)
destroy(this.em_embalaje)
destroy(this.cbx_embalaje)
destroy(this.st_12)
destroy(this.st_especie)
destroy(this.uo_selespecie)
destroy(this.st_14)
destroy(this.cbx_orden)
destroy(this.st_15)
destroy(this.em_orden)
destroy(this.st_16)
destroy(this.cbx_categorias)
destroy(this.dw_categorias)
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
dw_planta.SetItem(1, "plde_codigo", gi_codplanta)

dw_productor.GetChild("prod_codigo", idwc_productor)
idwc_productor.SetTransObject(sqlca)
idwc_productor.Retrieve(gi_CodExport)
dw_productor.InsertRow(0)

dw_packing.GetChild("plde_codigo", idwc_packing)
idwc_packing.SetTransObject(sqlca)
idwc_packing.Retrieve(2)
dw_packing.InsertRow(0)
dw_packing.SetItem(1, "plde_codigo", gi_packing)

// iuo_selespecie = Create uo_seleccion_especie
IF IsNull(uo_selespecie.Codigo) THEN lb_Cerrar = True
IF lb_Cerrar THEN
	Close(This)
ELSE
	uo_selespecie.Seleccion(True,False)
END IF
// iuo_selvariedad = Create uo_seleccion_variedad
IF IsNull(uo_selvariedad.Codigo) THEN lb_Cerrar = True
IF lb_Cerrar THEN
	Close(This)
ELSE
	uo_selvariedad.Seleccion(True,False)
	uo_selvariedad.Enabled		=	False
END IF

iuo_calibre   				=	Create uo_calibre

dw_categorias.GetChild("cate_codigo", idwc_categorias)
idwc_categorias.SetTransObject(SQLCA)
idwc_categorias.Retrieve(0)
idwc_categorias.SetFilter("cate_embala = 1")
idwc_categorias.Filter()
dw_categorias.InsertRow(0)

iuo_spro_ordenproceso	= Create uo_spro_ordenproceso
em_desde.Text				=	String(RelativeDate(Today(), -365))
em_hasta.Text				=	String(Today())
istr_mant.argumento[1]	= 	String(gi_CodExport) 	// cliente
istr_mant.argumento[2]	= 	String(gi_codplanta)		//	planta
istr_mant.argumento[3]	= 	em_desde.Text				//	fecha inicio
istr_mant.argumento[4]	=	em_hasta.Text				//	fecha final
istr_mant.argumento[6]  =  "0"							//	productor
istr_mant.argumento[7]  =  String(gi_packing)		//	packing

end event

event resize;//

end event

type st_computador from w_para_informes`st_computador within w_info_consultarecepciondespacking
end type

type st_usuario from w_para_informes`st_usuario within w_info_consultarecepciondespacking
end type

type st_temporada from w_para_informes`st_temporada within w_info_consultarecepciondespacking
end type

type p_logo from w_para_informes`p_logo within w_info_consultarecepciondespacking
end type

type st_titulo from w_para_informes`st_titulo within w_info_consultarecepciondespacking
integer width = 1920
integer height = 84
string text = "Consulta Pallet Recepcionados desde Origen"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_consultarecepciondespacking
string tag = "Imprimir Reporte"
integer x = 2281
integer y = 1808
integer taborder = 190
string powertiptext = "Imprimir Reporte"
end type

event pb_acepta::clicked;SetPointer(Arrow!)

Integer	fila, li_cliente, li_planta, li_tipallet, li_caja, li_varirotula, li_cate
Date		ld_desde, ld_hasta
String	texto_desde, texto_hasta, texto_fecha, ls_nroguia, ls_cajas, ls_descri,ls_tipallet,&
         ls_calidad, ls_embalaje
Long		ll_guia, ll_Orden
istr_info.titulo	= 'CONSULTA RECEPCIONES DESDE PACKING ORIGEN'

OpenWithParm(vinf, istr_info)
vinf.dw_1.DataObject = "dw_info_consultarecepciondespacking"

li_cliente 		=	Integer(istr_mant.argumento[1])
li_planta		=	Integer(istr_mant.argumento[2])
ld_desde			=	Date(istr_mant.argumento[3])
ld_hasta			=	Date(istr_mant.argumento[4])
texto_desde		=  f_fecha_texto(String(ld_desde), 1)
texto_hasta		=	f_fecha_texto(String(ld_hasta), 1)
texto_fecha		=	"Desde El :  " + texto_desde + "   Hasta El :  " + texto_hasta

IF cbx_guias.Checked THEN
	ll_guia = 0
ELSE
	ll_guia = Long(em_guia.Text)
END IF

IF cbx_porcaja.Checked THEN
	li_caja = 1
ELSE
	li_caja = 0
END IF

IF cbx_calidad.Checked THEN
	ls_calidad = '-1'
ELSE
	ls_calidad = em_calidad.Text
END IF

IF cbx_embalaje.Checked THEN
	ls_embalaje = '-1'
ELSE
	ls_embalaje = em_embalaje.Text
END IF

IF cbx_orden.Checked THEN
	ll_Orden = -1
ELSE
	ll_Orden = Long(em_orden.Text)
END IF

IF cbx_categorias.Checked THEN
	li_cate = -1
ELSE
	li_cate = dw_categorias.object.cate_codigo[1]
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

vinf.dw_1.SetTransObject(sqlca)

fila	=	vinf.dw_1.Retrieve(li_cliente, li_planta, ld_desde, ld_hasta, ll_guia,&
									Long(istr_mant.argumento[6]),Integer(istr_mant.argumento[7]),li_caja,&
									uo_selespecie.Codigo,uo_selvariedad.Codigo,li_varirotula,ls_calidad,&
									ls_embalaje,ll_Orden, li_cate)

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

type pb_salir from w_para_informes`pb_salir within w_info_consultarecepciondespacking
string tag = "Salir [Cerrar Ventana Activa]"
integer x = 2281
integer y = 2100
integer taborder = 200
string powertiptext = "Salir [Cerrar Ventana Activa]"
end type

type st_4 from statictext within w_info_consultarecepciondespacking
integer x = 251
integer y = 440
integer width = 1920
integer height = 428
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

type st_1 from statictext within w_info_consultarecepciondespacking
integer x = 315
integer y = 572
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

type st_2 from statictext within w_info_consultarecepciondespacking
integer x = 315
integer y = 2092
integer width = 517
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 553648127
string text = "Recepción Inicio "
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type em_desde from editmask within w_info_consultarecepciondespacking
integer x = 869
integer y = 2092
integer width = 393
integer height = 96
integer taborder = 160
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

type dw_cliente from datawindow within w_info_consultarecepciondespacking
integer x = 686
integer y = 468
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

type st_6 from statictext within w_info_consultarecepciondespacking
integer x = 320
integer y = 468
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

type dw_planta from datawindow within w_info_consultarecepciondespacking
integer x = 686
integer y = 572
integer width = 969
integer height = 92
integer taborder = 20
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

IF ExistePlanta(li_cliente,Integer(data), ls_Columna[]) THEN
	istr_mant.argumento[2]	=	data
ELSE
	This.SetItem(1, "plde_codigo", ls_null)
	RETURN 1
END IF
end event

event itemerror;RETURN 1
end event

type st_7 from statictext within w_info_consultarecepciondespacking
integer x = 1285
integer y = 2092
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

type em_hasta from editmask within w_info_consultarecepciondespacking
integer x = 1573
integer y = 2104
integer width = 393
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
long backcolor = 16777215
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
end type

event modified;istr_mant.argumento[4]	=	This.Text
end event

type st_8 from statictext within w_info_consultarecepciondespacking
integer x = 315
integer y = 968
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

type dw_productor from datawindow within w_info_consultarecepciondespacking
integer x = 686
integer y = 956
integer width = 974
integer height = 92
integer taborder = 70
boolean bringtotop = true
boolean enabled = false
string title = "none"
string dataobject = "dddw_productores"
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

type cbx_productor from checkbox within w_info_consultarecepciondespacking
integer x = 686
integer y = 876
integer width = 402
integer height = 80
integer taborder = 50
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

event clicked;IF cbx_productor.Checked = TRUE THEN
	dw_productor.Enabled  = False
	dw_productor.Reset()
	dw_productor.insertrow(0)
	istr_mant.argumento[6]	=	'0'
ELSE
	dw_productor.Enabled  = True
	dw_productor.SetFocus()
	dw_productor.Reset()
	dw_productor.InsertRow(0)
END IF
	
end event

type st_9 from statictext within w_info_consultarecepciondespacking
integer x = 315
integer y = 752
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

type cbx_packing from checkbox within w_info_consultarecepciondespacking
integer x = 686
integer y = 672
integer width = 402
integer height = 80
integer taborder = 30
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
end type

event clicked;IF This.checked = True THEN
	dw_packing.Enabled = False
	istr_mant.argumento[7]	=	'0'
ELSE
	dw_packing.Enabled=True	
END IF	
end event

type dw_packing from datawindow within w_info_consultarecepciondespacking
integer x = 686
integer y = 752
integer width = 965
integer height = 100
integer taborder = 40
boolean bringtotop = true
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

type st_5 from statictext within w_info_consultarecepciondespacking
integer x = 251
integer y = 868
integer width = 1920
integer height = 204
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

type st_11 from statictext within w_info_consultarecepciondespacking
integer x = 251
integer y = 2080
integer width = 1920
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

type st_13 from statictext within w_info_consultarecepciondespacking
integer x = 251
integer y = 1808
integer width = 1920
integer height = 136
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_3 from statictext within w_info_consultarecepciondespacking
integer x = 315
integer y = 1824
integer width = 553
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 553648127
string text = "Guía Relaci."
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type cbx_guias from checkbox within w_info_consultarecepciondespacking
integer x = 1166
integer y = 1828
integer width = 402
integer height = 80
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
string text = "Todas"
boolean checked = true
end type

event clicked;IF cbx_guias.Checked THEN
	em_guia.Enabled  	= False
	em_guia.Text  		= ''
ELSE
	em_guia.Enabled  	= True
	em_guia.SetFocus()
END IF
	
end event

type em_guia from editmask within w_info_consultarecepciondespacking
integer x = 686
integer y = 1824
integer width = 389
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
long backcolor = 134217752
boolean enabled = false
alignment alignment = center!
borderstyle borderstyle = stylelowered!
string mask = "########"
string minmax = "8~~"
end type

type cbx_porcaja from checkbox within w_info_consultarecepciondespacking
integer x = 974
integer y = 2224
integer width = 402
integer height = 80
integer taborder = 180
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
string text = "Por Caja"
end type

type st_10 from statictext within w_info_consultarecepciondespacking
integer x = 251
integer y = 2216
integer width = 1920
integer height = 100
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type uo_selvariedad from uo_seleccion_variedad within w_info_consultarecepciondespacking
integer x = 686
integer y = 1260
integer taborder = 80
boolean bringtotop = true
end type

on uo_selvariedad.destroy
call uo_seleccion_variedad::destroy
end on

type st_variedad from statictext within w_info_consultarecepciondespacking
integer x = 315
integer y = 1360
integer width = 279
integer height = 60
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

type cbx_varirotula from checkbox within w_info_consultarecepciondespacking
integer x = 1682
integer y = 1360
integer width = 306
integer height = 80
integer taborder = 90
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
string text = "Var. Rot."
boolean checked = true
end type

type st_calidad from statictext within w_info_consultarecepciondespacking
integer x = 315
integer y = 1460
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
long backcolor = 553648127
boolean enabled = false
string text = "Calidad"
boolean focusrectangle = false
end type

type em_calidad from editmask within w_info_consultarecepciondespacking
integer x = 686
integer y = 1456
integer width = 297
integer height = 84
integer taborder = 100
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

IF This.Text <> '' THEN
	li_especie	=	uo_selespecie.Codigo // Especie
	li_variedad	=	uo_selvariedad.Codigo // Variedad
	ls_calibre	=	This.Text
	
	IF NOT iuo_calibre.existe(li_especie,li_variedad,ls_calibre,True,sqlca) THEN
		em_calidad.Text = ''
		This.SetFocus()
		Return 1
	ELSE	
		em_calidad.Text			=	ls_calibre
		Return 1
	END IF	
END IF	

end event

type cbx_calidad from checkbox within w_info_consultarecepciondespacking
integer x = 1166
integer y = 1456
integer width = 297
integer height = 80
integer taborder = 110
boolean bringtotop = true
integer textsize = -8
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
	em_calidad.Text			=	''
	em_calidad.Enabled		=	False
ELSE
	em_calidad.Enabled		=	True
	em_calidad.SetFocus()
END IF

end event

type st_embalaje from statictext within w_info_consultarecepciondespacking
integer x = 315
integer y = 1568
integer width = 288
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
string text = "Embalaje"
boolean focusrectangle = false
end type

type em_embalaje from editmask within w_info_consultarecepciondespacking
integer x = 686
integer y = 1564
integer width = 297
integer height = 84
integer taborder = 120
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
string mask = "xxxxxxxxxx"
end type

event modified;Integer  li_cliente
String	ls_embalaje, ls_Nombre

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
END IF
end event

type cbx_embalaje from checkbox within w_info_consultarecepciondespacking
integer x = 1166
integer y = 1564
integer width = 283
integer height = 80
integer taborder = 130
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
	em_embalaje.Enabled			=	False
	em_embalaje.Text				=	''
	
ELSE
	em_embalaje.Enabled			=	True
	
END IF
end event

type st_12 from statictext within w_info_consultarecepciondespacking
integer x = 251
integer y = 1072
integer width = 1920
integer height = 736
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

type st_especie from statictext within w_info_consultarecepciondespacking
integer x = 315
integer y = 1188
integer width = 238
integer height = 60
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 553648127
string text = "Especie"
boolean focusrectangle = false
end type

type uo_selespecie from uo_seleccion_especie within w_info_consultarecepciondespacking
event destroy ( )
integer x = 686
integer y = 1084
integer height = 180
integer taborder = 60
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
		uo_selvariedad.Enabled						=	False
		
	CASE ELSE
		uo_selvariedad.Enabled		=	True
		uo_selvariedad.Filtra(This.Codigo)

END CHOOSE
end event

type st_14 from statictext within w_info_consultarecepciondespacking
integer x = 315
integer y = 1972
integer width = 329
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
string text = "Nro. Orden"
boolean focusrectangle = false
end type

type cbx_orden from checkbox within w_info_consultarecepciondespacking
integer x = 1166
integer y = 1984
integer width = 306
integer height = 80
integer taborder = 90
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

event clicked;IF cbx_orden.Checked THEN
	em_orden.Enabled  	= False
	em_orden.Text  		= ''
ELSE
	em_orden.Enabled  	= True
	em_orden.SetFocus()
END IF
	
end event

type st_15 from statictext within w_info_consultarecepciondespacking
integer x = 251
integer y = 1944
integer width = 1920
integer height = 136
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type em_orden from editmask within w_info_consultarecepciondespacking
integer x = 686
integer y = 1960
integer width = 389
integer height = 96
integer taborder = 100
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 134217752
boolean enabled = false
alignment alignment = center!
borderstyle borderstyle = stylelowered!
string mask = "#####"
end type

event modified;String	ls_null

SetNull(ls_Null)

IF Not iuo_spro_ordenproceso.Existe(dw_planta.Object.plde_codigo[1],4,Long(em_orden.text),True,Sqlca,dw_cliente.Object.clie_codigo[1]) THEN
	em_calidad.Text			=	ls_Null
	This.SetFocus()
END IF	

end event

type st_16 from statictext within w_info_consultarecepciondespacking
integer x = 315
integer y = 1676
integer width = 297
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
string text = "Categoria"
boolean focusrectangle = false
end type

type cbx_categorias from checkbox within w_info_consultarecepciondespacking
integer x = 1682
integer y = 1688
integer width = 283
integer height = 80
integer taborder = 130
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

event clicked;integer	li_null

SetNull(li_null)

IF This.Checked THEN
	dw_categorias.Enabled						=	False
	dw_categorias.Object.cate_codigo[1]		=	li_null
	
ELSE
	dw_categorias.Enabled						=	True
	
END IF
end event

type dw_categorias from datawindow within w_info_consultarecepciondespacking
integer x = 681
integer y = 1668
integer width = 896
integer height = 108
integer taborder = 140
boolean bringtotop = true
boolean enabled = false
string title = "none"
string dataobject = "dddw_categorias"
boolean border = false
boolean livescroll = true
end type

