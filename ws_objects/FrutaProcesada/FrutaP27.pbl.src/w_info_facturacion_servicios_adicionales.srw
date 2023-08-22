$PBExportHeader$w_info_facturacion_servicios_adicionales.srw
forward
global type w_info_facturacion_servicios_adicionales from w_para_informes
end type
type st_4 from statictext within w_info_facturacion_servicios_adicionales
end type
type st_1 from statictext within w_info_facturacion_servicios_adicionales
end type
type st_2 from statictext within w_info_facturacion_servicios_adicionales
end type
type em_desde from editmask within w_info_facturacion_servicios_adicionales
end type
type dw_cliente from datawindow within w_info_facturacion_servicios_adicionales
end type
type st_6 from statictext within w_info_facturacion_servicios_adicionales
end type
type st_3 from statictext within w_info_facturacion_servicios_adicionales
end type
type st_7 from statictext within w_info_facturacion_servicios_adicionales
end type
type em_hasta from editmask within w_info_facturacion_servicios_adicionales
end type
type st_8 from statictext within w_info_facturacion_servicios_adicionales
end type
type gb_3 from groupbox within w_info_facturacion_servicios_adicionales
end type
type st_5 from statictext within w_info_facturacion_servicios_adicionales
end type
type st_variedad from statictext within w_info_facturacion_servicios_adicionales
end type
type st_embalaje from statictext within w_info_facturacion_servicios_adicionales
end type
type cbx_embalaje from checkbox within w_info_facturacion_servicios_adicionales
end type
type cb_buscaembalaje from commandbutton within w_info_facturacion_servicios_adicionales
end type
type cbx_consembalaje from checkbox within w_info_facturacion_servicios_adicionales
end type
type st_calidad from statictext within w_info_facturacion_servicios_adicionales
end type
type cbx_calidad from checkbox within w_info_facturacion_servicios_adicionales
end type
type cbx_conscalidad from checkbox within w_info_facturacion_servicios_adicionales
end type
type cbx_plantascons from checkbox within w_info_facturacion_servicios_adicionales
end type
type st_18 from statictext within w_info_facturacion_servicios_adicionales
end type
type cbx_packing from checkbox within w_info_facturacion_servicios_adicionales
end type
type cbx_packingcons from checkbox within w_info_facturacion_servicios_adicionales
end type
type dw_packing from datawindow within w_info_facturacion_servicios_adicionales
end type
type st_12 from statictext within w_info_facturacion_servicios_adicionales
end type
type em_calidad from editmask within w_info_facturacion_servicios_adicionales
end type
type cbx_cliente from checkbox within w_info_facturacion_servicios_adicionales
end type
type uo_selespecie from uo_seleccion_especie within w_info_facturacion_servicios_adicionales
end type
type uo_selvariedad from uo_seleccion_variedad within w_info_facturacion_servicios_adicionales
end type
type st_10 from statictext within w_info_facturacion_servicios_adicionales
end type
type em_embalaje from singlelineedit within w_info_facturacion_servicios_adicionales
end type
type st_19 from statictext within w_info_facturacion_servicios_adicionales
end type
type uo_selcate from uo_seleccion_categoria within w_info_facturacion_servicios_adicionales
end type
type uo_selproductor from uo_seleccion_varios_productores_clientes within w_info_facturacion_servicios_adicionales
end type
type uo_selplanta from uo_seleccion_plantas within w_info_facturacion_servicios_adicionales
end type
type uo_selservicio from uo_seleccion_servicios within w_info_facturacion_servicios_adicionales
end type
type st_9 from statictext within w_info_facturacion_servicios_adicionales
end type
type st_11 from statictext within w_info_facturacion_servicios_adicionales
end type
type uo_selserviciodeta from uo_seleccion_servicios_detalle within w_info_facturacion_servicios_adicionales
end type
type cbx_factura from checkbox within w_info_facturacion_servicios_adicionales
end type
type uo_seletiqueta from uo_seleccion_etiquetas within w_info_facturacion_servicios_adicionales
end type
type st_13 from statictext within w_info_facturacion_servicios_adicionales
end type
end forward

global type w_info_facturacion_servicios_adicionales from w_para_informes
integer x = 14
integer y = 32
integer width = 4032
integer height = 2100
string title = "FACTURACION SERVICIOS ADICIONALES PLANTA"
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
st_3 st_3
st_7 st_7
em_hasta em_hasta
st_8 st_8
gb_3 gb_3
st_5 st_5
st_variedad st_variedad
st_embalaje st_embalaje
cbx_embalaje cbx_embalaje
cb_buscaembalaje cb_buscaembalaje
cbx_consembalaje cbx_consembalaje
st_calidad st_calidad
cbx_calidad cbx_calidad
cbx_conscalidad cbx_conscalidad
cbx_plantascons cbx_plantascons
st_18 st_18
cbx_packing cbx_packing
cbx_packingcons cbx_packingcons
dw_packing dw_packing
st_12 st_12
em_calidad em_calidad
cbx_cliente cbx_cliente
uo_selespecie uo_selespecie
uo_selvariedad uo_selvariedad
st_10 st_10
em_embalaje em_embalaje
st_19 st_19
uo_selcate uo_selcate
uo_selproductor uo_selproductor
uo_selplanta uo_selplanta
uo_selservicio uo_selservicio
st_9 st_9
st_11 st_11
uo_selserviciodeta uo_selserviciodeta
cbx_factura cbx_factura
uo_seletiqueta uo_seletiqueta
st_13 st_13
end type
global w_info_facturacion_servicios_adicionales w_info_facturacion_servicios_adicionales

type variables
str_busqueda istr_busq
str_mant istr_mant

DataWindowChild	idwc_cliente,  idwc_productor, idwc_packing
String 	is_NomPlanta


uo_seleccion_varios_productores	iuo_selproductor
uo_calibre								iuo_calibre

end variables

forward prototypes
public function boolean existeespecie (integer especie)
public function boolean existeproductor (long ll_productor)
public function boolean existepacking (integer li_planta)
end prototypes

public function boolean existeespecie (integer especie);String		ls_Nombre

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
	istr_mant.argumento[4] = String(ll_Productor)	
	RETURN True
END IF
end function

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
	//istr_mant.argumento[7] = String(li_planta)
	RETURN True 
END IF
end function

on w_info_facturacion_servicios_adicionales.create
int iCurrent
call super::create
this.st_4=create st_4
this.st_1=create st_1
this.st_2=create st_2
this.em_desde=create em_desde
this.dw_cliente=create dw_cliente
this.st_6=create st_6
this.st_3=create st_3
this.st_7=create st_7
this.em_hasta=create em_hasta
this.st_8=create st_8
this.gb_3=create gb_3
this.st_5=create st_5
this.st_variedad=create st_variedad
this.st_embalaje=create st_embalaje
this.cbx_embalaje=create cbx_embalaje
this.cb_buscaembalaje=create cb_buscaembalaje
this.cbx_consembalaje=create cbx_consembalaje
this.st_calidad=create st_calidad
this.cbx_calidad=create cbx_calidad
this.cbx_conscalidad=create cbx_conscalidad
this.cbx_plantascons=create cbx_plantascons
this.st_18=create st_18
this.cbx_packing=create cbx_packing
this.cbx_packingcons=create cbx_packingcons
this.dw_packing=create dw_packing
this.st_12=create st_12
this.em_calidad=create em_calidad
this.cbx_cliente=create cbx_cliente
this.uo_selespecie=create uo_selespecie
this.uo_selvariedad=create uo_selvariedad
this.st_10=create st_10
this.em_embalaje=create em_embalaje
this.st_19=create st_19
this.uo_selcate=create uo_selcate
this.uo_selproductor=create uo_selproductor
this.uo_selplanta=create uo_selplanta
this.uo_selservicio=create uo_selservicio
this.st_9=create st_9
this.st_11=create st_11
this.uo_selserviciodeta=create uo_selserviciodeta
this.cbx_factura=create cbx_factura
this.uo_seletiqueta=create uo_seletiqueta
this.st_13=create st_13
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_4
this.Control[iCurrent+2]=this.st_1
this.Control[iCurrent+3]=this.st_2
this.Control[iCurrent+4]=this.em_desde
this.Control[iCurrent+5]=this.dw_cliente
this.Control[iCurrent+6]=this.st_6
this.Control[iCurrent+7]=this.st_3
this.Control[iCurrent+8]=this.st_7
this.Control[iCurrent+9]=this.em_hasta
this.Control[iCurrent+10]=this.st_8
this.Control[iCurrent+11]=this.gb_3
this.Control[iCurrent+12]=this.st_5
this.Control[iCurrent+13]=this.st_variedad
this.Control[iCurrent+14]=this.st_embalaje
this.Control[iCurrent+15]=this.cbx_embalaje
this.Control[iCurrent+16]=this.cb_buscaembalaje
this.Control[iCurrent+17]=this.cbx_consembalaje
this.Control[iCurrent+18]=this.st_calidad
this.Control[iCurrent+19]=this.cbx_calidad
this.Control[iCurrent+20]=this.cbx_conscalidad
this.Control[iCurrent+21]=this.cbx_plantascons
this.Control[iCurrent+22]=this.st_18
this.Control[iCurrent+23]=this.cbx_packing
this.Control[iCurrent+24]=this.cbx_packingcons
this.Control[iCurrent+25]=this.dw_packing
this.Control[iCurrent+26]=this.st_12
this.Control[iCurrent+27]=this.em_calidad
this.Control[iCurrent+28]=this.cbx_cliente
this.Control[iCurrent+29]=this.uo_selespecie
this.Control[iCurrent+30]=this.uo_selvariedad
this.Control[iCurrent+31]=this.st_10
this.Control[iCurrent+32]=this.em_embalaje
this.Control[iCurrent+33]=this.st_19
this.Control[iCurrent+34]=this.uo_selcate
this.Control[iCurrent+35]=this.uo_selproductor
this.Control[iCurrent+36]=this.uo_selplanta
this.Control[iCurrent+37]=this.uo_selservicio
this.Control[iCurrent+38]=this.st_9
this.Control[iCurrent+39]=this.st_11
this.Control[iCurrent+40]=this.uo_selserviciodeta
this.Control[iCurrent+41]=this.cbx_factura
this.Control[iCurrent+42]=this.uo_seletiqueta
this.Control[iCurrent+43]=this.st_13
end on

on w_info_facturacion_servicios_adicionales.destroy
call super::destroy
destroy(this.st_4)
destroy(this.st_1)
destroy(this.st_2)
destroy(this.em_desde)
destroy(this.dw_cliente)
destroy(this.st_6)
destroy(this.st_3)
destroy(this.st_7)
destroy(this.em_hasta)
destroy(this.st_8)
destroy(this.gb_3)
destroy(this.st_5)
destroy(this.st_variedad)
destroy(this.st_embalaje)
destroy(this.cbx_embalaje)
destroy(this.cb_buscaembalaje)
destroy(this.cbx_consembalaje)
destroy(this.st_calidad)
destroy(this.cbx_calidad)
destroy(this.cbx_conscalidad)
destroy(this.cbx_plantascons)
destroy(this.st_18)
destroy(this.cbx_packing)
destroy(this.cbx_packingcons)
destroy(this.dw_packing)
destroy(this.st_12)
destroy(this.em_calidad)
destroy(this.cbx_cliente)
destroy(this.uo_selespecie)
destroy(this.uo_selvariedad)
destroy(this.st_10)
destroy(this.em_embalaje)
destroy(this.st_19)
destroy(this.uo_selcate)
destroy(this.uo_selproductor)
destroy(this.uo_selplanta)
destroy(this.uo_selservicio)
destroy(this.st_9)
destroy(this.st_11)
destroy(this.uo_selserviciodeta)
destroy(this.cbx_factura)
destroy(this.uo_seletiqueta)
destroy(this.st_13)
end on

event open;Boolean lb_Cerrar

x	=	0
y	=	0

dw_cliente.GetChild("clie_codigo", idwc_cliente)
idwc_cliente.SetTransObject(SQLCA)
idwc_cliente.Retrieve()
dw_cliente.InsertRow(0)
dw_cliente.SetItem(1, "clie_codigo", gi_CodExport)

iuo_calibre   						=	Create uo_calibre

dw_packing.GetChild("plde_codigo", idwc_packing)
idwc_packing.SetTransObject(sqlca)
idwc_packing.Retrieve(2)
dw_packing.InsertRow(0)

dw_packing.Enabled											=	False
dw_packing.Object.plde_codigo.BackGround.Color	=	RGB(213,213,255)

// uo_seleccion_especie
IF IsNull(uo_SelEspecie.Codigo) 	THEN lb_Cerrar = True
IF IsNull(uo_SelEtiqueta.Codigo) 	THEN lb_Cerrar	=	True
IF IsNull(uo_SelCate.Codigo) 		THEN lb_Cerrar	=	True
IF IsNull(uo_SelProductor.Codigo)	THEN lb_Cerrar = True
IF IsNull(uo_SelVariedad.Codigo) 	THEN lb_Cerrar = True
IF IsNull(uo_SelPlanta.Codigo) 		THEN lb_Cerrar = True
IF IsNull(uo_SelServicio.Codigo) 	THEN lb_Cerrar = True
IF IsNull(uo_SelServiciodeta.Codigo) THEN lb_Cerrar = True

IF lb_Cerrar THEN
	Close(This)
ELSE
	uo_SelEspecie.Seleccion(True,True)
	uo_SelEtiqueta.Seleccion(True, True)
	uo_SelCate.Seleccion(True, True)
	uo_SelProductor.Seleccion(True,True)
	uo_SelVariedad.Seleccion(True,True)
	uo_SelPlanta.Seleccion(True,False)
	uo_SelServicio.Seleccion(True,False)
	uo_SelServiciodeta.Seleccion(True,False)

	uo_SelPlanta.Inicia(gi_codplanta)
END IF


em_desde.Text					=	String(RelativeDate(Today(), -365))
em_hasta.Text					=	String(Today())
istr_mant.argumento[8]  	=  "-1"							//	calidad
istr_mant.argumento[9]		= 	em_desde.Text				//	fecha inicio
istr_mant.argumento[10]	=	em_hasta.Text				//	fecha final
istr_mant.argumento[23]	= 	"-1"							//	packing
istr_mant.argumento[16] 	=  '-9'
end event

type pb_excel from w_para_informes`pb_excel within w_info_facturacion_servicios_adicionales
integer x = 3598
integer y = 820
fontcharset fontcharset = ansi!
string facename = "Tahoma"
end type

type st_computador from w_para_informes`st_computador within w_info_facturacion_servicios_adicionales
integer textsize = -10
fontfamily fontfamily = swiss!
string facename = "Tahoma"
end type

type st_usuario from w_para_informes`st_usuario within w_info_facturacion_servicios_adicionales
integer textsize = -10
fontfamily fontfamily = swiss!
string facename = "Tahoma"
end type

type st_temporada from w_para_informes`st_temporada within w_info_facturacion_servicios_adicionales
integer textsize = -10
fontfamily fontfamily = swiss!
string facename = "Tahoma"
end type

type p_logo from w_para_informes`p_logo within w_info_facturacion_servicios_adicionales
end type

type st_titulo from w_para_informes`st_titulo within w_info_facturacion_servicios_adicionales
integer width = 3259
string text = "Informe Facturación Servicios Adicionales Planta"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_facturacion_servicios_adicionales
integer x = 3643
integer y = 1248
integer taborder = 160
fontcharset fontcharset = ansi!
string facename = "Tahoma"
end type

event pb_acepta::clicked;SetPointer(Arrow!)

Integer	fila, li_cliente, li_planta, li_packing, li_emba, li_emba2, li_factura
Date		ld_desde, ld_hasta
String	texto_desde, texto_hasta, texto_fecha, ls_cajas, ls_null, ls_cliente, ls_descri, ls_lista, ls_construyelike1, ls_construye, &
			ls_string, ls_construyelike, ls_embalaje, ls_calidad
Long		ll_productor

SetNull(ls_null)

istr_info.titulo	= 'INFORME DE FACTURACION SERVICIOS ADICIONALES PLANTA'

OpenWithParm(vinf, istr_info)
vinf.dw_1.DataObject = "dw_info_factura_serviciopalnataadicional"

IF cbx_cliente.Checked THEN
	li_cliente = -1
ELSE
	li_cliente = dw_cliente.Object.clie_codigo[1]
END IF	

li_emba = len(ls_embalaje)

FOR li_emba2 = 1 TO li_emba
	ls_string = mid(ls_embalaje,li_emba2,1)
	
	IF ls_string <> ',' THEN
		ls_construye = ls_construye+ls_string
	ELSE
		IF ls_construyelike1 = '' THEN
			ls_construyelike1 = ' emba_codigo like '+''+"'%"+ls_construye+"%'"+''
			ls_construye = ''
		ELSE	
			IF ls_construyelike = '' THEN
				ls_construyelike = ls_construyelike1 +' or emba_codigo like '+''+"'%"+ls_construye+"%'"+''
			ELSE
				ls_construyelike = ls_construyelike +' or emba_codigo like '+''+"'%"+ls_construye+"%'"+''
			END IF
			ls_construye = ''
		END IF	
	END IF	
NEXT	

IF ls_construyelike = '' THEN
	ls_construyelike = ls_construyelike1
END IF	

/*
Especies
*/
IF IsNull(uo_selespecie.Codigo)THEN
	MessageBox("Atención","Debe Seleccionar una Especie Previamente",Exclamation!)
	uo_selespecie.dw_Seleccion.SetFocus()
	RETURN
END IF

ls_lista = uo_selproductor.Lista

/*
Variedad
*/
IF IsNull(uo_selvariedad.Codigo)THEN
	MessageBox("Atención","Debe Seleccionar una Variedad Previamente",Exclamation!)
	uo_selvariedad.dw_Seleccion.SetFocus()
	RETURN
END IF
/*
planta
*/
IF IsNull(uo_selplanta.Codigo)THEN
	MessageBox("Atención","Debe Seleccionar una Planta Previamente",Exclamation!)
	uo_selplanta.dw_Seleccion.SetFocus()
	RETURN
END IF
/*
categoria
*/
IF IsNull(uo_selcate.Codigo)THEN
	MessageBox("Atención","Debe Seleccionar una Categoria Previamente",Exclamation!)
	uo_selvariedad.dw_Seleccion.SetFocus()
	RETURN
END IF

ld_desde = Date(em_desde.Text)
ld_hasta = Date(em_hasta.Text)

texto_desde		=  f_fecha_texto(String(ld_desde), 1)
texto_hasta		=	f_fecha_texto(String(ld_hasta), 1)
texto_fecha		=	"Desde El :  " + texto_desde + "   Hasta El :  " + texto_hasta

//Obtiene descripciones de cliente y especie
  Select clie_nombre  
    into :ls_cliente  
    from dbo.clientesprod  
   where clie_codigo = :li_cliente ;

IF uo_selplanta.Codigo = -1 OR uo_selplanta.Codigo = -9 THEN 
	uo_selplanta.Codigo = 0
END IF	

IF cbx_embalaje.Checked OR cbx_consembalaje.Checked THEN
	ls_construyelike = "'Z' = 'Z'"
END IF

IF cbx_factura.Checked THEN
	li_factura = 1
ELSE
	li_factura = 0
END IF	

vinf.dw_1.SetTransObject(sqlca)

fila	=	vinf.dw_1.Retrieve(li_cliente, uo_selplanta.Codigo, ls_lista, Integer(istr_mant.argumento[23]),&
					uo_selespecie.Codigo,uo_selvariedad.Codigo,ls_construyelike, istr_mant.argumento[8],&
					uo_selcate.Codigo,uo_selservicio.codigo,uo_selserviciodeta.codigo,&
					date(istr_mant.argumento[9]),date(istr_mant.argumento[10]),li_factura,uo_seletiqueta.Codigo,&
					istr_mant.argumento[16])

IF fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
			   	"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", &
	             StopSign!, Ok!)
ELSE
	   F_Membrete(vinf.dw_1)
		vinf.dw_1.Modify("fechas.text = '" + texto_fecha + "'")
		vinf.dw_1.Modify("Cajas.text = '" + ls_cajas + "'")
		vinf.dw_1.Modify("Cliente.text = '" + ls_cliente + "'")
		vinf.dw_1.Modify("Especie.text = '" + uo_selespecie.Nombre + "'")
	IF gs_Ambiente <> 'Windows' THEN
		F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
	END IF
END IF

SetPointer(Arrow!)				
end event

type pb_salir from w_para_informes`pb_salir within w_info_facturacion_servicios_adicionales
integer x = 3648
integer y = 1520
integer taborder = 170
fontcharset fontcharset = ansi!
string facename = "Tahoma"
end type

type st_4 from statictext within w_info_facturacion_servicios_adicionales
integer x = 242
integer y = 564
integer width = 1618
integer height = 1060
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long backcolor = 16711680
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_1 from statictext within w_info_facturacion_servicios_adicionales
integer x = 302
integer y = 660
integer width = 238
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

type st_2 from statictext within w_info_facturacion_servicios_adicionales
integer x = 850
integer y = 1736
integer width = 197
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

type em_desde from editmask within w_info_facturacion_servicios_adicionales
integer x = 1061
integer y = 1720
integer width = 457
integer height = 96
integer taborder = 140
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

event modified;istr_mant.argumento[9]	=	This.Text
end event

type dw_cliente from datawindow within w_info_facturacion_servicios_adicionales
integer x = 1271
integer y = 460
integer width = 1143
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
	This.SetItem(1, "clie_codigo", Integer(ls_null))
	RETURN 1
END IF
end event

event itemerror;RETURN 1
end event

type st_6 from statictext within w_info_facturacion_servicios_adicionales
integer x = 1038
integer y = 476
integer width = 233
integer height = 52
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

type st_3 from statictext within w_info_facturacion_servicios_adicionales
integer x = 302
integer y = 1312
integer width = 270
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

type st_7 from statictext within w_info_facturacion_servicios_adicionales
integer x = 1778
integer y = 1736
integer width = 279
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

type em_hasta from editmask within w_info_facturacion_servicios_adicionales
integer x = 2085
integer y = 1720
integer width = 457
integer height = 96
integer taborder = 150
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

event modified;istr_mant.argumento[10]	=	This.Text
end event

type st_8 from statictext within w_info_facturacion_servicios_adicionales
integer x = 302
integer y = 904
integer width = 329
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

type gb_3 from groupbox within w_info_facturacion_servicios_adicionales
integer x = 311
integer y = 1652
integer width = 3086
integer height = 200
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Fecha Movimiento"
end type

type st_5 from statictext within w_info_facturacion_servicios_adicionales
integer x = 1861
integer y = 564
integer width = 1641
integer height = 1060
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long backcolor = 16711680
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_variedad from statictext within w_info_facturacion_servicios_adicionales
integer x = 302
integer y = 1484
integer width = 315
integer height = 96
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

type st_embalaje from statictext within w_info_facturacion_servicios_adicionales
integer x = 1888
integer y = 668
integer width = 302
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
string text = "Embalaje"
boolean focusrectangle = false
end type

type cbx_embalaje from checkbox within w_info_facturacion_servicios_adicionales
integer x = 2395
integer y = 580
integer width = 416
integer height = 72
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
	cbx_consembalaje.Enabled	=	True
	em_embalaje.Enabled			=	False
	cb_buscaembalaje.Enabled	=	False
	em_embalaje.Text				=	''
	istr_mant.argumento[6]		=	'Z'
	istr_mant.argumento[16]		=	'0'
ELSE
	cbx_consembalaje.Enabled	=	False
	cbx_consembalaje.Checked	=	False
	em_embalaje.Enabled			=	True
	cb_buscaembalaje.Enabled	=	True
END IF
end event

type cb_buscaembalaje from commandbutton within w_info_facturacion_servicios_adicionales
integer x = 2720
integer y = 656
integer width = 110
integer height = 84
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
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
	istr_mant.argumento[6]	=	lstr_busq.argum[2]
END IF
end event

type cbx_consembalaje from checkbox within w_info_facturacion_servicios_adicionales
integer x = 2885
integer y = 584
integer width = 535
integer height = 72
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Consolidado"
boolean checked = true
end type

event clicked;IF This.Checked = TRUE THEN
	istr_mant.argumento[16]	=	'-9'
ELSE
	istr_mant.argumento[16]	=	'-1'
END IF

end event

type st_calidad from statictext within w_info_facturacion_servicios_adicionales
integer x = 1888
integer y = 832
integer width = 270
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

type cbx_calidad from checkbox within w_info_facturacion_servicios_adicionales
integer x = 2395
integer y = 748
integer width = 311
integer height = 72
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
	istr_mant.argumento[8]		=	'-1'
	
ELSE
	cbx_conscalidad.Enabled	=	False
	cbx_conscalidad.Checked	=	False
	em_calidad.Enabled		=	True
	em_calidad.SetFocus()
END IF


end event

type cbx_conscalidad from checkbox within w_info_facturacion_servicios_adicionales
integer x = 2885
integer y = 748
integer width = 485
integer height = 72
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Consolidado"
boolean checked = true
end type

event clicked;IF This.Checked = TRUE THEN
	istr_mant.argumento[8]	=	'-9'
END IF

end event

type cbx_plantascons from checkbox within w_info_facturacion_servicios_adicionales
boolean visible = false
integer x = 4361
integer y = 776
integer width = 489
integer height = 72
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
long backcolor = 12632256
string text = "Consolidadas"
boolean checked = true
end type

event clicked;IF This.Checked = TRUE THEN
	istr_mant.argumento[13]	=	'1'
ELSE
	istr_mant.argumento[13]	=	'0'
END IF
	
end event

type st_18 from statictext within w_info_facturacion_servicios_adicionales
integer x = 302
integer y = 1120
integer width = 247
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
string text = "Packing"
boolean focusrectangle = false
end type

type cbx_packing from checkbox within w_info_facturacion_servicios_adicionales
integer x = 782
integer y = 1048
integer width = 402
integer height = 72
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
	cbx_packingcons.Enabled									=	True
	dw_packing.Enabled										=	False
	dw_packing.Object.plde_codigo.BackGround.Color	=	RGB(213,213,255)
	istr_mant.argumento[23]									=	'-1'
	dw_packing.Reset()
	
	idwc_packing.Retrieve(2)
	dw_packing.InsertRow(0)

ELSE
	cbx_packingcons.Enabled									=	False
	cbx_packingcons.Checked									=	False
	dw_packing.Enabled										=	True
	dw_packing.Object.plde_codigo.BackGround.Color	=	RGB(255, 255, 255)

	dw_packing.SetFocus()
END IF
end event

type cbx_packingcons from checkbox within w_info_facturacion_servicios_adicionales
integer x = 1253
integer y = 1044
integer width = 471
integer height = 72
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Consolidado"
end type

event clicked;IF This.Checked = TRUE THEN
	istr_mant.argumento[23]	=	'-9'
ELSE
	istr_mant.argumento[23]	=	'-1'
END IF
	
end event

type dw_packing from datawindow within w_info_facturacion_servicios_adicionales
integer x = 777
integer y = 1112
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

li_Cliente	=	Integer(istr_mant.argumento[1])

IF ExistePacking(Integer(data)) THEN
	istr_mant.argumento[23]	=	data
ELSE
	This.SetItem(1, "plde_codigo", ls_null)
	RETURN 1
END IF
end event

event itemerror;RETURN 1
end event

type st_12 from statictext within w_info_facturacion_servicios_adicionales
integer x = 242
integer y = 436
integer width = 3259
integer height = 124
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long backcolor = 16711680
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type em_calidad from editmask within w_info_facturacion_servicios_adicionales
integer x = 2395
integer y = 812
integer width = 311
integer height = 96
integer taborder = 100
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
	li_cliente	=	Integer(istr_mant.argumento[1]) // Cliente
	li_especie	=	Integer(uo_selespecie.Codigo) // Especie
	li_variedad	=	Integer(uo_selvariedad.Codigo) // Variedad
	ls_calibre	=	This.Text
	
	IF NOT iuo_calibre.existe(li_especie,li_variedad,ls_calibre,True,sqlca) THEN
		em_calidad.Text = ''
		This.SetFocus()
		Return 1
	ELSE	
		istr_mant.argumento[8]	 = iuo_calibre.calibre
		em_calidad.Text			 = iuo_calibre.calibre
		Return 1
	END IF	
END IF	
end event

type cbx_cliente from checkbox within w_info_facturacion_servicios_adicionales
integer x = 2441
integer y = 472
integer width = 293
integer height = 60
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
end type

event clicked;Integer li_null
SetNull(li_null)
IF This.Checked THEN
	dw_cliente.Enabled										=	False
	dw_cliente.Object.clie_codigo.BackGround.Color	=	RGB(192, 192, 192)
   istr_mant.argumento[1]                          = '-1'
	dw_cliente.SetItem(1,"clie_codigo",li_null)
	
	uo_selproductor.Filtra(-1,-1,-1)
	
ELSE
	dw_cliente.Enabled											=	True
	dw_cliente.Object.clie_codigo.BackGround.Color	=	RGB(255, 255, 255)
	dw_cliente.SetFocus()
END IF
end event

type uo_selespecie from uo_seleccion_especie within w_info_facturacion_servicios_adicionales
event destroy ( )
integer x = 777
integer y = 1216
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

type uo_selvariedad from uo_seleccion_variedad within w_info_facturacion_servicios_adicionales
event destroy ( )
integer x = 777
integer y = 1392
integer width = 910
integer taborder = 80
boolean bringtotop = true
end type

on uo_selvariedad.destroy
call uo_seleccion_variedad::destroy
end on

type st_10 from statictext within w_info_facturacion_servicios_adicionales
integer x = 242
integer y = 1624
integer width = 3259
integer height = 288
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long backcolor = 16711680
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type em_embalaje from singlelineedit within w_info_facturacion_servicios_adicionales
integer x = 2395
integer y = 660
integer width = 311
integer height = 84
integer taborder = 120
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
boolean enabled = false
textcase textcase = upper!
borderstyle borderstyle = stylelowered!
end type

event modified;Integer		li_cliente
String		ls_embalaje, ls_Nombre

li_cliente	=	Integer(istr_mant.argumento[1]) // Cliente
ls_embalaje	=	This.Text

SELECT	emba_nombre
	INTO	:ls_Nombre
	FROM	dbo.embalajesprod
	WHERE	clie_codigo	=	:li_cliente
	AND	emba_codigo	=	:ls_embalaje;
	
//IF sqlca.SQLCode = -1 THEN
//	F_ErrorBaseDatos(sqlca, "Lectura de tabla Embalajes")
//	This.SetFocus()
//ELSEIF sqlca.SQLCode = 100 THEN
//	MessageBox("Atención", "Código de Embalaje no ha sido Definido.~r~r" + &
//		"Ingrese o seleccione otro Código.")
//	This.SetFocus()
//ELSE
	istr_mant.argumento[6]	=	ls_embalaje
	istr_mant.argumento[16]	=	'0'
//END IF
end event

type st_19 from statictext within w_info_facturacion_servicios_adicionales
integer x = 1888
integer y = 1000
integer width = 320
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
string text = "Categoria"
boolean focusrectangle = false
end type

type uo_selcate from uo_seleccion_categoria within w_info_facturacion_servicios_adicionales
integer x = 2395
integer y = 916
integer width = 910
integer taborder = 340
boolean bringtotop = true
end type

on uo_selcate.destroy
call uo_seleccion_categoria::destroy
end on

type uo_selproductor from uo_seleccion_varios_productores_clientes within w_info_facturacion_servicios_adicionales
integer x = 777
integer y = 760
integer taborder = 40
boolean bringtotop = true
end type

on uo_selproductor.destroy
call uo_seleccion_varios_productores_clientes::destroy
end on

event ue_cambio;call super::ue_cambio;uo_selproductor.Filtra(-1,-1,dw_cliente.Object.clie_Codigo[1])
end event

type uo_selplanta from uo_seleccion_plantas within w_info_facturacion_servicios_adicionales
integer x = 777
integer y = 572
integer taborder = 40
boolean bringtotop = true
end type

on uo_selplanta.destroy
call uo_seleccion_plantas::destroy
end on

type uo_selservicio from uo_seleccion_servicios within w_info_facturacion_servicios_adicionales
integer x = 2395
integer y = 1080
integer taborder = 80
boolean bringtotop = true
end type

on uo_selservicio.destroy
call uo_seleccion_servicios::destroy
end on

event ue_cambio;call super::ue_cambio;IF IsNull(This.Codigo) THEN RETURN

CHOOSE CASE This.Codigo
	CASE -1, -9
		uo_selserviciodeta.cbx_Todos.Checked		=	True
		uo_selserviciodeta.cbx_Consolida.Enabled	=	False
		uo_selserviciodeta.dw_Seleccion.Enabled	=	False
		uo_selserviciodeta.Enabled						=	False		
	CASE ELSE
		uo_selserviciodeta.Filtra(This.Codigo)
		uo_selserviciodeta.Enabled						=	True

END CHOOSE
end event

type st_9 from statictext within w_info_facturacion_servicios_adicionales
integer x = 1888
integer y = 1148
integer width = 320
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
string text = "Servicios"
boolean focusrectangle = false
end type

type st_11 from statictext within w_info_facturacion_servicios_adicionales
integer x = 1888
integer y = 1308
integer width = 503
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
string text = "Servicios Detalle"
boolean focusrectangle = false
end type

type uo_selserviciodeta from uo_seleccion_servicios_detalle within w_info_facturacion_servicios_adicionales
integer x = 2395
integer y = 1256
integer taborder = 80
boolean bringtotop = true
boolean enabled = false
end type

on uo_selserviciodeta.destroy
call uo_seleccion_servicios_detalle::destroy
end on

type cbx_factura from checkbox within w_info_facturacion_servicios_adicionales
integer x = 2670
integer y = 1728
integer width = 571
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
string text = "Facturable"
boolean lefttext = true
end type

type uo_seletiqueta from uo_seleccion_etiquetas within w_info_facturacion_servicios_adicionales
integer x = 2395
integer y = 1428
integer taborder = 90
boolean bringtotop = true
end type

on uo_seletiqueta.destroy
call uo_seleccion_etiquetas::destroy
end on

type st_13 from statictext within w_info_facturacion_servicios_adicionales
integer x = 1888
integer y = 1472
integer width = 503
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
string text = "Etiquetas "
boolean focusrectangle = false
end type

