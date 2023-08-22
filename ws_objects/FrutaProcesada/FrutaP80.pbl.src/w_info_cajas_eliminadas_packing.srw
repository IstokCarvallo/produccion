$PBExportHeader$w_info_cajas_eliminadas_packing.srw
$PBExportComments$Informe Existencia Sitio Revisión
forward
global type w_info_cajas_eliminadas_packing from w_para_informes
end type
type st_1 from statictext within w_info_cajas_eliminadas_packing
end type
type st_2 from statictext within w_info_cajas_eliminadas_packing
end type
type em_desde from editmask within w_info_cajas_eliminadas_packing
end type
type dw_cliente from datawindow within w_info_cajas_eliminadas_packing
end type
type st_6 from statictext within w_info_cajas_eliminadas_packing
end type
type dw_planta from datawindow within w_info_cajas_eliminadas_packing
end type
type st_3 from statictext within w_info_cajas_eliminadas_packing
end type
type st_7 from statictext within w_info_cajas_eliminadas_packing
end type
type em_hasta from editmask within w_info_cajas_eliminadas_packing
end type
type st_variedad from statictext within w_info_cajas_eliminadas_packing
end type
type st_embalaje from statictext within w_info_cajas_eliminadas_packing
end type
type cbx_embalaje from checkbox within w_info_cajas_eliminadas_packing
end type
type cbx_consembalaje from checkbox within w_info_cajas_eliminadas_packing
end type
type st_calidad from statictext within w_info_cajas_eliminadas_packing
end type
type em_calidad from editmask within w_info_cajas_eliminadas_packing
end type
type cbx_calidad from checkbox within w_info_cajas_eliminadas_packing
end type
type cbx_conscalidad from checkbox within w_info_cajas_eliminadas_packing
end type
type cbx_planta from checkbox within w_info_cajas_eliminadas_packing
end type
type dw_embalaje from datawindow within w_info_cajas_eliminadas_packing
end type
type dw_packing from datawindow within w_info_cajas_eliminadas_packing
end type
type st_5 from statictext within w_info_cajas_eliminadas_packing
end type
type cbx_cajas from checkbox within w_info_cajas_eliminadas_packing
end type
type gb_4 from groupbox within w_info_cajas_eliminadas_packing
end type
type gb_3 from groupbox within w_info_cajas_eliminadas_packing
end type
type st_4 from statictext within w_info_cajas_eliminadas_packing
end type
type st_9 from statictext within w_info_cajas_eliminadas_packing
end type
type st_10 from statictext within w_info_cajas_eliminadas_packing
end type
type uo_selespecie from uo_seleccion_especie within w_info_cajas_eliminadas_packing
end type
type uo_selvariedad from uo_seleccion_variedad within w_info_cajas_eliminadas_packing
end type
type cbx_varirotula from checkbox within w_info_cajas_eliminadas_packing
end type
type st_12 from statictext within w_info_cajas_eliminadas_packing
end type
type em_numero from editmask within w_info_cajas_eliminadas_packing
end type
end forward

global type w_info_cajas_eliminadas_packing from w_para_informes
integer x = 14
integer y = 32
integer width = 3890
integer height = 1784
string title = "CAJAS ELIMINADAS POR PACKING"
boolean minbox = false
boolean maxbox = false
boolean resizable = false
windowtype windowtype = response!
string icon = "F:\Desarrollo\Producción\FrutaProcesada\Producc.ico"
st_1 st_1
st_2 st_2
em_desde em_desde
dw_cliente dw_cliente
st_6 st_6
dw_planta dw_planta
st_3 st_3
st_7 st_7
em_hasta em_hasta
st_variedad st_variedad
st_embalaje st_embalaje
cbx_embalaje cbx_embalaje
cbx_consembalaje cbx_consembalaje
st_calidad st_calidad
em_calidad em_calidad
cbx_calidad cbx_calidad
cbx_conscalidad cbx_conscalidad
cbx_planta cbx_planta
dw_embalaje dw_embalaje
dw_packing dw_packing
st_5 st_5
cbx_cajas cbx_cajas
gb_4 gb_4
gb_3 gb_3
st_4 st_4
st_9 st_9
st_10 st_10
uo_selespecie uo_selespecie
uo_selvariedad uo_selvariedad
cbx_varirotula cbx_varirotula
st_12 st_12
em_numero em_numero
end type
global w_info_cajas_eliminadas_packing w_info_cajas_eliminadas_packing

type variables
str_busqueda istr_busq
str_mant istr_mant

DataWindowChild	idwc_tipopro, idwc_cliente, idwc_planta, idwc_especie, idwc_productor, idwc_pesoneto, &
						idwc_etiqueta, idwc_packing, idwc_zonas, idwc_envases, dwc_mercado
						
String 				is_NomPlanta,is_embalaje, is_calibre, is_Cliente
Integer 				ii_cliente, ii_planta, ii_productor, ii_especie, ii_variedad, ii_destino, ii_status,&
		 				ii_envate, ii_envaec, ii_Packing
Date 					id_fecini, id_fecter				

uo_seleccion_especie		iuo_selespecie
uo_seleccion_variedad	iuo_selvariedad
uo_calibre								iuo_calibre
end variables

forward prototypes
public function boolean existeespecie (integer especie)
public function boolean existeproductor (long ll_productor)
public function boolean existepacking (integer li_planta)
public function boolean noexisteetiqueta (integer li_etiqueta)
public function boolean noexistestatus (integer ia_codigo)
public function boolean noexistedestinos (integer ia_codigo)
public subroutine buscadescliente (integer ai_cliente)
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
	istr_mant.argumento[4] = String(ll_Productor)	
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
	//istr_mant.argumento[7] = String(li_planta)
	RETURN True 
END IF
end function

public function boolean noexisteetiqueta (integer li_etiqueta);String	ls_nombre


SELECT	etiq_nombre
INTO    :ls_nombre
FROM	dba.etiquetas
WHERE	etiq_codigo =  :li_etiqueta;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de tabla Etiquetas")
	RETURN True
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "Código de Etiqueta no ha sido Definido.~r~r" + &
					"Ingrese o seleccione otro Código.")
	RETURN True
ELSE
	RETURN False 
END IF
end function

public function boolean noexistestatus (integer ia_codigo);Integer	li_existe
boolean lb_retorno
SELECT	count(*)
	INTO	:li_existe
	FROM	dba.status
	WHERE	stat_codigo	= :ia_codigo;
		
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de tabla status")
	lb_retorno = TRUE
ELSEIF li_existe > 0 THEN	
	lb_retorno = FALSE
ELSE
	lb_retorno = TRUE
END IF

RETURN lb_retorno
end function

public function boolean noexistedestinos (integer ia_codigo);Integer	li_existe
boolean lb_retorno
SELECT	count(*)
	INTO	:li_existe
	FROM	dba.destinos
	WHERE	dest_codigo	= :ia_codigo;
		
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de tabla destinos")
	lb_retorno = TRUE
ELSEIF li_existe > 0 THEN	
	lb_retorno = FALSE
ELSE
	lb_retorno = TRUE
END IF

RETURN lb_retorno
end function

public subroutine buscadescliente (integer ai_cliente);
SELECT	clie_nombre
	INTO	:is_Cliente
	FROM	dba.clientesprod
	WHERE	clie_codigo	=	:ai_Cliente ;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, " Error en Lectura de tabla Clientes Producción")	
	is_Cliente = ''
ELSEIF sqlca.SQLCode = 100 THEN
	is_Cliente = ''
END IF
end subroutine

on w_info_cajas_eliminadas_packing.create
int iCurrent
call super::create
this.st_1=create st_1
this.st_2=create st_2
this.em_desde=create em_desde
this.dw_cliente=create dw_cliente
this.st_6=create st_6
this.dw_planta=create dw_planta
this.st_3=create st_3
this.st_7=create st_7
this.em_hasta=create em_hasta
this.st_variedad=create st_variedad
this.st_embalaje=create st_embalaje
this.cbx_embalaje=create cbx_embalaje
this.cbx_consembalaje=create cbx_consembalaje
this.st_calidad=create st_calidad
this.em_calidad=create em_calidad
this.cbx_calidad=create cbx_calidad
this.cbx_conscalidad=create cbx_conscalidad
this.cbx_planta=create cbx_planta
this.dw_embalaje=create dw_embalaje
this.dw_packing=create dw_packing
this.st_5=create st_5
this.cbx_cajas=create cbx_cajas
this.gb_4=create gb_4
this.gb_3=create gb_3
this.st_4=create st_4
this.st_9=create st_9
this.st_10=create st_10
this.uo_selespecie=create uo_selespecie
this.uo_selvariedad=create uo_selvariedad
this.cbx_varirotula=create cbx_varirotula
this.st_12=create st_12
this.em_numero=create em_numero
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_1
this.Control[iCurrent+2]=this.st_2
this.Control[iCurrent+3]=this.em_desde
this.Control[iCurrent+4]=this.dw_cliente
this.Control[iCurrent+5]=this.st_6
this.Control[iCurrent+6]=this.dw_planta
this.Control[iCurrent+7]=this.st_3
this.Control[iCurrent+8]=this.st_7
this.Control[iCurrent+9]=this.em_hasta
this.Control[iCurrent+10]=this.st_variedad
this.Control[iCurrent+11]=this.st_embalaje
this.Control[iCurrent+12]=this.cbx_embalaje
this.Control[iCurrent+13]=this.cbx_consembalaje
this.Control[iCurrent+14]=this.st_calidad
this.Control[iCurrent+15]=this.em_calidad
this.Control[iCurrent+16]=this.cbx_calidad
this.Control[iCurrent+17]=this.cbx_conscalidad
this.Control[iCurrent+18]=this.cbx_planta
this.Control[iCurrent+19]=this.dw_embalaje
this.Control[iCurrent+20]=this.dw_packing
this.Control[iCurrent+21]=this.st_5
this.Control[iCurrent+22]=this.cbx_cajas
this.Control[iCurrent+23]=this.gb_4
this.Control[iCurrent+24]=this.gb_3
this.Control[iCurrent+25]=this.st_4
this.Control[iCurrent+26]=this.st_9
this.Control[iCurrent+27]=this.st_10
this.Control[iCurrent+28]=this.uo_selespecie
this.Control[iCurrent+29]=this.uo_selvariedad
this.Control[iCurrent+30]=this.cbx_varirotula
this.Control[iCurrent+31]=this.st_12
this.Control[iCurrent+32]=this.em_numero
end on

on w_info_cajas_eliminadas_packing.destroy
call super::destroy
destroy(this.st_1)
destroy(this.st_2)
destroy(this.em_desde)
destroy(this.dw_cliente)
destroy(this.st_6)
destroy(this.dw_planta)
destroy(this.st_3)
destroy(this.st_7)
destroy(this.em_hasta)
destroy(this.st_variedad)
destroy(this.st_embalaje)
destroy(this.cbx_embalaje)
destroy(this.cbx_consembalaje)
destroy(this.st_calidad)
destroy(this.em_calidad)
destroy(this.cbx_calidad)
destroy(this.cbx_conscalidad)
destroy(this.cbx_planta)
destroy(this.dw_embalaje)
destroy(this.dw_packing)
destroy(this.st_5)
destroy(this.cbx_cajas)
destroy(this.gb_4)
destroy(this.gb_3)
destroy(this.st_4)
destroy(this.st_9)
destroy(this.st_10)
destroy(this.uo_selespecie)
destroy(this.uo_selvariedad)
destroy(this.cbx_varirotula)
destroy(this.st_12)
destroy(this.em_numero)
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

iuo_calibre   				=	Create uo_calibre

dw_planta.Enabled											=	False
dw_planta.Object.plde_codigo.BackGround.Color	=	RGB(192, 192, 192)

dw_embalaje.enabled 											=	False
dw_embalaje.Object.emba_codigo.BackGround.Color		=	RGB(192, 192, 192)

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


dw_embalaje.Enabled		=	False
dw_embalaje.SetTransObject(SQLCA)
dw_embalaje.Retrieve()
dw_embalaje.InsertRow(0)

em_desde.Text				=	String(RelativeDate(Today(), -365))
em_hasta.Text				=	String(Today())

ii_cliente 					= 	gi_CodExport
ii_planta					= 	-1
ii_productor				= 	-1
ii_envate					=	-1
ii_envaec					=	-1
id_fecini					=	RelativeDate(Today(), -365)
id_fecter					=	Today()
is_embalaje					=	'-1'
is_calibre					=	'-1'

SELECT empr_packing 
	INTO :ii_Packing from dba.parempresa;
	
dw_packing.GetChild("plde_codigo", idwc_packing)
idwc_packing.SetTransObject(sqlca)
idwc_packing.Retrieve(2)
dw_packing.InsertRow(0)
dw_packing.SetItem(1, "plde_codigo", ii_Packing)

istr_mant.argumento[1]	=	String(gi_codexport)

end event

event resize;//
end event

type st_computador from w_para_informes`st_computador within w_info_cajas_eliminadas_packing
end type

type st_usuario from w_para_informes`st_usuario within w_info_cajas_eliminadas_packing
end type

type st_temporada from w_para_informes`st_temporada within w_info_cajas_eliminadas_packing
end type

type p_logo from w_para_informes`p_logo within w_info_cajas_eliminadas_packing
end type

type st_titulo from w_para_informes`st_titulo within w_info_cajas_eliminadas_packing
integer width = 3131
string text = "Cajas Eliminadas por Packing (Ajusta R.Proceso)"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_cajas_eliminadas_packing
integer x = 3461
integer y = 1120
integer taborder = 130
end type

event pb_acepta::clicked;SetPointer(Arrow!)

Integer	fila, li_Caja, li_varirotula
String	texto_desde, texto_hasta, texto_fecha, ls_null
Long		ll_Proceso

li_caja = 1

SetNull(ls_null)

istr_info.titulo	= 'CAJAS ELIMINADAS POR PACKING'	

ll_Proceso = Long(em_numero.Text)

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

BuscaDesCliente(ii_cliente)

OpenWithParm(vinf, istr_info)
vinf.dw_1.DataObject = "dw_info_cajas_eliminadas_packing"

texto_desde		=  f_fecha_texto(String(id_fecini), 1)
texto_hasta		=	f_fecha_texto(String(id_fecter), 1)
texto_fecha		=	"Desde El :  " + texto_desde + "   Hasta El :  " + texto_hasta

vinf.dw_1.SetTransObject(sqlca)

IF IsNull(ii_cliente) OR IsNull(ii_planta) OR IsNull(is_embalaje) OR IsNull(is_calibre) OR IsNull(id_fecini) &
	OR IsNull(id_fecter) OR IsNull(ii_Packing) OR IsNull(ll_Proceso) THEN
		MessageBox("Error", "Debe Ingresar todos los parametros para el informe", StopSign!)
		return 1
END IF

IF cbx_cajas.Checked = True THEN
	li_caja = 2
ELSE
	li_caja = 1
END IF	

IF cbx_varirotula.Checked THEN
	li_varirotula = 1
ELSE
	li_varirotula = 0
END IF

fila	=	vinf.dw_1.Retrieve(ii_cliente, ii_planta, uo_selespecie.Codigo, uo_selvariedad.Codigo, is_embalaje, &
									is_calibre, id_fecini,id_fecter,ii_Packing,li_caja,li_varirotula,ll_Proceso)

IF fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
			   	"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", &
	             StopSign!, Ok!)
ELSE
	   F_Membrete(vinf.dw_1)
		vinf.dw_1.Modify("fechas.text = '" + texto_fecha + "'")
//		vinf.dw_1.Modify("t_cliente.text = '" + is_Cliente + "'")	
//		vinf.dw_1.Modify("proceso.text = '" + String(ll_Proceso) + "'")	
	IF gs_Ambiente <> 'Windows' THEN
		F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
	END IF
END IF

SetPointer(Arrow!)				
end event

type pb_salir from w_para_informes`pb_salir within w_info_cajas_eliminadas_packing
integer x = 3465
integer y = 1384
integer taborder = 140
end type

type st_1 from statictext within w_info_cajas_eliminadas_packing
integer x = 329
integer y = 764
integer width = 238
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

type st_2 from statictext within w_info_cajas_eliminadas_packing
integer x = 329
integer y = 1236
integer width = 229
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 553648127
string text = "Inicio"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type em_desde from editmask within w_info_cajas_eliminadas_packing
integer x = 640
integer y = 1220
integer width = 375
integer height = 96
integer taborder = 50
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

event modified;istr_mant.argumento[9]	=	This.Text
id_fecini					= 	Date(this.Text)
end event

type dw_cliente from datawindow within w_info_cajas_eliminadas_packing
integer x = 635
integer y = 556
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
	idwc_productor.Retrieve()	
	idwc_etiqueta.Retrieve()
	ii_cliente = integer(data)
ELSE
	This.SetItem(1, "clie_codigo", ls_null)
	RETURN 1
	SetNull(ii_cliente)
END IF
end event

event itemerror;RETURN 1
end event

type st_6 from statictext within w_info_cajas_eliminadas_packing
integer x = 329
integer y = 568
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

type dw_planta from datawindow within w_info_cajas_eliminadas_packing
integer x = 635
integer y = 776
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

li_Cliente	=	Integer(ii_cliente)

IF ExistePlanta(li_Cliente, Integer(data), ls_Columna[]) THEN
	istr_mant.argumento[3]	=	data
	ii_planta 					= 	Integer(data)
ELSE
	This.SetItem(1, "plde_codigo", ls_null)
	RETURN 1
	SetNull(ii_planta)
END IF
end event

event itemerror;RETURN 1
end event

type st_3 from statictext within w_info_cajas_eliminadas_packing
integer x = 329
integer y = 980
integer width = 270
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

type st_7 from statictext within w_info_cajas_eliminadas_packing
integer x = 1061
integer y = 1236
integer width = 279
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

type em_hasta from editmask within w_info_cajas_eliminadas_packing
integer x = 1326
integer y = 1224
integer width = 375
integer height = 96
integer taborder = 60
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

event modified;istr_mant.argumento[10]	=	This.Text
id_fecter					= 	Date(this.Text)
end event

type st_variedad from statictext within w_info_cajas_eliminadas_packing
integer x = 1838
integer y = 572
integer width = 302
integer height = 96
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

type st_embalaje from statictext within w_info_cajas_eliminadas_packing
integer x = 1838
integer y = 788
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

type cbx_embalaje from checkbox within w_info_cajas_eliminadas_packing
integer x = 2181
integer y = 696
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
	dw_embalaje.Enabled			=	False
	istr_mant.argumento[6]		=	'Z'
	istr_mant.argumento[16]		=	'0'
	dw_embalaje.Object.emba_codigo.BackGround.Color	=	RGB(192, 192, 192)
	is_embalaje						= '-1'
ELSE
	cbx_consembalaje.Enabled	=	False
	cbx_consembalaje.Checked	=	False
	dw_embalaje.Enabled			=	True
	dw_embalaje.Object.emba_codigo.BackGround.Color	=	RGB(255, 255, 255)
	dw_embalaje.SetFocus()
	SetNull(is_embalaje)
END IF
end event

type cbx_consembalaje from checkbox within w_info_cajas_eliminadas_packing
integer x = 2697
integer y = 696
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
string text = "Consolidados"
end type

event clicked;IF This.Checked = TRUE THEN
	istr_mant.argumento[16]	=	'1'
	is_embalaje					= '-9'
ELSE
	istr_mant.argumento[16]	=	'0'
	is_embalaje					= '-1'
END IF

end event

type st_calidad from statictext within w_info_cajas_eliminadas_packing
integer x = 1838
integer y = 988
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

type em_calidad from editmask within w_info_cajas_eliminadas_packing
integer x = 2181
integer y = 976
integer width = 297
integer height = 96
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
		istr_mant.argumento[8]	=	 iuo_calibre.calibre
		em_calidad.Text			=	 iuo_calibre.calibre
		is_calibre 					=   iuo_calibre.calibre
		Return 1
	END IF	
END IF	





end event

type cbx_calidad from checkbox within w_info_cajas_eliminadas_packing
integer x = 2181
integer y = 892
integer width = 297
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
string text = "Todas"
boolean checked = true
end type

event clicked;IF This.Checked THEN
	cbx_conscalidad.Enabled	=	True
	em_calidad.Text			=	''
	em_calidad.Enabled		=	False
	istr_mant.argumento[8]		=	'Z'
	istr_mant.argumento[18]		=	'0'
	is_calibre						= '-1'
ELSE
	cbx_conscalidad.Enabled	=	False
	cbx_conscalidad.Checked	=	False
	em_calidad.Enabled		=	True
	em_calidad.SetFocus()
	SetNull(is_calibre)
END IF


end event

type cbx_conscalidad from checkbox within w_info_cajas_eliminadas_packing
integer x = 2697
integer y = 892
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
end type

event clicked;IF This.Checked = TRUE THEN
	istr_mant.argumento[18]	=	'1'
	is_calibre 					= '-9'
ELSE
	istr_mant.argumento[18]	=	'0'
	is_calibre 					= '-1'
END IF

end event

type cbx_planta from checkbox within w_info_cajas_eliminadas_packing
integer x = 635
integer y = 692
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
	dw_planta.Enabled											=	False
	dw_planta.Object.plde_codigo.BackGround.Color	=	RGB(192, 192, 192)
	istr_mant.argumento[3]									=	'0'
	istr_mant.argumento[13]									=	'0'
	ii_planta 													= 	-1
ELSE
	dw_planta.Enabled											=	True
	dw_planta.Object.plde_codigo.BackGround.Color	=	RGB(255, 255, 255)
	SetNull(ii_planta)
	dw_planta.SetFocus()
END IF
end event

type dw_embalaje from datawindow within w_info_cajas_eliminadas_packing
integer x = 2181
integer y = 772
integer width = 1143
integer height = 104
integer taborder = 100
boolean bringtotop = true
string dataobject = "dddw_embalajes"
boolean border = false
boolean livescroll = true
end type

event itemchanged;is_embalaje 				=	data
end event

event itemerror;RETURN 1
end event

type dw_packing from datawindow within w_info_cajas_eliminadas_packing
integer x = 2181
integer y = 1124
integer width = 965
integer height = 100
integer taborder = 120
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_plantadesp_tipo"
boolean border = false
boolean livescroll = true
end type

event itemchanged;ii_Packing	=	Integer(Data)
//String	 ls_null
//SetNull(ls_null)
//
//	IF ExistePacking(Integer(data))THEN
//		istr_mant.argumento[7]	=	data		
//		RETURN 0
//	ELSE
//		This.SetItem(1, "plde_codigo", ls_null)
//		RETURN 1
//	END IF
end event

type st_5 from statictext within w_info_cajas_eliminadas_packing
integer x = 1838
integer y = 1140
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

type cbx_cajas from checkbox within w_info_cajas_eliminadas_packing
integer x = 1138
integer y = 1432
integer width = 539
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
string text = "Por Nº Caja"
end type

type gb_4 from groupbox within w_info_cajas_eliminadas_packing
integer x = 955
integer y = 1788
integer width = 631
integer height = 128
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
end type

type gb_3 from groupbox within w_info_cajas_eliminadas_packing
integer x = 274
integer y = 1148
integer width = 1490
integer height = 200
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 553648127
string text = "Fecha Embalaje"
end type

type st_4 from statictext within w_info_cajas_eliminadas_packing
integer x = 238
integer y = 440
integer width = 1573
integer height = 952
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

type st_9 from statictext within w_info_cajas_eliminadas_packing
integer x = 1806
integer y = 440
integer width = 1586
integer height = 952
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

type st_10 from statictext within w_info_cajas_eliminadas_packing
integer x = 238
integer y = 1396
integer width = 3154
integer height = 164
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

type uo_selespecie from uo_seleccion_especie within w_info_cajas_eliminadas_packing
event destroy ( )
integer x = 635
integer y = 904
integer height = 180
integer taborder = 40
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
		uo_selvariedad.dw_Seleccion.Enabled		=	True
END CHOOSE
end event

type uo_selvariedad from uo_seleccion_variedad within w_info_cajas_eliminadas_packing
event destroy ( )
integer x = 2181
integer y = 484
integer taborder = 80
boolean bringtotop = true
end type

on uo_selvariedad.destroy
call uo_seleccion_variedad::destroy
end on

type cbx_varirotula from checkbox within w_info_cajas_eliminadas_packing
integer x = 1911
integer y = 1432
integer width = 654
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

type st_12 from statictext within w_info_cajas_eliminadas_packing
integer x = 1838
integer y = 1288
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
string text = "Nro.Proceso"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type em_numero from editmask within w_info_cajas_eliminadas_packing
integer x = 2304
integer y = 1276
integer width = 466
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
long backcolor = 16777215
alignment alignment = center!
borderstyle borderstyle = stylelowered!
string mask = "########"
end type

