$PBExportHeader$w_info_existencia_sitio_revusda.srw
$PBExportComments$Informe Existencia Sitio Revisión
forward
global type w_info_existencia_sitio_revusda from w_para_informes
end type
type gb_4 from groupbox within w_info_existencia_sitio_revusda
end type
type st_1 from statictext within w_info_existencia_sitio_revusda
end type
type st_2 from statictext within w_info_existencia_sitio_revusda
end type
type em_desde from editmask within w_info_existencia_sitio_revusda
end type
type dw_cliente from datawindow within w_info_existencia_sitio_revusda
end type
type st_6 from statictext within w_info_existencia_sitio_revusda
end type
type dw_planta from datawindow within w_info_existencia_sitio_revusda
end type
type st_3 from statictext within w_info_existencia_sitio_revusda
end type
type st_7 from statictext within w_info_existencia_sitio_revusda
end type
type em_hasta from editmask within w_info_existencia_sitio_revusda
end type
type st_8 from statictext within w_info_existencia_sitio_revusda
end type
type dw_productor from datawindow within w_info_existencia_sitio_revusda
end type
type gb_3 from groupbox within w_info_existencia_sitio_revusda
end type
type st_variedad from statictext within w_info_existencia_sitio_revusda
end type
type st_embalaje from statictext within w_info_existencia_sitio_revusda
end type
type cbx_embalaje from checkbox within w_info_existencia_sitio_revusda
end type
type cbx_consembalaje from checkbox within w_info_existencia_sitio_revusda
end type
type st_calidad from statictext within w_info_existencia_sitio_revusda
end type
type em_calidad from editmask within w_info_existencia_sitio_revusda
end type
type cbx_calidad from checkbox within w_info_existencia_sitio_revusda
end type
type cbx_conscalidad from checkbox within w_info_existencia_sitio_revusda
end type
type cbx_productor from checkbox within w_info_existencia_sitio_revusda
end type
type cbx_productorcons from checkbox within w_info_existencia_sitio_revusda
end type
type cbx_planta from checkbox within w_info_existencia_sitio_revusda
end type
type dw_envase from datawindow within w_info_existencia_sitio_revusda
end type
type st_envase from statictext within w_info_existencia_sitio_revusda
end type
type cbx_envase from checkbox within w_info_existencia_sitio_revusda
end type
type dw_embalaje from datawindow within w_info_existencia_sitio_revusda
end type
type st_9 from statictext within w_info_existencia_sitio_revusda
end type
type cbx_todosesta from checkbox within w_info_existencia_sitio_revusda
end type
type dw_stat from datawindow within w_info_existencia_sitio_revusda
end type
type st_12 from statictext within w_info_existencia_sitio_revusda
end type
type dw_destino from datawindow within w_info_existencia_sitio_revusda
end type
type st_5 from statictext within w_info_existencia_sitio_revusda
end type
type cbx_todosdesti from checkbox within w_info_existencia_sitio_revusda
end type
type rb_sitio from radiobutton within w_info_existencia_sitio_revusda
end type
type rb_frigorifico from radiobutton within w_info_existencia_sitio_revusda
end type
type cbx_cajas from checkbox within w_info_existencia_sitio_revusda
end type
type gb_5 from groupbox within w_info_existencia_sitio_revusda
end type
type uo_selespecie from uo_seleccion_especie within w_info_existencia_sitio_revusda
end type
type uo_selvariedad from uo_seleccion_variedad within w_info_existencia_sitio_revusda
end type
type cbx_varirotula from checkbox within w_info_existencia_sitio_revusda
end type
type rb_prefrio from radiobutton within w_info_existencia_sitio_revusda
end type
type gb_6 from groupbox within w_info_existencia_sitio_revusda
end type
type st_10 from statictext within w_info_existencia_sitio_revusda
end type
type st_4 from statictext within w_info_existencia_sitio_revusda
end type
end forward

global type w_info_existencia_sitio_revusda from w_para_informes
integer x = 14
integer y = 32
integer width = 3899
integer height = 2096
string title = "EXISTENCIA NO DISPONIBLE"
boolean minbox = false
boolean maxbox = false
boolean resizable = false
windowtype windowtype = response!
string icon = "F:\Desarrollo\Producción\FrutaProcesada\Producc.ico"
gb_4 gb_4
st_1 st_1
st_2 st_2
em_desde em_desde
dw_cliente dw_cliente
st_6 st_6
dw_planta dw_planta
st_3 st_3
st_7 st_7
em_hasta em_hasta
st_8 st_8
dw_productor dw_productor
gb_3 gb_3
st_variedad st_variedad
st_embalaje st_embalaje
cbx_embalaje cbx_embalaje
cbx_consembalaje cbx_consembalaje
st_calidad st_calidad
em_calidad em_calidad
cbx_calidad cbx_calidad
cbx_conscalidad cbx_conscalidad
cbx_productor cbx_productor
cbx_productorcons cbx_productorcons
cbx_planta cbx_planta
dw_envase dw_envase
st_envase st_envase
cbx_envase cbx_envase
dw_embalaje dw_embalaje
st_9 st_9
cbx_todosesta cbx_todosesta
dw_stat dw_stat
st_12 st_12
dw_destino dw_destino
st_5 st_5
cbx_todosdesti cbx_todosdesti
rb_sitio rb_sitio
rb_frigorifico rb_frigorifico
cbx_cajas cbx_cajas
gb_5 gb_5
uo_selespecie uo_selespecie
uo_selvariedad uo_selvariedad
cbx_varirotula cbx_varirotula
rb_prefrio rb_prefrio
gb_6 gb_6
st_10 st_10
st_4 st_4
end type
global w_info_existencia_sitio_revusda w_info_existencia_sitio_revusda

type variables
str_busqueda istr_busq
str_mant istr_mant

DataWindowChild	idwc_tipopro, idwc_cliente, idwc_planta, idwc_productor, idwc_pesoneto, &
						idwc_etiqueta, idwc_packing, idwc_zonas, idwc_envases, idwc_stat, dwc_mercado						
						
String 				is_NomPlanta, is_embalaje, is_calibre, is_Cliente
Integer 				ii_cliente, ii_planta, ii_destino, ii_status, &
		 				ii_envate, ii_envaec, ii_condicion, ii_inspeccion, ii_Estado, ii_cajas
Date 					id_fecini, id_fecter
Long					ii_productor
				

uo_seleccion_especie		iuo_selespecie
uo_seleccion_variedad	iuo_selvariedad
uo_calibre					iuo_calibre
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

on w_info_existencia_sitio_revusda.create
int iCurrent
call super::create
this.gb_4=create gb_4
this.st_1=create st_1
this.st_2=create st_2
this.em_desde=create em_desde
this.dw_cliente=create dw_cliente
this.st_6=create st_6
this.dw_planta=create dw_planta
this.st_3=create st_3
this.st_7=create st_7
this.em_hasta=create em_hasta
this.st_8=create st_8
this.dw_productor=create dw_productor
this.gb_3=create gb_3
this.st_variedad=create st_variedad
this.st_embalaje=create st_embalaje
this.cbx_embalaje=create cbx_embalaje
this.cbx_consembalaje=create cbx_consembalaje
this.st_calidad=create st_calidad
this.em_calidad=create em_calidad
this.cbx_calidad=create cbx_calidad
this.cbx_conscalidad=create cbx_conscalidad
this.cbx_productor=create cbx_productor
this.cbx_productorcons=create cbx_productorcons
this.cbx_planta=create cbx_planta
this.dw_envase=create dw_envase
this.st_envase=create st_envase
this.cbx_envase=create cbx_envase
this.dw_embalaje=create dw_embalaje
this.st_9=create st_9
this.cbx_todosesta=create cbx_todosesta
this.dw_stat=create dw_stat
this.st_12=create st_12
this.dw_destino=create dw_destino
this.st_5=create st_5
this.cbx_todosdesti=create cbx_todosdesti
this.rb_sitio=create rb_sitio
this.rb_frigorifico=create rb_frigorifico
this.cbx_cajas=create cbx_cajas
this.gb_5=create gb_5
this.uo_selespecie=create uo_selespecie
this.uo_selvariedad=create uo_selvariedad
this.cbx_varirotula=create cbx_varirotula
this.rb_prefrio=create rb_prefrio
this.gb_6=create gb_6
this.st_10=create st_10
this.st_4=create st_4
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.gb_4
this.Control[iCurrent+2]=this.st_1
this.Control[iCurrent+3]=this.st_2
this.Control[iCurrent+4]=this.em_desde
this.Control[iCurrent+5]=this.dw_cliente
this.Control[iCurrent+6]=this.st_6
this.Control[iCurrent+7]=this.dw_planta
this.Control[iCurrent+8]=this.st_3
this.Control[iCurrent+9]=this.st_7
this.Control[iCurrent+10]=this.em_hasta
this.Control[iCurrent+11]=this.st_8
this.Control[iCurrent+12]=this.dw_productor
this.Control[iCurrent+13]=this.gb_3
this.Control[iCurrent+14]=this.st_variedad
this.Control[iCurrent+15]=this.st_embalaje
this.Control[iCurrent+16]=this.cbx_embalaje
this.Control[iCurrent+17]=this.cbx_consembalaje
this.Control[iCurrent+18]=this.st_calidad
this.Control[iCurrent+19]=this.em_calidad
this.Control[iCurrent+20]=this.cbx_calidad
this.Control[iCurrent+21]=this.cbx_conscalidad
this.Control[iCurrent+22]=this.cbx_productor
this.Control[iCurrent+23]=this.cbx_productorcons
this.Control[iCurrent+24]=this.cbx_planta
this.Control[iCurrent+25]=this.dw_envase
this.Control[iCurrent+26]=this.st_envase
this.Control[iCurrent+27]=this.cbx_envase
this.Control[iCurrent+28]=this.dw_embalaje
this.Control[iCurrent+29]=this.st_9
this.Control[iCurrent+30]=this.cbx_todosesta
this.Control[iCurrent+31]=this.dw_stat
this.Control[iCurrent+32]=this.st_12
this.Control[iCurrent+33]=this.dw_destino
this.Control[iCurrent+34]=this.st_5
this.Control[iCurrent+35]=this.cbx_todosdesti
this.Control[iCurrent+36]=this.rb_sitio
this.Control[iCurrent+37]=this.rb_frigorifico
this.Control[iCurrent+38]=this.cbx_cajas
this.Control[iCurrent+39]=this.gb_5
this.Control[iCurrent+40]=this.uo_selespecie
this.Control[iCurrent+41]=this.uo_selvariedad
this.Control[iCurrent+42]=this.cbx_varirotula
this.Control[iCurrent+43]=this.rb_prefrio
this.Control[iCurrent+44]=this.gb_6
this.Control[iCurrent+45]=this.st_10
this.Control[iCurrent+46]=this.st_4
end on

on w_info_existencia_sitio_revusda.destroy
call super::destroy
destroy(this.gb_4)
destroy(this.st_1)
destroy(this.st_2)
destroy(this.em_desde)
destroy(this.dw_cliente)
destroy(this.st_6)
destroy(this.dw_planta)
destroy(this.st_3)
destroy(this.st_7)
destroy(this.em_hasta)
destroy(this.st_8)
destroy(this.dw_productor)
destroy(this.gb_3)
destroy(this.st_variedad)
destroy(this.st_embalaje)
destroy(this.cbx_embalaje)
destroy(this.cbx_consembalaje)
destroy(this.st_calidad)
destroy(this.em_calidad)
destroy(this.cbx_calidad)
destroy(this.cbx_conscalidad)
destroy(this.cbx_productor)
destroy(this.cbx_productorcons)
destroy(this.cbx_planta)
destroy(this.dw_envase)
destroy(this.st_envase)
destroy(this.cbx_envase)
destroy(this.dw_embalaje)
destroy(this.st_9)
destroy(this.cbx_todosesta)
destroy(this.dw_stat)
destroy(this.st_12)
destroy(this.dw_destino)
destroy(this.st_5)
destroy(this.cbx_todosdesti)
destroy(this.rb_sitio)
destroy(this.rb_frigorifico)
destroy(this.cbx_cajas)
destroy(this.gb_5)
destroy(this.uo_selespecie)
destroy(this.uo_selvariedad)
destroy(this.cbx_varirotula)
destroy(this.rb_prefrio)
destroy(this.gb_6)
destroy(this.st_10)
destroy(this.st_4)
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

dw_planta.GetChild("plde_codigo", idwc_planta)
idwc_planta.SetTransObject(sqlca)
idwc_planta.Retrieve(1)
dw_planta.InsertRow(0)

dw_planta.Enabled											=	False
dw_planta.Object.plde_codigo.BackGround.Color	=	RGB(166,180,210)

dw_productor.GetChild("prod_codigo", idwc_productor)
idwc_productor.SetTransObject(sqlca)
idwc_productor.Retrieve(-1)
dw_productor.InsertRow(0)

dw_productor.Enabled											=	False
dw_productor.Object.prod_codigo.BackGround.Color	=	RGB(166,180,210)

dw_envase.enabled 											=	False
dw_envase.Object.enva_codigo.BackGround.Color		=	RGB(166,180,210)

dw_embalaje.enabled 											=	False
dw_embalaje.Object.emba_codigo.BackGround.Color		=	RGB(166,180,210)

//dw_especie.Enabled											=	False
//dw_especie.Object.espe_codigo.BackGround.Color		=	RGB(166,180,210)
//	
//dw_especie.GetChild("espe_codigo", idwc_especie)
//idwc_especie.SetTransObject(sqlca)
//idwc_especie.Retrieve()
//dw_especie.InsertRow(0)
////dw_especie.SetItem(1, "espe_codigo", gi_CodEspecie)
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

dw_envase.getChild("enva_codigo", idwc_envases)
idwc_envases.SetTransObject(SQLCA)
idwc_envases.Retrieve()
dw_envase.InsertRow(0)

dw_embalaje.Enabled		=	False
dw_embalaje.SetTransObject(SQLCA)
dw_embalaje.Retrieve()
dw_embalaje.InsertRow(0)

dw_stat.GetChild("stat_codigo", idwc_stat)
idwc_stat.SetTransObject(SQLCA)
idwc_stat.Retrieve()
dw_stat.InsertRow(0)
	
dw_stat.Object.stat_codigo.background.color = RGB(166,180,210)

dw_destino.GetChild("dest_codigo",dwc_mercado)
dwc_mercado.SetTransObject(Sqlca)
dwc_mercado.Retrieve(0)
dw_destino.SetTransObject(Sqlca)
dw_destino.InsertRow(0)

dw_destino.Object.dest_codigo.background.color = RGB(166,180,210)
	
em_desde.Text				=	String(RelativeDate(Today(), -365))
em_hasta.Text				=	String(Today())
ii_cajas = 1

ii_cliente 					= 	gi_CodExport
ii_planta					= 	-1
ii_productor				= 	-1
ii_envate					=	-1
ii_envaec					=	-1
ii_condicion				=	-1
ii_inspeccion				=	-1
id_fecini					=	RelativeDate(Today(), -365)
id_fecter					=	Today()
is_embalaje					=	'-1'
is_calibre					=	'-1'
ii_destino					=	-1
ii_status					= 	-1
ii_Estado					=  4
end event

event resize;//
end event

type st_computador from w_para_informes`st_computador within w_info_existencia_sitio_revusda
end type

type st_usuario from w_para_informes`st_usuario within w_info_existencia_sitio_revusda
end type

type st_temporada from w_para_informes`st_temporada within w_info_existencia_sitio_revusda
end type

type p_logo from w_para_informes`p_logo within w_info_existencia_sitio_revusda
end type

type st_titulo from w_para_informes`st_titulo within w_info_existencia_sitio_revusda
integer width = 3150
string text = "Existencia No Disponible"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_existencia_sitio_revusda
integer x = 3529
integer y = 1392
integer taborder = 150
end type

event pb_acepta::clicked;SetPointer(Arrow!)

Integer	fila, li_varirotula
String	texto_desde, texto_hasta, texto_fecha, ls_null, ls_Titulo

SetNull(ls_null)

IF rb_sitio.checked=True THEN
	istr_info.titulo	= 'EXISTENCIA SITIO REVISION'	
	ls_Titulo			= 'Existencia Sitio Revisión'	
END IF

IF rb_frigorifico.checked=True THEN
	istr_info.titulo	= 'EXISTENCIA SITIO FRIGORIFICO'	
	ls_Titulo			= 'Existencia Sitio Frigorifico'	
END IF

IF rb_PreFrio.checked=True THEN
	istr_info.titulo	= 'EXISTENCIA EN PREFRIO'	
	ls_Titulo			= 'Existencia En PreFrio'	
END IF

IF cbx_cajas.Checked THEN
	ii_cajas = 1
ELSE
	ii_cajas = 2
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

BuscaDesCliente(ii_cliente)

OpenWithParm(vinf, istr_info)
vinf.dw_1.DataObject = "dw_info_existencia_sitio_revusda"

texto_desde		=  f_fecha_texto(String(id_fecini), 1)
texto_hasta		=	f_fecha_texto(String(id_fecter), 1)
texto_fecha		=	"Desde El :  " + texto_desde + "   Hasta El :  " + texto_hasta

vinf.dw_1.SetTransObject(sqlca)

IF IsNull(ii_cliente) OR IsNull(ii_planta) OR IsNull(ii_productor) OR  IsNull(ii_envate) OR &
	IsNull(ii_envaec) OR IsNull(is_embalaje) OR IsNull(is_calibre) OR IsNull(id_fecini) OR &
	IsNull(id_fecter) OR IsNull(ii_condicion) OR IsNull(ii_inspeccion) OR IsNull(ii_status) OR &
	IsNull(ii_destino) THEN
		MessageBox("Error", "Debe Ingresar todos los parametros para el informe", StopSign!)
		return 1
END IF

fila	=	vinf.dw_1.Retrieve(ii_cliente, ii_planta, ii_productor, uo_selespecie.Codigo,&
								    uo_selvariedad.Codigo, ii_envate, ii_envaec, is_embalaje, &
									 is_calibre, id_fecini, id_fecter, ii_condicion, ii_inspeccion,&
									 ii_destino,ii_status,ii_Estado,ii_cajas,li_varirotula)

IF fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
			   	"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", &
	             StopSign!, Ok!)
ELSE
	   F_Membrete(vinf.dw_1)
		vinf.dw_1.Modify("fechas.text = '" + texto_fecha + "'")
		vinf.dw_1.Modify("t_cliente.text = '" + is_Cliente + "'")		
		vinf.dw_1.Modify("titulo_informe.text = '" +ls_Titulo+"'")
	IF gs_Ambiente <> 'Windows' THEN
		F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
	END IF
END IF

SetPointer(Arrow!)				
end event

type pb_salir from w_para_informes`pb_salir within w_info_existencia_sitio_revusda
integer x = 3529
integer y = 1668
integer taborder = 160
end type

type gb_4 from groupbox within w_info_existencia_sitio_revusda
integer x = 297
integer y = 1404
integer width = 430
integer height = 184
integer taborder = 130
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
end type

type st_1 from statictext within w_info_existencia_sitio_revusda
integer x = 343
integer y = 692
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

type st_2 from statictext within w_info_existencia_sitio_revusda
integer x = 343
integer y = 1276
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

type em_desde from editmask within w_info_existencia_sitio_revusda
integer x = 654
integer y = 1260
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

event modified;istr_mant.argumento[9]	=	This.Text
id_fecini					= 	Date(this.Text)
end event

type dw_cliente from datawindow within w_info_existencia_sitio_revusda
integer x = 649
integer y = 512
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
	idwc_etiqueta.Retrieve()
	ii_cliente = integer(data)
	
	dw_productor.GetChild("prod_codigo", idwc_productor)
	idwc_productor.SetTransObject(sqlca)
	idwc_productor.Retrieve(Integer(data))
	dw_productor.InsertRow(0)
ELSE
	This.SetItem(1, "clie_codigo", ls_null)
	RETURN 1
	SetNull(ii_cliente)
END IF
end event

event itemerror;RETURN 1
end event

type st_6 from statictext within w_info_existencia_sitio_revusda
integer x = 343
integer y = 532
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

type dw_planta from datawindow within w_info_existencia_sitio_revusda
integer x = 649
integer y = 684
integer width = 969
integer height = 92
integer taborder = 30
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

type st_3 from statictext within w_info_existencia_sitio_revusda
integer x = 343
integer y = 1068
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

type st_7 from statictext within w_info_existencia_sitio_revusda
integer x = 1070
integer y = 1276
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

type em_hasta from editmask within w_info_existencia_sitio_revusda
integer x = 1344
integer y = 1260
integer width = 375
integer height = 96
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
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
end type

event modified;istr_mant.argumento[10]	=	This.Text
id_fecter					= 	Date(this.Text)
end event

type st_8 from statictext within w_info_existencia_sitio_revusda
integer x = 343
integer y = 884
integer width = 329
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

type dw_productor from datawindow within w_info_existencia_sitio_revusda
integer x = 649
integer y = 872
integer width = 1138
integer height = 92
integer taborder = 40
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
	istr_mant.argumento[4]	=	data	
	ii_productor 				= 	Integer(data)
	RETURN 0
ELSE
	This.SetItem(1, "prod_codigo", Long(ls_null))
	RETURN 1
	SetNull(ii_productor)
END IF
end event

type gb_3 from groupbox within w_info_existencia_sitio_revusda
integer x = 297
integer y = 1188
integer width = 1490
integer height = 204
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 553648127
string text = "Fecha Embalaje"
end type

type st_variedad from statictext within w_info_existencia_sitio_revusda
integer x = 1851
integer y = 552
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

type st_embalaje from statictext within w_info_existencia_sitio_revusda
integer x = 1851
integer y = 928
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

type cbx_embalaje from checkbox within w_info_existencia_sitio_revusda
integer x = 2194
integer y = 836
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
	dw_embalaje.Object.emba_codigo.BackGround.Color	=	RGB(166,180,210)
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

type cbx_consembalaje from checkbox within w_info_existencia_sitio_revusda
integer x = 2711
integer y = 836
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

type st_calidad from statictext within w_info_existencia_sitio_revusda
integer x = 1851
integer y = 1140
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

type em_calidad from editmask within w_info_existencia_sitio_revusda
integer x = 2194
integer y = 1124
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
textcase textcase = upper!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = stringmask!
string mask = "xxx"
end type

event modified;Integer	li_cliente,	li_especie, li_variedad, li_cantid
String	ls_calibre

IF This.Text <> '' THEN
	li_especie	=	Integer(uo_selespecie.Codigo) // Especie
	li_variedad	=	Integer(uo_selvariedad.Codigo) // Variedad
	ls_calibre	=	This.Text
	is_calibre  =  ''
	IF NOT iuo_calibre.existe(li_especie,li_variedad,ls_calibre,True,sqlca) THEN
		em_calidad.Text = ''
		This.SetFocus()
		Return 1
	ELSE	
		istr_mant.argumento[7]	 = iuo_calibre.calibre
		em_calidad.Text			 = iuo_calibre.calibre
		is_calibre				    = iuo_calibre.calibre
		Return 1
	END IF	
END IF	
end event

type cbx_calidad from checkbox within w_info_existencia_sitio_revusda
integer x = 2194
integer y = 1044
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

type cbx_conscalidad from checkbox within w_info_existencia_sitio_revusda
integer x = 2711
integer y = 1044
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

type cbx_productor from checkbox within w_info_existencia_sitio_revusda
integer x = 649
integer y = 788
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
	cbx_productorcons.Enabled									=	True
	dw_productor.Enabled											=	False
	dw_productor.Object.prod_codigo.BackGround.Color	=	RGB(166,180,210)
	istr_mant.argumento[4]										=	'0'
	istr_mant.argumento[14]										=	'0'
	ii_productor													= 	-1
ELSE
	cbx_productorcons.Enabled									=	False
	cbx_productorcons.Checked									=	False
	dw_productor.Enabled											=	True
	dw_productor.Object.prod_codigo.BackGround.Color	=	RGB(255, 255, 255)
	dw_productor.SetFocus()
	istr_mant.argumento[14]	=	'0'
	SetNull(ii_productor)
END IF
end event

type cbx_productorcons from checkbox within w_info_existencia_sitio_revusda
integer x = 1129
integer y = 788
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
	istr_mant.argumento[14]	=	'1'
	ii_productor				= 	-9
ELSE
	istr_mant.argumento[14]	=	'0'
	ii_productor				= 	-1
END IF
	
end event

type cbx_planta from checkbox within w_info_existencia_sitio_revusda
integer x = 649
integer y = 608
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
	dw_planta.Object.plde_codigo.BackGround.Color	=	RGB(166,180,210)
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

type dw_envase from datawindow within w_info_existencia_sitio_revusda
integer x = 2194
integer y = 724
integer width = 1143
integer height = 104
integer taborder = 10
boolean bringtotop = true
string dataobject = "dddw_envases_1"
boolean border = false
boolean livescroll = true
end type

event itemchanged;String				ls_Columna[], ls_null, ls_filtro
DataWindowChild 	ldwc_Embalajes
SetNull(ls_null)


ii_EnvaTE	= 	Integer(left(data, 2))
ii_EnvaEC	= 	Integer(mid(data, 6,2))

dw_embalaje.Reset()
dw_embalaje.GetChild("emba_codigo", ldwc_Embalajes)
ldwc_Embalajes.SetTransObject(SQLCA)
ldwc_Embalajes.Retrieve()
ls_filtro = "enva_tipoen = " + String(ii_envate) + " and enva_codigo = " + String(ii_envaec)
ldwc_embalajes.SetFilter(ls_filtro)
ldwc_embalajes.Filter()
dw_embalaje.InsertRow(0)
end event

event itemerror;RETURN 1
end event

type st_envase from statictext within w_info_existencia_sitio_revusda
integer x = 1851
integer y = 744
integer width = 247
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 553648127
string text = "Envases"
boolean focusrectangle = false
end type

type cbx_envase from checkbox within w_info_existencia_sitio_revusda
integer x = 2194
integer y = 644
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
	dw_envase.Enabled			=	False
	dw_envase.Object.enva_codigo.BackGround.Color	=	RGB(166,180,210)
	istr_mant.argumento[6]		=	'Z'
	istr_mant.argumento[16]		=	'0'
	ii_envate						= 	-1
	ii_envaec						= 	-1
ELSE
	dw_envase.Enabled			=	True
	dw_envase.Object.enva_codigo.BackGround.Color	=	RGB(255, 255, 255)
	SetNull(ii_envate)
	SetNull(ii_envaec)
END IF
end event

type dw_embalaje from datawindow within w_info_existencia_sitio_revusda
integer x = 2194
integer y = 912
integer width = 1143
integer height = 104
integer taborder = 20
boolean bringtotop = true
string dataobject = "dddw_embalajes"
boolean border = false
boolean livescroll = true
end type

event itemchanged;is_embalaje 				=	data
end event

event itemerror;RETURN 1
end event

type st_9 from statictext within w_info_existencia_sitio_revusda
integer x = 1819
integer y = 440
integer width = 1586
integer height = 1208
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

type cbx_todosesta from checkbox within w_info_existencia_sitio_revusda
integer x = 2199
integer y = 1256
integer width = 471
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
string text = "Todos"
boolean checked = true
end type

event clicked;IF This.checked THEN
	dw_stat.enabled = FALSE
	dw_stat.Object.stat_codigo.background.color = RGB(166,180,210)
	dw_stat.Reset()
	dw_stat.GetChild("stat_codigo", idwc_stat)
	idwc_stat.SetTransObject(sqlca)
	idwc_stat.Retrieve()
	dw_stat.InsertRow(0)
	dw_stat.SetFocus()
	ii_status = 	-1
ELSE
	dw_stat.enabled = TRUE
	dw_stat.Object.stat_codigo.background.color = rgb(255,255,255)
	dw_stat.Reset()
	dw_stat.GetChild("stat_codigo", idwc_stat)
	idwc_stat.SetTransObject(sqlca)
	idwc_stat.Retrieve()
	dw_stat.InsertRow(0)
	dw_stat.SetItem(1, "stat_codigo", 1)
	dw_stat.SetFocus()
	ii_status = dw_stat.Object.stat_codigo[1]
END IF	




end event

type dw_stat from datawindow within w_info_existencia_sitio_revusda
integer x = 2190
integer y = 1328
integer width = 1051
integer height = 116
integer taborder = 120
boolean bringtotop = true
boolean enabled = false
string title = "none"
string dataobject = "dddw_status1"
boolean border = false
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event itemchanged;Integer li_Null
SetNull(li_Null)

IF  NoExisteStatus(integer(data)) THEN
	MessageBox("informe","No Existe Status")
	dw_stat.SetItem(1,"stat_codigo",li_Null)
	RETURN 1
ELSE	
	ii_status	= Integer(data)
	istr_mant.argumento[22]	=  f_statnombre(integer(data))
END IF

end event

event itemerror;Return 1
end event

type st_12 from statictext within w_info_existencia_sitio_revusda
integer x = 1851
integer y = 1340
integer width = 215
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 553648127
string text = "Status"
boolean focusrectangle = false
end type

type dw_destino from datawindow within w_info_existencia_sitio_revusda
integer x = 2190
integer y = 1512
integer width = 882
integer height = 92
integer taborder = 70
boolean bringtotop = true
boolean enabled = false
string title = "none"
string dataobject = "dddw_destinos"
boolean border = false
boolean livescroll = true
end type

event itemchanged;Integer li_Null
SetNull(li_Null)

IF noexistedestinos(integer(data)) THEN
	MessageBox("informe","No Existe Destinos")
	dw_destino.SetItem(1,"dest_codigo",li_Null)
	RETURN 1
ELSE	
	ii_destino	= Integer(data)
END IF
end event

event itemerror;Return 1
end event

type st_5 from statictext within w_info_existencia_sitio_revusda
integer x = 1851
integer y = 1524
integer width = 279
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
string text = "Destino "
boolean focusrectangle = false
end type

type cbx_todosdesti from checkbox within w_info_existencia_sitio_revusda
integer x = 2199
integer y = 1436
integer width = 402
integer height = 72
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
string text = "Todos"
boolean checked = true
end type

event clicked;IF This.checked THEN
	dw_destino.enabled = FALSE
	dw_destino.Object.dest_codigo.background.color = RGB(166,180,210)
	dw_destino.Reset()
	dw_destino.GetChild("dest_codigo", dwc_mercado)
	dwc_mercado.SetTransObject(sqlca)
	dwc_mercado.Retrieve(0)
	dw_destino.InsertRow(0)
	dw_destino.SetFocus()
	ii_destino = -1
ELSE
	dw_destino.enabled = TRUE
	dw_destino.Object.dest_codigo.background.color = rgb(255,255,255)
	dw_destino.Reset()
	dw_destino.GetChild("dest_codigo", dwc_mercado)
	dwc_mercado.SetTransObject(sqlca)
	dwc_mercado.Retrieve(0)
	dw_destino.InsertRow(0)
	dw_destino.SetItem(1, "dest_codigo", 99)
	dw_destino.SetFocus()
	ii_destino = dw_destino.Object.dest_codigo[1]
END IF	
end event

type rb_sitio from radiobutton within w_info_existencia_sitio_revusda
integer x = 1527
integer y = 1716
integer width = 453
integer height = 68
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
string text = "Sitio USDA"
end type

event clicked;IF This.checked=True THEN
	ii_Estado	=	8
END IF
end event

type rb_frigorifico from radiobutton within w_info_existencia_sitio_revusda
integer x = 2702
integer y = 1716
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
string text = "USDA Frigorifico"
end type

event clicked;IF This.checked=True THEN
	ii_Estado	=	7
END IF
end event

type cbx_cajas from checkbox within w_info_existencia_sitio_revusda
integer x = 315
integer y = 1476
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
string text = "Por Cajas"
end type

event clicked;IF cbx_cajas.Checked THEN
	ii_cajas = 1
ELSE	
	ii_cajas = 2
END IF	
end event

type gb_5 from groupbox within w_info_existencia_sitio_revusda
integer x = 297
integer y = 1656
integer width = 3058
integer height = 156
integer taborder = 140
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 553648127
end type

type uo_selespecie from uo_seleccion_especie within w_info_existencia_sitio_revusda
event destroy ( )
integer x = 649
integer y = 972
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
		uo_selvariedad.dw_Seleccion.Enabled		=	True
END CHOOSE
end event

type uo_selvariedad from uo_seleccion_variedad within w_info_existencia_sitio_revusda
event destroy ( )
integer x = 2194
integer y = 460
integer taborder = 80
boolean bringtotop = true
end type

on uo_selvariedad.destroy
call uo_seleccion_variedad::destroy
end on

type cbx_varirotula from checkbox within w_info_existencia_sitio_revusda
integer x = 759
integer y = 1476
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

type rb_prefrio from radiobutton within w_info_existencia_sitio_revusda
integer x = 393
integer y = 1716
integer width = 421
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
string text = "En PreFrio"
boolean checked = true
end type

event clicked;IF This.checked=True THEN
	ii_Estado	=	4
END IF
end event

type gb_6 from groupbox within w_info_existencia_sitio_revusda
integer x = 736
integer y = 1404
integer width = 1051
integer height = 184
integer taborder = 140
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
end type

type st_10 from statictext within w_info_existencia_sitio_revusda
integer x = 251
integer y = 1648
integer width = 3154
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

type st_4 from statictext within w_info_existencia_sitio_revusda
integer x = 247
integer y = 440
integer width = 1573
integer height = 1208
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

