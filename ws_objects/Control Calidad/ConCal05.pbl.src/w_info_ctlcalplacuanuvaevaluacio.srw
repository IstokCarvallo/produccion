$PBExportHeader$w_info_ctlcalplacuanuvaevaluacio.srw
$PBExportComments$Ventana de Informe de Evaluación y Calificación.
forward
global type w_info_ctlcalplacuanuvaevaluacio from w_para_informes
end type
type st_1 from statictext within w_info_ctlcalplacuanuvaevaluacio
end type
type em_fechadesde from editmask within w_info_ctlcalplacuanuvaevaluacio
end type
type st_12 from statictext within w_info_ctlcalplacuanuvaevaluacio
end type
type st_13 from statictext within w_info_ctlcalplacuanuvaevaluacio
end type
type em_fechahasta from editmask within w_info_ctlcalplacuanuvaevaluacio
end type
type st_zona from statictext within w_info_ctlcalplacuanuvaevaluacio
end type
type dw_zona from datawindow within w_info_ctlcalplacuanuvaevaluacio
end type
type cbx_todoplanta from checkbox within w_info_ctlcalplacuanuvaevaluacio
end type
type cbx_consplanta from checkbox within w_info_ctlcalplacuanuvaevaluacio
end type
type dw_planta from datawindow within w_info_ctlcalplacuanuvaevaluacio
end type
type st_33 from statictext within w_info_ctlcalplacuanuvaevaluacio
end type
type dw_productor from datawindow within w_info_ctlcalplacuanuvaevaluacio
end type
type cbx_todoprod from checkbox within w_info_ctlcalplacuanuvaevaluacio
end type
type cbx_consprod from checkbox within w_info_ctlcalplacuanuvaevaluacio
end type
type st_2 from statictext within w_info_ctlcalplacuanuvaevaluacio
end type
type dw_agronomo from datawindow within w_info_ctlcalplacuanuvaevaluacio
end type
type dw_variedades from datawindow within w_info_ctlcalplacuanuvaevaluacio
end type
type cbx_todosvar from checkbox within w_info_ctlcalplacuanuvaevaluacio
end type
type cbx_consvariedad from checkbox within w_info_ctlcalplacuanuvaevaluacio
end type
type cbx_todoszona from checkbox within w_info_ctlcalplacuanuvaevaluacio
end type
type cbx_conszonas from checkbox within w_info_ctlcalplacuanuvaevaluacio
end type
type st_3 from statictext within w_info_ctlcalplacuanuvaevaluacio
end type
type cbx_todosagro from checkbox within w_info_ctlcalplacuanuvaevaluacio
end type
type st_5 from statictext within w_info_ctlcalplacuanuvaevaluacio
end type
type dw_packing from datawindow within w_info_ctlcalplacuanuvaevaluacio
end type
type cbx_todospacking from checkbox within w_info_ctlcalplacuanuvaevaluacio
end type
type cbx_conspacking from checkbox within w_info_ctlcalplacuanuvaevaluacio
end type
type cbx_todosfecha from checkbox within w_info_ctlcalplacuanuvaevaluacio
end type
type cbx_consfecha from checkbox within w_info_ctlcalplacuanuvaevaluacio
end type
type st_6 from statictext within w_info_ctlcalplacuanuvaevaluacio
end type
type em_embalaje from editmask within w_info_ctlcalplacuanuvaevaluacio
end type
type cb_buscaembalaje from commandbutton within w_info_ctlcalplacuanuvaevaluacio
end type
type em_descripcion from editmask within w_info_ctlcalplacuanuvaevaluacio
end type
type cbx_todosemb from checkbox within w_info_ctlcalplacuanuvaevaluacio
end type
type cbx_consembalaje from checkbox within w_info_ctlcalplacuanuvaevaluacio
end type
type st_7 from statictext within w_info_ctlcalplacuanuvaevaluacio
end type
type cbx_todoscalibre from checkbox within w_info_ctlcalplacuanuvaevaluacio
end type
type cbx_conscalibre from checkbox within w_info_ctlcalplacuanuvaevaluacio
end type
type em_calibre from editmask within w_info_ctlcalplacuanuvaevaluacio
end type
type gb_4 from groupbox within w_info_ctlcalplacuanuvaevaluacio
end type
type st_8 from statictext within w_info_ctlcalplacuanuvaevaluacio
end type
type cbx_consagro from checkbox within w_info_ctlcalplacuanuvaevaluacio
end type
type gb_5 from groupbox within w_info_ctlcalplacuanuvaevaluacio
end type
type st_44 from statictext within w_info_ctlcalplacuanuvaevaluacio
end type
type st_9 from statictext within w_info_ctlcalplacuanuvaevaluacio
end type
type st_10 from statictext within w_info_ctlcalplacuanuvaevaluacio
end type
type st_11 from statictext within w_info_ctlcalplacuanuvaevaluacio
end type
type dw_especie from datawindow within w_info_ctlcalplacuanuvaevaluacio
end type
type st_4 from statictext within w_info_ctlcalplacuanuvaevaluacio
end type
end forward

global type w_info_ctlcalplacuanuvaevaluacio from w_para_informes
integer x = 14
integer y = 32
integer width = 3045
integer height = 1956
string title = "Calificación y Resolución de Lotes"
boolean minbox = false
boolean maxbox = false
boolean resizable = false
windowtype windowtype = response!
string icon = "F:\Desarrollo\Producción\FrutaProcesada\Producc.ico"
st_1 st_1
em_fechadesde em_fechadesde
st_12 st_12
st_13 st_13
em_fechahasta em_fechahasta
st_zona st_zona
dw_zona dw_zona
cbx_todoplanta cbx_todoplanta
cbx_consplanta cbx_consplanta
dw_planta dw_planta
st_33 st_33
dw_productor dw_productor
cbx_todoprod cbx_todoprod
cbx_consprod cbx_consprod
st_2 st_2
dw_agronomo dw_agronomo
dw_variedades dw_variedades
cbx_todosvar cbx_todosvar
cbx_consvariedad cbx_consvariedad
cbx_todoszona cbx_todoszona
cbx_conszonas cbx_conszonas
st_3 st_3
cbx_todosagro cbx_todosagro
st_5 st_5
dw_packing dw_packing
cbx_todospacking cbx_todospacking
cbx_conspacking cbx_conspacking
cbx_todosfecha cbx_todosfecha
cbx_consfecha cbx_consfecha
st_6 st_6
em_embalaje em_embalaje
cb_buscaembalaje cb_buscaembalaje
em_descripcion em_descripcion
cbx_todosemb cbx_todosemb
cbx_consembalaje cbx_consembalaje
st_7 st_7
cbx_todoscalibre cbx_todoscalibre
cbx_conscalibre cbx_conscalibre
em_calibre em_calibre
gb_4 gb_4
st_8 st_8
cbx_consagro cbx_consagro
gb_5 gb_5
st_44 st_44
st_9 st_9
st_10 st_10
st_11 st_11
dw_especie dw_especie
st_4 st_4
end type
global w_info_ctlcalplacuanuvaevaluacio w_info_ctlcalplacuanuvaevaluacio

type variables
str_busqueda istr_busq
str_mant istr_mant
Integer	ii_tipo, Codigo
String	is_report, nombre

DataWindowChild		idwc_zona, idwc_productores,idwc_planta, idwc_cliente,&
							idwc_variedad, idwc_tecnico, idwc_inspector, idwc_packing,idwc_especie
uo_zonas					iuo_zonas
uo_ctlcalagronomos	iuo_ctlcalagronomos
uo_especie				iuo_especie

end variables

forward prototypes
public function boolean noexisteplanta (integer planta, integer tipo)
public function boolean noexisteembalaje (string embalaje)
public function boolean noexistevariedad (integer variedad)
public subroutine buscaembalaje (integer cliente)
public function boolean noexisteagroproduc (integer ai_agronomo)
public function boolean noexistevariecalibre (integer variedad, string calibre)
public function boolean noexisteproductor (long productor)
end prototypes

public function boolean noexisteplanta (integer planta, integer tipo);Integer li_Contador,li_zona


Select Count(*)
Into :li_Contador
From dba.plantadesp
Where plde_codigo = :planta
and	plde_tipopl	= :Tipo;

IF sqlca.sqlcode = -1 THEN
	F_ErrorBaseDatos(sqlca,"No se pudo leer la tabla Plantas Despachos")
	RETURN TRUE
ELSEIF li_Contador = 0 THEN
	IF Tipo = 1 THEN
		messagebox("Atención","Código Planta No Existe Para la Zona" + &
						"~nSeleccionada, Ingrese Otro Por Favor",Exclamation!)
	ELSE		
		messagebox("Atención","Código Packing No Existe Para la Zona" + &
						"~nSeleccionada, Ingrese Otro Por Favor",Exclamation!)
	END IF					
	RETURN TRUE
ELSE	
	RETURN FALSE	
END IF
	
end function

public function boolean noexisteembalaje (string embalaje);String ls_Nombre,ls_Codigo

ls_codigo	=	''

Select emba_codigo,emba_nombre
Into :ls_Codigo,:ls_Nombre
From dba.embalajes
Where emba_codigo = :embalaje;

IF sqlca.sqlcode = -1 THEN
	F_ErrorBaseDatos(sqlca,"No se pudo leer la tabla Embalajes")
	RETURN TRUE
ELSEIF ls_Codigo = '' THEN
	MessageBox("Atencion","Código de Embalaje no Existe, Ingrese Otro Código",Exclamation!)
	RETURN TRUE
ELSE	
	em_descripcion.text	=	ls_Nombre
	RETURN FALSE	
END IF
	

end function

public function boolean noexistevariedad (integer variedad);Integer li_Contador

Select Count(*)
Into :li_Contador
From dba.variedades
Where vari_codigo = :variedad;

IF sqlca.sqlcode = -1 THEN
	F_ErrorBaseDatos(sqlca,"No se pudo leer la tabla Variedades")
	RETURN TRUE
ELSEIF li_Contador = 0 THEN
	messagebox("Atención","Código Variedad No Existe Para la Zona" + &
					"~n~nSeleccionada, Ingrese Otro Por Favor",Exclamation!)
	RETURN TRUE
ELSE	
	RETURN FALSE	
END IF

end function

public subroutine buscaembalaje (integer cliente);istr_busq.argum[1] = String(gi_CodExport)

OpenWithParm(w_busc_embalajes, istr_busq)

istr_busq	= Message.PowerObjectParm

IF istr_busq.argum[2] <> "" THEN
//	istr_mant.argumento[2] = istr_busq.argum[2]
//	istr_mant.argumento[3] = istr_busq.argum[4]
//	istr_mant.argumento[4] = istr_busq.argum[5]
	em_embalaje.Text	  	  = istr_busq.Argum[2]
	em_descripcion.Text	  = istr_busq.Argum[3]
ELSE
	cb_buscaembalaje.SetFocus()
	
END IF
end subroutine

public function boolean noexisteagroproduc (integer ai_agronomo);Integer li_Contador, li_zona
Long    ll_prod

IF cbx_todoszona.Checked = TRUE THEN
	li_zona = 0
ELSE
	li_zona	=	dw_zona.object.zona_codigo[1]
END IF

IF cbx_todoprod.Checked = TRUE THEN
	ll_prod = 0
ELSE
	ll_prod	=	dw_productor.object.prod_codigo[1]
END IF

SELECT	Count(*)
	INTO	:li_Contador
	FROM	dba.ctlcalagroproduc
	WHERE	ccag_codigo = :ai_agronomo
	AND	:ll_prod in (0,prod_codigo )
	AND	:li_zona in (0,zona_codigo);

IF sqlca.sqlcode = -1 THEN
	F_ErrorBaseDatos(sqlca,"No se pudo leer la tabla Productor")
	RETURN TRUE
ELSEIF li_Contador = 0 THEN
	messagebox("Atención","Código de Agrónomo No Existe Para la Zona" + &
					"~n~nSeleccionada, Ingrese Otro Por Favor",Exclamation!)
	RETURN TRUE
ELSE	
	RETURN FALSE
END IF

end function

public function boolean noexistevariecalibre (integer variedad, string calibre);String ls_Codigo
Integer li_variedad

Calibre	= Calibre + Fill(" ",3 - Len(Calibre))
ls_codigo	=	''



	SELECT Max(vaca_calibr)
		INTO	:ls_Codigo
		FROM	dba.variecalibre
		WHERE espe_codigo =  :gi_CodEspecie
		AND   :li_variedad in (0,vari_codigo)
		AND   vaca_calibr	=	:Calibre;
	
	IF sqlca.sqlcode = -1 THEN
		F_ErrorBaseDatos(sqlca,"No se pudo leer la tabla VarieCalibre")
		RETURN TRUE
	ELSEIF ls_Codigo = '' THEN
		MessageBox("Atencion","Código de Calibre no Existe, Ingrese Otro Código",Exclamation!)
		RETURN TRUE
	ELSE	
		RETURN FALSE	
	END IF

		
end function

public function boolean noexisteproductor (long productor);Integer li_Contador, li_zona

IF cbx_todoszona.Checked = TRUE THEN
	li_zona = 0
ELSE
	li_zona	=	dw_zona.object.zona_codigo[1]
END IF

SELECT	Count(*)
	INTO	:li_Contador
	FROM	dba.productores
	WHERE	prod_codigo =	:productor
	AND	:li_zona in (0,zona_codigo);

IF sqlca.sqlcode = -1 THEN
	F_ErrorBaseDatos(sqlca,"No se pudo leer la tabla Productor")
	RETURN TRUE
ELSEIF li_Contador = 0 THEN
	messagebox("Atención","Código Productor No Existe Para la Zona" + &
					"~n~nSeleccionada, Ingrese Otro Por Favor",Exclamation!)
	RETURN TRUE
ELSE	
	RETURN FALSE	
END IF

end function

on w_info_ctlcalplacuanuvaevaluacio.create
int iCurrent
call super::create
this.st_1=create st_1
this.em_fechadesde=create em_fechadesde
this.st_12=create st_12
this.st_13=create st_13
this.em_fechahasta=create em_fechahasta
this.st_zona=create st_zona
this.dw_zona=create dw_zona
this.cbx_todoplanta=create cbx_todoplanta
this.cbx_consplanta=create cbx_consplanta
this.dw_planta=create dw_planta
this.st_33=create st_33
this.dw_productor=create dw_productor
this.cbx_todoprod=create cbx_todoprod
this.cbx_consprod=create cbx_consprod
this.st_2=create st_2
this.dw_agronomo=create dw_agronomo
this.dw_variedades=create dw_variedades
this.cbx_todosvar=create cbx_todosvar
this.cbx_consvariedad=create cbx_consvariedad
this.cbx_todoszona=create cbx_todoszona
this.cbx_conszonas=create cbx_conszonas
this.st_3=create st_3
this.cbx_todosagro=create cbx_todosagro
this.st_5=create st_5
this.dw_packing=create dw_packing
this.cbx_todospacking=create cbx_todospacking
this.cbx_conspacking=create cbx_conspacking
this.cbx_todosfecha=create cbx_todosfecha
this.cbx_consfecha=create cbx_consfecha
this.st_6=create st_6
this.em_embalaje=create em_embalaje
this.cb_buscaembalaje=create cb_buscaembalaje
this.em_descripcion=create em_descripcion
this.cbx_todosemb=create cbx_todosemb
this.cbx_consembalaje=create cbx_consembalaje
this.st_7=create st_7
this.cbx_todoscalibre=create cbx_todoscalibre
this.cbx_conscalibre=create cbx_conscalibre
this.em_calibre=create em_calibre
this.gb_4=create gb_4
this.st_8=create st_8
this.cbx_consagro=create cbx_consagro
this.gb_5=create gb_5
this.st_44=create st_44
this.st_9=create st_9
this.st_10=create st_10
this.st_11=create st_11
this.dw_especie=create dw_especie
this.st_4=create st_4
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_1
this.Control[iCurrent+2]=this.em_fechadesde
this.Control[iCurrent+3]=this.st_12
this.Control[iCurrent+4]=this.st_13
this.Control[iCurrent+5]=this.em_fechahasta
this.Control[iCurrent+6]=this.st_zona
this.Control[iCurrent+7]=this.dw_zona
this.Control[iCurrent+8]=this.cbx_todoplanta
this.Control[iCurrent+9]=this.cbx_consplanta
this.Control[iCurrent+10]=this.dw_planta
this.Control[iCurrent+11]=this.st_33
this.Control[iCurrent+12]=this.dw_productor
this.Control[iCurrent+13]=this.cbx_todoprod
this.Control[iCurrent+14]=this.cbx_consprod
this.Control[iCurrent+15]=this.st_2
this.Control[iCurrent+16]=this.dw_agronomo
this.Control[iCurrent+17]=this.dw_variedades
this.Control[iCurrent+18]=this.cbx_todosvar
this.Control[iCurrent+19]=this.cbx_consvariedad
this.Control[iCurrent+20]=this.cbx_todoszona
this.Control[iCurrent+21]=this.cbx_conszonas
this.Control[iCurrent+22]=this.st_3
this.Control[iCurrent+23]=this.cbx_todosagro
this.Control[iCurrent+24]=this.st_5
this.Control[iCurrent+25]=this.dw_packing
this.Control[iCurrent+26]=this.cbx_todospacking
this.Control[iCurrent+27]=this.cbx_conspacking
this.Control[iCurrent+28]=this.cbx_todosfecha
this.Control[iCurrent+29]=this.cbx_consfecha
this.Control[iCurrent+30]=this.st_6
this.Control[iCurrent+31]=this.em_embalaje
this.Control[iCurrent+32]=this.cb_buscaembalaje
this.Control[iCurrent+33]=this.em_descripcion
this.Control[iCurrent+34]=this.cbx_todosemb
this.Control[iCurrent+35]=this.cbx_consembalaje
this.Control[iCurrent+36]=this.st_7
this.Control[iCurrent+37]=this.cbx_todoscalibre
this.Control[iCurrent+38]=this.cbx_conscalibre
this.Control[iCurrent+39]=this.em_calibre
this.Control[iCurrent+40]=this.gb_4
this.Control[iCurrent+41]=this.st_8
this.Control[iCurrent+42]=this.cbx_consagro
this.Control[iCurrent+43]=this.gb_5
this.Control[iCurrent+44]=this.st_44
this.Control[iCurrent+45]=this.st_9
this.Control[iCurrent+46]=this.st_10
this.Control[iCurrent+47]=this.st_11
this.Control[iCurrent+48]=this.dw_especie
this.Control[iCurrent+49]=this.st_4
end on

on w_info_ctlcalplacuanuvaevaluacio.destroy
call super::destroy
destroy(this.st_1)
destroy(this.em_fechadesde)
destroy(this.st_12)
destroy(this.st_13)
destroy(this.em_fechahasta)
destroy(this.st_zona)
destroy(this.dw_zona)
destroy(this.cbx_todoplanta)
destroy(this.cbx_consplanta)
destroy(this.dw_planta)
destroy(this.st_33)
destroy(this.dw_productor)
destroy(this.cbx_todoprod)
destroy(this.cbx_consprod)
destroy(this.st_2)
destroy(this.dw_agronomo)
destroy(this.dw_variedades)
destroy(this.cbx_todosvar)
destroy(this.cbx_consvariedad)
destroy(this.cbx_todoszona)
destroy(this.cbx_conszonas)
destroy(this.st_3)
destroy(this.cbx_todosagro)
destroy(this.st_5)
destroy(this.dw_packing)
destroy(this.cbx_todospacking)
destroy(this.cbx_conspacking)
destroy(this.cbx_todosfecha)
destroy(this.cbx_consfecha)
destroy(this.st_6)
destroy(this.em_embalaje)
destroy(this.cb_buscaembalaje)
destroy(this.em_descripcion)
destroy(this.cbx_todosemb)
destroy(this.cbx_consembalaje)
destroy(this.st_7)
destroy(this.cbx_todoscalibre)
destroy(this.cbx_conscalibre)
destroy(this.em_calibre)
destroy(this.gb_4)
destroy(this.st_8)
destroy(this.cbx_consagro)
destroy(this.gb_5)
destroy(this.st_44)
destroy(this.st_9)
destroy(this.st_10)
destroy(this.st_11)
destroy(this.dw_especie)
destroy(this.st_4)
end on

event open;
x = 0
y = 0

This.Icon	=	Gstr_apl.Icono

iuo_zonas				 =  Create uo_zonas
iuo_ctlcalagronomos	 =  Create uo_ctlcalagronomos

//zona
dw_zona.GetChild("zona_codigo", idwc_zona)
idwc_zona.SetTransObject(sqlca)
idwc_zona.Retrieve()
dw_zona.InsertRow(0)
idwc_zona.SetSort("Zona_nombre asc")
idwc_zona.Sort()
//dw_zona.SetItem(1,"zona_codigo", gi_codZona)

//Planta
dw_planta.GetChild("plde_codigo", idwc_planta)
idwc_planta.SetTransObject(sqlca)
idwc_planta.Retrieve(1,0)
idwc_planta.SetSort("plde_nombre A")
idwc_planta.Sort()
dw_planta.InsertRow(0)

//Productor
dw_productor.GetChild("prod_codigo", idwc_productores)
idwc_productores.SetTransObject(sqlca)
idwc_productores.Retrieve(0)
idwc_productores.SetSort("prod_nombre A")
idwc_productores.Sort()
dw_productor.InsertRow(0)

//Especie
dw_Especie.GetChild("espe_codigo", idwc_especie)
idwc_especie.SetTransObject(sqlca)
idwc_especie.Retrieve()
dw_Especie.InsertRow(1)
dw_Especie.SetItem(1,"espe_codigo", 11)

istr_Mant.Argumento[10] = string(dw_especie.Object.espe_codigo[1])
iuo_especie  =	Create uo_especie

////Variedades
dw_variedades.GetChild("vari_codigo", idwc_variedad)
idwc_variedad.SetTransObject(sqlca)
idwc_variedad.Retrieve(gi_CodEspecie)
dw_variedades.InsertRow(0)
idwc_variedad.SetSort("vari_nombre asc")
idwc_variedad.Sort()

//Técnicos
dw_agronomo.GetChild("ccag_codigo", idwc_tecnico)
idwc_tecnico.SetTransObject(sqlca)
idwc_tecnico.Retrieve(0)
dw_agronomo.InsertRow(0)
idwc_tecnico.SetSort("ccag_nombre asc")
idwc_tecnico.Sort()
////Packing
dw_packing.GetChild("plde_codigo",idwc_packing)
idwc_packing.SetTransObject(sqlca)
idwc_packing.Retrieve(2,0) 
dw_packing.InsertRow(0)
idwc_packing.SetSort("plde_nombre A")
idwc_packing.Sort()	

em_fechadesde.text	=	String(Today())
em_fechahasta.text	=	String(Today())
//dw_planta.Enabled		=	FALSE
//dw_zona.Enabled		=	FALSE

end event

type st_computador from w_para_informes`st_computador within w_info_ctlcalplacuanuvaevaluacio
end type

type st_usuario from w_para_informes`st_usuario within w_info_ctlcalplacuanuvaevaluacio
end type

type st_temporada from w_para_informes`st_temporada within w_info_ctlcalplacuanuvaevaluacio
end type

type p_logo from w_para_informes`p_logo within w_info_ctlcalplacuanuvaevaluacio
end type

type st_titulo from w_para_informes`st_titulo within w_info_ctlcalplacuanuvaevaluacio
integer x = 73
integer y = 64
integer width = 2546
string text = "Informe Porcentual de Calificación y Resolución de Lotes"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_ctlcalplacuanuvaevaluacio
string tag = "Imprimir Reporte"
integer x = 2738
integer y = 596
integer taborder = 310
string powertiptext = "Imprimir Reporte"
end type

event pb_acepta::clicked;SetPointer(Arrow!)
Integer	li_fila, li_planta, li_Variedad, li_Agronomo,li_Packing, li_especie, &
			li_zona, li_ConsFechaEmb
Date		ld_FechaEmbaini, ld_FechaEmbafin
String   ls_calibre, ls_embalaje,setting
Long     ll_Productor
SetPointer(HourGlass!)

IF cbx_conszonas.Checked  THEN
	li_zona	= -9
ELSEIF cbx_todoszona.Checked THEN
	li_zona	= -1
ELSE
	li_zona	= dw_zona.Object.zona_codigo[1]
END IF

IF cbx_consplanta.Checked THEN
	li_planta	 = -9
ELSEIF cbx_todoplanta.Checked THEN 
	li_planta	 = -1
ELSE	
	li_planta	 = dw_planta.Object.plde_codigo[1]
END IF

IF cbx_consprod.Checked THEN
	ll_productor	 = -9
ELSEIF cbx_todoprod.Checked THEN 
	ll_productor	 = -1
ELSE
	ll_productor	= dw_productor.Object.prod_codigo[1]
	IF IsNull(ll_productor)THEN
		MessageBox("Atención","Debe Seleccionar un Productor Previamente",Exclamation!)
	   RETURN
	END IF
END IF

IF cbx_consvariedad.Checked THEN
	li_variedad	 = -9
ELSEIF cbx_todosvar.Checked THEN 
	li_variedad	 = -1
ELSE
	li_variedad  = dw_variedades.Object.vari_codigo[1]
END IF

IF cbx_consagro.Checked THEN
	li_agronomo	 = -9
ELSEIF cbx_todosagro.Checked THEN
	li_agronomo	 = -1
ELSE
	li_agronomo	= dw_agronomo.Object.ccag_codigo[1]
	IF IsNull(li_agronomo)THEN
		MessageBox("Atención","Debe Seleccionar un Agronomo Previamente",Exclamation!)
		RETURN
	END IF
END IF

IF cbx_conspacking.Checked THEN
	li_packing	 = -9
ELSEIF cbx_todospacking.Checked THEN
	li_packing	 = -1
ELSE
	li_packing	=	dw_packing.Object.plde_codigo[1]
	IF IsNull(li_Packing)THEN
		MessageBox("Atención","Debe Seleccionar un Packing Previamente",Exclamation!)
		RETURN
	END IF
END IF

IF cbx_todosfecha.Checked THEN
	ld_FechaEmbaini =	Date(01/01/2000)
	ld_FechaEmbafin =	Today()
ELSE
	ld_FechaEmbaini = Date(em_fechadesde.Text)
	ld_FechaEmbafin = Date(em_fechahasta.Text)
END IF

IF cbx_consfecha.Checked THEN
	li_ConsFechaEmb	= 1
	ld_FechaEmbaini =	Date(01/01/2000)
	ld_FechaEmbafin =	Today()
ELSEIF cbx_todosfecha.Checked THEN
	ld_FechaEmbaini =	Date(01/01/2000)
	ld_FechaEmbafin =	Today() 
	li_ConsFechaEmb	= 0
ELSE
	ld_FechaEmbaini = Date(em_fechadesde.Text)
	ld_FechaEmbafin = Date(em_fechahasta.Text)
	li_ConsFechaEmb	= 0
END IF

IF cbx_conscalibre.Checked THEN
	ls_calibre	= '**'
ELSEIF cbx_todoscalibre.Checked THEN
	ls_calibre	= '*' 
ELSE 
	ls_calibre	= em_calibre.Text 
	IF IsNull(ls_calibre) OR ls_calibre = "" THEN
		MessageBox("Atención","Debe Digitar un Calibre Previamente",Exclamation!)
		RETURN
	ELSE
		ls_calibre	= em_calibre.Text + Fill(" ",3 - Len(ls_calibre))
	END IF
END IF

IF cbx_consembalaje.Checked THEN
	ls_embalaje = '**'
ELSEIF cbx_todosemb.Checked THEN
	ls_embalaje = '*' 
ELSE
	ls_embalaje	= em_embalaje.Text
	IF IsNull(ls_embalaje) OR ls_embalaje = "" THEN
		MessageBox("Atención","Debe Digitar un Embalaje Previamente",Exclamation!)
		RETURN
	END IF
END IF

//li_especie = integer(istr_Mant.Argumento[10])
li_especie = dw_especie.Object.espe_codigo[1]


istr_info.titulo	= 'INFORME DE CALIFICACION Y RESOLUCION DE LOTES'

OpenWithParm(vinf,istr_info)

vinf.dw_1.DataObject = "dw_info_ctlcalplacuanuvaevaluacion"
vinf.dw_1.SetTransObject(sqlca)

li_fila = vinf.dw_1.Retrieve( gi_codexport,li_zona, li_agronomo, li_Planta, ld_FechaEmbaini, &
									  ld_FechaEmbafin, ll_productor, li_especie, li_Variedad, &
									  ls_embalaje, ls_calibre, li_Packing, li_ConsFechaEmb)


IF li_fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF li_fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", &
					StopSign!, Ok!)
ELSE
	F_Membrete(vinf.dw_1)
	vinf.dw_1.Modify("desde.text = '" + em_fechadesde.text + "'")
	vinf.dw_1.Modify("hasta.text = '" + em_fechahasta.text + "'")	

	setting = vinf.dw_1.Object.DataWindow.Zoom
	
	vinf.dw_1.Object.DataWindow.Zoom = 89
	
	IF gs_Ambiente <> 'Windows' THEN
		F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)

	END IF
END IF

SetPointer(Arrow!)

end event

type pb_salir from w_para_informes`pb_salir within w_info_ctlcalplacuanuvaevaluacio
string tag = "Salir [Cerrar Ventana Activa]"
integer x = 2734
integer y = 876
integer taborder = 320
string powertiptext = "Salir [Cerrar Ventana Activa]"
end type

type st_1 from statictext within w_info_ctlcalplacuanuvaevaluacio
integer x = 178
integer y = 940
integer width = 311
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
string text = "Frigorífico"
boolean focusrectangle = false
end type

type em_fechadesde from editmask within w_info_ctlcalplacuanuvaevaluacio
integer x = 667
integer y = 1612
integer width = 370
integer height = 92
integer taborder = 290
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16777215
boolean enabled = false
string text = "none"
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
end type

type st_12 from statictext within w_info_ctlcalplacuanuvaevaluacio
integer x = 672
integer y = 1548
integer width = 201
integer height = 56
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
string text = "Desde"
boolean focusrectangle = false
end type

type st_13 from statictext within w_info_ctlcalplacuanuvaevaluacio
integer x = 1230
integer y = 1552
integer width = 178
integer height = 56
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
string text = "Hasta"
boolean focusrectangle = false
end type

type em_fechahasta from editmask within w_info_ctlcalplacuanuvaevaluacio
integer x = 1211
integer y = 1612
integer width = 370
integer height = 92
integer taborder = 300
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16777215
boolean enabled = false
string text = "none"
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
end type

type st_zona from statictext within w_info_ctlcalplacuanuvaevaluacio
integer x = 178
integer y = 572
integer width = 462
integer height = 84
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
boolean enabled = false
string text = "Zona Origen"
boolean focusrectangle = false
end type

type dw_zona from datawindow within w_info_ctlcalplacuanuvaevaluacio
integer x = 667
integer y = 556
integer width = 841
integer height = 92
integer taborder = 70
boolean bringtotop = true
string dataobject = "dddw_zonas"
boolean border = false
boolean livescroll = true
end type

event itemchanged;String ls_Nula
SetNull(ls_Nula)

dw_zona.PostEvent(Clicked!)

IF iuo_zonas.existe(Integer(Data),True,sqlca) = False THEN
	This.SetItem(1, "zona_codigo", Long(ls_nula))
	RETURN 1
ELSE	
	dw_Packing.GetChild("plde_codigo", idwc_packing)
	idwc_packing.SetTransObject(sqlca)
	IF idwc_packing.Retrieve(2,Integer(Data)) = 0 THEN
		dw_Packing.SetItem(1, "plde_codigo", Long(ls_Nula))
	END IF	
	
	dw_agronomo.GetChild("ccag_codigo", idwc_tecnico)
	idwc_tecnico.SetTransObject(sqlca)
	idwc_tecnico.Retrieve(Integer(Data))
	dw_agronomo.SetItem(1, "ccag_codigo", Long(ls_Nula))
	
	dw_productor.GetChild("prod_codigo", idwc_productores)
	idwc_productores.SetTransObject(sqlca)
	idwc_productores.Retrieve(Integer(Data))
	dw_productor.SetItem(1, "prod_codigo", Long(ls_Nula))	
	
	dw_planta.GetChild("plde_codigo", idwc_planta)
	idwc_planta.SetTransObject(sqlca)
	idwc_planta.Retrieve(1,Integer(data))
	idwc_planta.SetSort("plde_nombre A")
	idwc_planta.Sort()
	dw_planta.InsertRow(0)
	
	RETURN 0
END IF	
end event

event itemerror;Return 1
end event

event clicked;cbx_todoszona.Checked = False
cbx_conszonas.Checked = False
end event

type cbx_todoplanta from checkbox within w_info_ctlcalplacuanuvaevaluacio
integer x = 1943
integer y = 936
integer width = 123
integer height = 64
integer taborder = 150
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = " "
boolean checked = true
end type

event clicked;Integer	li_null

SetNull(li_Null)

IF This.Checked THEN
	//dw_planta.Enabled = FALSE	
	//cbx_consplanta.Checked	=	FALSE
	cbx_consplanta.Checked	=	False
   dw_planta.SetItem(1, "plde_codigo", li_null)
ELSE
//cbx_consplanta.Checked	=	FALSE
//cbx_consplanta.Enabled	=	FALSE
//dw_planta.Enabled 		=  TRUE
dw_planta.Setfocus()
dw_planta.SetItem(1, "plde_codigo",li_null)
	
END IF
return 0

end event

type cbx_consplanta from checkbox within w_info_ctlcalplacuanuvaevaluacio
integer x = 2309
integer y = 936
integer width = 146
integer height = 64
integer taborder = 140
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = " "
end type

event clicked;String ls_Nula
SetNull(ls_Nula)
IF This.CheCked THEN
	cbx_todoplanta.Checked	=	FALSE	
	//dw_planta.Enabled 		=	FALSE
	dw_planta.SetItem(1, "plde_codigo", Long(ls_nula))
ELSE
	//dw_planta.Enabled 		=  TRUE
	//dw_planta.Setfocus()
	//dw_planta.SetItem(1, "plde_codigo", gi_CodPlanta)
END IF
RETURN 0
end event

type dw_planta from datawindow within w_info_ctlcalplacuanuvaevaluacio
integer x = 667
integer y = 928
integer width = 969
integer height = 92
integer taborder = 160
boolean bringtotop = true
string dataobject = "dddw_plantadesp_tipo_zona"
boolean border = false
boolean livescroll = true
end type

event itemchanged;String ls_Nula
SetNull(ls_Nula)

dw_planta.PostEvent(Clicked!)

IF NoExistePlanta(Integer(data),1) THEN
	This.SetItem(1, "plde_codigo", Long(ls_nula))
	RETURN 1
END IF	

end event

event itemerror;RETURN 1
end event

event clicked;cbx_todoplanta.Checked = False
cbx_consplanta.Checked = False
end event

type st_33 from statictext within w_info_ctlcalplacuanuvaevaluacio
integer x = 178
integer y = 688
integer width = 306
integer height = 84
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
boolean enabled = false
string text = "Productor"
boolean focusrectangle = false
end type

type dw_productor from datawindow within w_info_ctlcalplacuanuvaevaluacio
integer x = 667
integer y = 680
integer width = 955
integer height = 92
integer taborder = 100
boolean bringtotop = true
string dataobject = "dddw_productores_zona"
boolean border = false
boolean livescroll = true
end type

event itemchanged;String ls_Nula

dw_productor.PostEvent(Clicked!)

SetNull(ls_Nula)
 IF NoExisteProductor(Long(data)) THEN
    This.SetItem(1,"prod_codigo", Long(ls_nula))
    RETURN 1
ELSE
	idwc_tecnico.Retrieve(Long(data),Integer(dw_zona.object.zona_codigo[1]))
END IF	


end event

event itemerror;Return 1
end event

event clicked;cbx_todoprod.Checked = False
cbx_consprod.Checked = False
end event

type cbx_todoprod from checkbox within w_info_ctlcalplacuanuvaevaluacio
integer x = 1943
integer y = 684
integer width = 123
integer height = 64
integer taborder = 90
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = " "
boolean checked = true
end type

event clicked;Integer	li_null
Setnull(li_null)
IF This.Checked THEN
	//dw_productor.Enabled = FALSE	
	cbx_consprod.Checked = False
	
	dw_productor.SetItem(1, "prod_codigo", Long(li_null))
	IF cbx_todoszona.Checked	=	TRUE	THEN
		idwc_tecnico.Retrieve(0,0)
	ELSE	
		idwc_tecnico.Retrieve(0,Integer(dw_zona.object.zona_codigo[1]))
	END IF
ELSE
	//cbx_consprod.Checked = FALSE
	//cbx_consprod.Enabled = FALSE
	//dw_productor.Enabled 		=  TRUE
	dw_productor.Setfocus()
END IF
end event

type cbx_consprod from checkbox within w_info_ctlcalplacuanuvaevaluacio
integer x = 2309
integer y = 684
integer width = 146
integer height = 64
integer taborder = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = " "
end type

event clicked;String ls_Nula
SetNull(ls_Nula)
IF THIS.CheCked THEN
	cbx_todoprod.Checked	=	FALSE	
	//dw_productor.Enabled 		=	FALSE
	dw_productor.SetItem(1, "prod_codigo", Long(ls_nula))
ELSE
	//dw_productor.Enabled 		=  FALSE
	dw_productor.Setfocus()
END IF

end event

type st_2 from statictext within w_info_ctlcalplacuanuvaevaluacio
integer x = 178
integer y = 1068
integer width = 338
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Agrónomo"
boolean focusrectangle = false
end type

type dw_agronomo from datawindow within w_info_ctlcalplacuanuvaevaluacio
integer x = 667
integer y = 1052
integer width = 1024
integer height = 92
integer taborder = 190
boolean bringtotop = true
string dataobject = "dddw_ctlcalagronomos"
boolean border = false
boolean livescroll = true
end type

event itemchanged;String	ls_Nula
Integer	li_zona
Long     ll_productor
SetNull(ls_Nula)


dw_agronomo.PostEvent(Clicked!)
//
//IF  iuo_ctlcalagronomos.ofp_recupera_ctlcalagronomos(Sqlca, li_zona, Integer(data),TRUE) = FALSE THEN
//  THIS.SetItem(1, "ccag_codigo", Long(ls_nula))
//  RETURN 1																		
//END IF
//RETURN 0

IF NoExisteagroproduc(Integer(data)) THEN
    This.SetItem(1,"ccag_codigo", Long(ls_nula))
    RETURN 1
END IF	

end event

event itemerror;RETURN 1
end event

event clicked;cbx_todosagro.Checked = False
cbx_consagro.Checked = False
end event

type dw_variedades from datawindow within w_info_ctlcalplacuanuvaevaluacio
integer x = 667
integer y = 432
integer width = 882
integer height = 92
integer taborder = 40
boolean bringtotop = true
string dataobject = "dddw_variedades"
boolean border = false
boolean livescroll = true
end type

event itemchanged;String ls_Nula
SetNull(ls_Nula)

dw_variedades.PostEvent(Clicked!)

IF NoExisteVariedad(Integer(data)) THEN
  THIS.SetItem(1, "vari_codigo", Long(ls_nula))
  RETURN 1
ELSE
  em_calibre.Text	=	''
  RETURN 0
END IF  
end event

event itemerror;Return 1
end event

event clicked;cbx_todosvar.Checked = False
cbx_consvariedad.Checked = False
end event

type cbx_todosvar from checkbox within w_info_ctlcalplacuanuvaevaluacio
integer x = 1943
integer y = 440
integer width = 123
integer height = 64
integer taborder = 30
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = " "
boolean checked = true
end type

event clicked;Integer	li_Null

SetNull(li_Null)
IF This.Checked THEN
	//dw_variedades.Enabled	=	FALSE	
	dw_variedades.SetItem(1, "vari_codigo", li_Null)
	cbx_consvariedad.Checked = false	
ELSE
	//cbx_consvariedad.Enabled = FALSE
	//cbx_consvariedad.Checked = FALSE
	//dw_variedades.Enabled	=	TRUE	
	dw_variedades.Setfocus()
	dw_variedades.SetItem(1, "vari_codigo", li_null)
END IF
end event

type cbx_consvariedad from checkbox within w_info_ctlcalplacuanuvaevaluacio
integer x = 2309
integer y = 440
integer width = 146
integer height = 64
integer taborder = 20
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = " "
end type

event clicked;String ls_Nula
SetNull(ls_Nula)
IF This.CheCked THEN
	cbx_todosvar.Checked	=	FALSE
	//dw_variedades.Enabled	=	FALSE
	dw_variedades.SetItem(1, "vari_codigo", Long(ls_nula))
ELSE
	//dw_variedades.Enabled	=	FALSE
	//dw_variedades.Setfocus()
	//dw_variedades.SetItem(1, "vari_codigo", 1)	
END IF
RETURN 0
end event

type cbx_todoszona from checkbox within w_info_ctlcalplacuanuvaevaluacio
integer x = 1943
integer y = 568
integer width = 123
integer height = 64
integer taborder = 60
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = " "
boolean checked = true
end type

event clicked;Integer	li_null

SetNull(li_null)
IF This.Checked THEN
	cbx_conszonas.Checked = False
	//dw_zona.Enabled = FALSE	
	dw_zona.SetItem(1, "zona_codigo", li_null)
	/*Planta*/
	idwc_planta.Retrieve(gi_codexport,1,0)
	idwc_planta.SetSort("plde_nombre A")
	idwc_planta.Sort()
	/*Packing*/
	idwc_packing.Retrieve(gi_codexport,2,0)
	/*Productores*/
	idwc_productores.Retrieve(gi_codexport,0)
	/*Agronomo*/
	idwc_tecnico.Retrieve(0,0)
ELSE
	//cbx_conszonas.Enabled = FALSE
	//cbx_conszonas.Checked = FALSE
	dw_zona.SetItem(1, "zona_codigo", li_null)
	/*Planta*/
	idwc_planta.Retrieve(gi_codexport,1,gi_codzona)
	idwc_planta.SetSort("plde_nombre A")
	idwc_planta.Sort()
	/*Packing*/
	idwc_packing.Retrieve(gi_codexport,2,gi_codzona)
	/*Productores*/
	idwc_productores.Retrieve(gi_codexport,gi_codzona)
	/*Agronomo*/
	idwc_tecnico.Retrieve(0,gi_codzona)
	//dw_zona.Enabled 		=  TRUE
	dw_zona.Setfocus()
END IF
return 0

end event

type cbx_conszonas from checkbox within w_info_ctlcalplacuanuvaevaluacio
integer x = 2309
integer y = 568
integer width = 146
integer height = 64
integer taborder = 50
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = " "
end type

event clicked;String ls_Nula
SetNull(ls_Nula)
IF This.CheCked THEN
	cbx_todoszona.Checked	=	FALSE
	//dw_zona.Enabled	=	FALSE
	dw_zona.SetItem(1, "zona_codigo", Long(ls_nula))
	idwc_planta.Retrieve(gi_codexport,1,0)
	idwc_planta.SetSort("plde_nombre A")
	idwc_planta.Sort()
ELSE
	//dw_zona.Enabled 		=  FALSE
	//dw_zona.Setfocus()
END IF
RETURN 0
end event

type st_3 from statictext within w_info_ctlcalplacuanuvaevaluacio
integer x = 178
integer y = 448
integer width = 343
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Variedades"
boolean focusrectangle = false
end type

type cbx_todosagro from checkbox within w_info_ctlcalplacuanuvaevaluacio
integer x = 1943
integer y = 1068
integer width = 123
integer height = 64
integer taborder = 180
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = " "
boolean checked = true
end type

event clicked;Integer	li_Null

SetNull(li_Null)
IF This.Checked THEN
	//dw_agronomo.Enabled	=	FALSE	
	cbx_consagro.Checked	=	False
	dw_agronomo.SetItem(1, "ccag_codigo", li_null)
ELSE
	//cbx_consagro.Checked	=	FALSE
	//cbx_consagro.Enabled	=	FALSE
	//dw_agronomo.Enabled	=	TRUE
	dw_agronomo.Setfocus()
END IF
end event

type st_5 from statictext within w_info_ctlcalplacuanuvaevaluacio
integer x = 178
integer y = 820
integer width = 325
integer height = 84
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Packing"
boolean focusrectangle = false
end type

type dw_packing from datawindow within w_info_ctlcalplacuanuvaevaluacio
integer x = 667
integer y = 804
integer width = 978
integer height = 92
integer taborder = 130
boolean bringtotop = true
string dataobject = "dddw_plantapacking_zonas"
boolean border = false
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event itemchanged;String ls_Nula
SetNull(ls_Nula)

dw_packing.PostEvent(Clicked!)

IF NoExistePlanta(Integer(data),2) THEN
	This.SetItem(1, "plde_codigo", Long(ls_nula))
	RETURN 1
END IF	

end event

event itemerror;Return 1
end event

event clicked;cbx_todospacking.Checked = False
cbx_conspacking.Checked = False
end event

type cbx_todospacking from checkbox within w_info_ctlcalplacuanuvaevaluacio
integer x = 1943
integer y = 820
integer width = 123
integer height = 64
integer taborder = 120
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = " "
boolean checked = true
end type

event clicked;Integer	li_Null

SetNull(li_null)
IF THIS.Checked THEN
	//dw_packing.Enabled	=	FALSE	
	cbx_conspacking.Checked	=	False
	dw_packing.SetItem(1, "plde_codigo",li_Null)
ELSE
	//cbx_conspacking.Checked = FALSE	
	//dw_packing.Enabled	=	TRUE
	//cbx_conspacking.Enabled	=	FALSE
	dw_packing.Setfocus()
END IF
end event

type cbx_conspacking from checkbox within w_info_ctlcalplacuanuvaevaluacio
integer x = 2309
integer y = 820
integer width = 146
integer height = 60
integer taborder = 110
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = " "
end type

event clicked;String ls_Nula
SetNull(ls_Nula)
IF This.CheCked THEN
	cbx_todospacking.Checked	=	FALSE	
	//dw_packing.Enabled	=	FALSE
	dw_packing.SetItem(1, "plde_codigo", Long(ls_nula))
ELSE
	//dw_packing.Enabled	=	FALSE
	//dw_packing.Setfocus()
END IF
RETURN 0
end event

type cbx_todosfecha from checkbox within w_info_ctlcalplacuanuvaevaluacio
integer x = 1943
integer y = 1628
integer width = 279
integer height = 64
integer taborder = 280
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = " "
boolean checked = true
end type

event clicked;IF This.Checked THEN
	em_fechadesde.Enabled		=	False
	em_fechahasta.Enabled	=	False
   istr_Mant.Argumento[12]	=	'01/01/1900'
	istr_Mant.Argumento[13]	=	String(Today())
	cbx_consfecha.Checked	=	False
ELSE
	//cbx_consfecha.Enabled	=	FALSE
	cbx_consfecha.Checked	=	FALSE
	em_fechadesde.Enabled	   =	TRUE
	em_fechahasta.Enabled	=	TRUE
	istr_Mant.Argumento[12] =	em_fechadesde.Text
	istr_Mant.Argumento[13]	=	em_fechahasta.Text
	em_fechadesde.Setfocus()
END IF
RETURN 0
end event

type cbx_consfecha from checkbox within w_info_ctlcalplacuanuvaevaluacio
integer x = 2309
integer y = 1624
integer width = 123
integer height = 64
integer taborder = 270
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = " "
end type

event clicked;IF This.Checked THEN
	em_fechadesde.Enabled		=	False
	em_fechahasta.Enabled	=	False
   istr_Mant.Argumento[12]	=	'01/01/1900'
	istr_Mant.Argumento[13]	=	String(Today())
	cbx_todosfecha.Checked	=	FALSE
ELSE
	em_fechadesde.Enabled	   =	TRUE
	em_fechahasta.Enabled	=	TRUE
	istr_Mant.Argumento[12] =	em_fechadesde.Text
	istr_Mant.Argumento[13]	=	em_fechahasta.Text
	em_fechadesde.Setfocus()
END IF
RETURN 0
end event

type st_6 from statictext within w_info_ctlcalplacuanuvaevaluacio
integer x = 178
integer y = 1328
integer width = 325
integer height = 84
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Embalaje"
boolean focusrectangle = false
end type

type em_embalaje from editmask within w_info_ctlcalplacuanuvaevaluacio
integer x = 667
integer y = 1324
integer width = 293
integer height = 92
integer taborder = 230
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16777215
boolean enabled = false
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = stringmask!
string minmax = "~~3"
end type

event modified;IF NoExisteEmbalaje(This.Text) THEN
	This.Text	=	''
	//em_embalaje.SetFocus()
	RETURN 

END IF
end event

type cb_buscaembalaje from commandbutton within w_info_ctlcalplacuanuvaevaluacio
integer x = 974
integer y = 1324
integer width = 105
integer height = 88
integer taborder = 220
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

event clicked;buscaembalaje(gi_CodExport) 
end event

type em_descripcion from editmask within w_info_ctlcalplacuanuvaevaluacio
integer x = 1083
integer y = 1324
integer width = 805
integer height = 92
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
boolean enabled = false
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = stringmask!
string minmax = "~~3"
end type

type cbx_todosemb from checkbox within w_info_ctlcalplacuanuvaevaluacio
integer x = 1943
integer y = 1336
integer width = 279
integer height = 64
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
string text = " "
boolean checked = true
end type

event clicked;IF This.Checked THEN
	em_embalaje.Enabled			=	FALSE	
	cb_buscaembalaje.Enabled	=	FALSE
	cbx_consembalaje.Checked	=	False
	em_descripcion.Text = " "
ELSE
	cbx_consembalaje.Enabled	=	FALSE
	cbx_consembalaje.Checked	=	FALSE
	em_embalaje.Enabled 			=  TRUE
	cb_buscaembalaje.Enabled	=	TRUE
	//em_embalaje.Setfocus()
END IF
end event

type cbx_consembalaje from checkbox within w_info_ctlcalplacuanuvaevaluacio
integer x = 2309
integer y = 1336
integer width = 233
integer height = 64
integer taborder = 200
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = " "
end type

event clicked;IF This.Checked THEN
	em_embalaje.Enabled			=	FALSE	
	cb_buscaembalaje.Enabled	=	FALSE
	cbx_todosemb.Checked	=	FALSE
	em_descripcion.Text = " "
ELSE
	em_embalaje.Enabled 			=  TRUE
	cb_buscaembalaje.Enabled	=	TRUE
	//em_embalaje.Setfocus()
END IF
end event

type st_7 from statictext within w_info_ctlcalplacuanuvaevaluacio
integer x = 178
integer y = 1464
integer width = 325
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = "Calibre"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type cbx_todoscalibre from checkbox within w_info_ctlcalplacuanuvaevaluacio
integer x = 1943
integer y = 1476
integer width = 279
integer height = 64
integer taborder = 250
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = " "
boolean checked = true
end type

event clicked;IF This.Checked THEN
	em_calibre.Enabled	=	FALSE	
	cbx_conscalibre.Checked	=	False
ELSE
	cbx_conscalibre.Enabled	=	FALSE
	cbx_conscalibre.Checked	=	FALSE
	em_calibre.Enabled	=	TRUE
	em_calibre.Setfocus()
END IF

RETURN 0
end event

type cbx_conscalibre from checkbox within w_info_ctlcalplacuanuvaevaluacio
integer x = 2309
integer y = 1476
integer width = 169
integer height = 64
integer taborder = 240
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = " "
end type

event clicked;IF This.Checked THEN
	em_calibre.Enabled	=	FALSE	
	cbx_todoscalibre.Checked	=	FALSE	
ELSE
	em_calibre.Enabled	=	TRUE
	em_calibre.Setfocus()
END IF

RETURN 0
end event

type em_calibre from editmask within w_info_ctlcalplacuanuvaevaluacio
integer x = 667
integer y = 1452
integer width = 256
integer height = 92
integer taborder = 260
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16777215
boolean enabled = false
textcase textcase = upper!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = stringmask!
string minmax = "~~3"
end type

event modified;Integer li_variedad

IF cbx_todosvar.Checked THEN
	li_variedad = 0
ELSE
	li_variedad = dw_variedades.Object.vari_codigo[1]
END IF

IF NOT IsNull(li_variedad) THEN
	IF THIS.text <> '' THEN
	 IF NoExisteVarieCalibre(li_variedad,This.Text) THEN
		THIS.Text	=	''
		em_calibre.SetFocus()
	 END IF	
	END IF 
ELSE
	MessageBox("Atención","Previamente Debe Elegir Una Variedad",Exclamation!)
	THIS.Text	=	''
	RETURN 1
END IF	

end event

type gb_4 from groupbox within w_info_ctlcalplacuanuvaevaluacio
integer x = 114
integer y = 1264
integer width = 2455
integer height = 484
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
end type

type st_8 from statictext within w_info_ctlcalplacuanuvaevaluacio
integer x = 73
integer y = 1256
integer width = 2546
integer height = 536
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type cbx_consagro from checkbox within w_info_ctlcalplacuanuvaevaluacio
integer x = 2309
integer y = 1068
integer width = 146
integer height = 64
integer taborder = 170
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = " "
end type

event clicked;String ls_Nula
SetNull(ls_Nula)
IF This.CheCked THEN
	cbx_todosagro.Checked	=	FALSE	
	//dw_agronomo.Enabled	=	FALSE
	dw_agronomo.SetItem(1, "ccag_codigo", Long(ls_nula))
ELSE
	//dw_agronomo.Enabled	=	TRUE
	//dw_agronomo.Setfocus()
END IF
RETURN 0
end event

type gb_5 from groupbox within w_info_ctlcalplacuanuvaevaluacio
integer x = 114
integer y = 176
integer width = 2455
integer height = 1040
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
end type

type st_44 from statictext within w_info_ctlcalplacuanuvaevaluacio
integer x = 73
integer y = 168
integer width = 2546
integer height = 1088
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_9 from statictext within w_info_ctlcalplacuanuvaevaluacio
integer x = 1874
integer y = 244
integer width = 247
integer height = 84
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
boolean enabled = false
string text = "Todos"
boolean focusrectangle = false
end type

type st_10 from statictext within w_info_ctlcalplacuanuvaevaluacio
integer x = 2162
integer y = 244
integer width = 375
integer height = 84
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
boolean enabled = false
string text = "Consolidado"
boolean focusrectangle = false
end type

type st_11 from statictext within w_info_ctlcalplacuanuvaevaluacio
integer x = 178
integer y = 324
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
string text = "Especie"
boolean focusrectangle = false
end type

type dw_especie from datawindow within w_info_ctlcalplacuanuvaevaluacio
integer x = 667
integer y = 308
integer width = 859
integer height = 92
integer taborder = 10
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_especies"
boolean border = false
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event itemchanged;Integer	li_Nula

SetNull(li_Nula)

IF iuo_Especie.Existe(Integer(data), True, sqlca) =False THEN
		This.SetItem(1, "espe_codigo", long(li_Nula))
		RETURN 1
END IF

idwc_variedad.SetTransObject(sqlca)
dw_variedades.reset()
idwc_variedad.Retrieve(Integer(data))
dw_variedades.InsertRow(0)
end event

event itemerror;Return 1
end event

type st_4 from statictext within w_info_ctlcalplacuanuvaevaluacio
integer x = 178
integer y = 1620
integer width = 485
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = "Fecha Embalaje"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

