$PBExportHeader$w_sele_observa_nulo.srw
$PBExportComments$Informe Existencia Sitio Revisión
forward
global type w_sele_observa_nulo from w_para_informes
end type
type mle_observacion from multilineedit within w_sele_observa_nulo
end type
end forward

global type w_sele_observa_nulo from w_para_informes
integer x = 14
integer y = 32
integer width = 3337
integer height = 1052
string title = "Observacion Guias Nulas"
boolean minbox = false
boolean maxbox = false
boolean resizable = false
windowtype windowtype = response!
string icon = "AppIcon!"
boolean toolbarvisible = false
mle_observacion mle_observacion
end type
global w_sele_observa_nulo w_sele_observa_nulo

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

on w_sele_observa_nulo.create
int iCurrent
call super::create
this.mle_observacion=create mle_observacion
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.mle_observacion
end on

on w_sele_observa_nulo.destroy
call super::destroy
destroy(this.mle_observacion)
end on

type pb_excel from w_para_informes`pb_excel within w_sele_observa_nulo
integer x = 2958
integer y = 132
end type

type st_computador from w_para_informes`st_computador within w_sele_observa_nulo
end type

type st_usuario from w_para_informes`st_usuario within w_sele_observa_nulo
end type

type st_temporada from w_para_informes`st_temporada within w_sele_observa_nulo
end type

type p_logo from w_para_informes`p_logo within w_sele_observa_nulo
end type

type st_titulo from w_para_informes`st_titulo within w_sele_observa_nulo
integer width = 2638
string text = "Observacion de Anulacion de GUias Despacho SII"
end type

type pb_acepta from w_para_informes`pb_acepta within w_sele_observa_nulo
boolean visible = false
integer x = 2981
integer y = 400
integer taborder = 130
boolean enabled = false
boolean default = false
end type

type pb_salir from w_para_informes`pb_salir within w_sele_observa_nulo
integer x = 2985
integer y = 664
integer taborder = 140
boolean cancel = false
boolean default = true
string picturename = "\Desarrollo 17\Imagenes\Botones\Aceptar.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\Aceptar-bn.png"
end type

event pb_salir::clicked;If IsNull(mle_observacion.Text) Or mle_observacion.Text = '' Then Return
istr_Mant.Argumento[1] = mle_observacion.Text

CloseWithReturn (Parent, istr_Mant)
end event

type mle_observacion from multilineedit within w_sele_observa_nulo
integer x = 256
integer y = 420
integer width = 2629
integer height = 492
integer taborder = 20
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean vscrollbar = true
textcase textcase = upper!
integer limit = 100
borderstyle borderstyle = stylelowered!
end type

