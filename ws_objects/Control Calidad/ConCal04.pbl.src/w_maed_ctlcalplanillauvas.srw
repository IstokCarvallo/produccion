$PBExportHeader$w_maed_ctlcalplanillauvas.srw
$PBExportComments$Ingresador de antecedentes para Planilla Cuantitativa.
forward
global type w_maed_ctlcalplanillauvas from w_mant_encab_deta_csd
end type
type dw_3 from datawindow within w_maed_ctlcalplanillauvas
end type
type cb_aprobar from commandbutton within w_maed_ctlcalplanillauvas
end type
type tab_1 from tab within w_maed_ctlcalplanillauvas
end type
type tabpage_1 from userobject within tab_1
end type
type dw_4 from uo_dw within tabpage_1
end type
type tabpage_1 from userobject within tab_1
dw_4 dw_4
end type
type tabpage_2 from userobject within tab_1
end type
type dw_5 from uo_dw within tabpage_2
end type
type tabpage_2 from userobject within tab_1
dw_5 dw_5
end type
type tab_1 from tab within w_maed_ctlcalplanillauvas
tabpage_1 tabpage_1
tabpage_2 tabpage_2
end type
end forward

global type w_maed_ctlcalplanillauvas from w_mant_encab_deta_csd
integer width = 5038
integer height = 2160
string title = "PLANILLA DE VERIFICACION DE PRODUCTO TERMINADO DE UVA DE MESA"
string menuname = ""
event ue_validapassword ( )
dw_3 dw_3
cb_aprobar cb_aprobar
tab_1 tab_1
end type
global w_maed_ctlcalplanillauvas w_maed_ctlcalplanillauvas

type variables
DataWindowChild idwc_clientes,idwc_zonas,idwc_plantas,idwc_productores,idwc_especies, &
idwc_variedades,idwc_tecnicos,idwc_inspectores,idwc_packings,idwc_calibres,idwc_embalajes, &
idwc_agronomos

w_mant_deta_ctlcalplanillauvas	iw_mantencion

uo_especie						iuo_especies
uo_ctlcalinspectores			iuo_ctlcalinspectores
uo_plantadesp					iuo_plantadesp
uo_productores					iuo_productores	
uo_ctlcaltipoinspec				iuo_ctlcaltipoinspec
uo_variedades					iuo_variedades	
uo_zonas						iuo_zonas
uo_embalajesprod				iuo_embalajesprod
uo_loteobjetadopendiente	iuo_Objetados




end variables

forward prototypes
protected function integer wf_modifica ()
public function long maximoplanilla ()
public function boolean noexisteagronomo (integer agronomo)
public function boolean noexistecalibre (string as_calibre)
public function boolean noexistegrupo (string data)
public subroutine existelote ()
public subroutine ingresodedatos ()
public function boolean existedetalleplanilla (string as_columna, string as_valor)
public function boolean existenumeroplanilla (string as_columna, string as_valor)
public function integer nuevolote (integer cliente, integer planta)
public subroutine habilitaencab (boolean habilita)
public subroutine habilitaingreso (string ls_columna)
protected function boolean wf_actualiza_db (boolean borrando)
public function boolean duplicado (string data)
public function boolean existenumerolote (string as_columna, string as_valor)
public subroutine buscalote ()
public subroutine limpia_data ()
public subroutine trae_planilla (long al_planilla)
end prototypes

event ue_validapassword();istr_mant.Argumento[1]	=	"Control de Calidad"
istr_mant.Argumento[2]	=	gstr_parlote.paswor

OpenWithParm(w_password, istr_mant)

istr_mant	=	Message.PowerObjectParm

IF istr_mant.Respuesta = 0 THEN Close(This)
end event

protected function integer wf_modifica ();IF dw_1.AcceptText() = -1 THEN RETURN -1
IF dw_2.AcceptText() = -1 THEN RETURN -1
IF dw_3.AcceptText() = -1 THEN RETURN -1
IF (dw_1.ModifiedCount() + dw_1.DeletedCount()) > 0 THEN RETURN 0
IF dw_2.ModifiedCount() > 0 THEN RETURN 0
IF dw_3.ModifiedCount() > 0 THEN RETURN 0

RETURN 1
end function

public function long maximoplanilla ();Long		ll_Maximo

SELECT	MAX(ccpe_numero)
	INTO	:ll_Maximo
	FROM	dbo.ctlcalplacuaninspuvaenc;

IF sqlca.sqlcode	=	-1	THEN
	F_ErrorBaseDatos(sqlca,"No se pudo leer la tabla de PLanillas de Inspección") 
ELSEIF ll_Maximo	>	0	THEN	
	
	ll_Maximo	=	ll_Maximo + 1	
	RETURN ll_Maximo
END IF

RETURN 0	
end function

public function boolean noexisteagronomo (integer agronomo);Integer li_Contador, li_zona
Long  ll_productor  

li_zona			=	dw_2.Object.zona_codigo[1]

SELECT	Count(*)
	INTO	:li_Contador
	FROM	dbo.ctlcalagronomos
	WHERE ccag_codigo = :agronomo
	AND   zona_codigo = :li_zona;

IF sqlca.sqlcode = -1 THEN
	F_ErrorBaseDatos(sqlca,"No se pudo leer la tabla Agrónomos")
	RETURN TRUE
ELSEIF li_Contador = 0 THEN
	messagebox("Atención","Código Agrónomo No Existe " + &
					"~n~nIngrese Otro Por Favor",Exclamation!)
	RETURN TRUE
ELSE	
	RETURN FALSE	
END IF
end function

public function boolean noexistecalibre (string as_calibre);//Integer li_contador
//
//Select Count(*)
//Into :li_Contador
//From dba.variecalibre
//Where vaca_calibr = :as_calibre;
//
//IF sqlca.sqlcode = -1 THEN
//	F_ErrorBaseDatos(sqlca,"No se pudo leer la tabla Calibre")
//	RETURN TRUE
//ELSEIF li_Contador = 0 THEN
//	RETURN TRUE
//ELSE	
//	RETURN FALSE	
//END IF
//	
//

RETURN TRUE
end function

public function boolean noexistegrupo (string data);Integer li_Contador, li_Grupo

li_Grupo = Integer(Data)

SELECT	Count(*)
	INTO	:li_Contador
	FROM	dbo.admagrupousuario
	WHERE	grpo_codigo = : li_grupo;

IF sqlca.sqlcode = -1 THEN
	F_ErrorBaseDatos(sqlca,"No se pudo leer la tabla Grupousuario")
	RETURN TRUE
ELSEIF li_Contador = 0 THEN
	RETURN TRUE
ELSE	
	istr_mant.argumento[1] = Data
	TriggerEvent("ue_recuperadatos")	
	RETURN FALSE	
END IF
end function

public subroutine existelote ();
end subroutine

public subroutine ingresodedatos ();String	ls_Mensaje, ls_Columna[]
Integer	li_Contador

IF Isnull(dw_2.Object.plde_codigo[il_Fila]) OR dw_2.Object.plde_codigo[il_Fila] = 0 THEN
	li_Contador	++
	ls_Mensaje 			= ls_Mensaje + "~nNúmero de Planta"
	ls_Columna[li_Contador]	= "plde_codigo"
END IF

IF Isnull(dw_2.Object.ccpe_numero[il_Fila]) OR dw_2.Object.ccpe_numero[il_Fila] = 0 THEN
	li_Contador	++
	ls_Mensaje 			= ls_Mensaje + "~nnúmero de Planilla"
	ls_Columna[li_Contador]	= "ccpe_numero"
END IF

IF li_Contador > 0 THEN
	MessageBox("Error de Consistencia", "Falta el Ingreso de :" + ls_Mensaje + ".", StopSign!, Ok!)
	dw_2.SetColumn(ls_Columna[1])
	dw_2.SetFocus()
	Message.DoubleParm = -1
END IF
end subroutine

public function boolean existedetalleplanilla (string as_columna, string as_valor);Integer	li_lote, li_planta, li_especie, li_variedad, li_packing, li_zona, &
         li_tecnico, li_inspector, li_tipoins, li_agronomo, li_estado
Long		ll_numero, ll_noguia, ll_productor
String	ls_Reslot
Date		ld_Fecemb, ld_fecins

ll_numero 		=	dw_2.Object.ccpe_numero[1]
li_Planta		=	dw_2.Object.plde_codigo[1]
// Hacer cambios para mostrar los lotes que existen para el detalle
CHOOSE CASE as_columna
	
	CASE "plde_codpak"
		ll_numero 		=	Long(as_valor)

END CHOOSE

SELECT   zona_codigo, prod_codigo, &
         espe_codigo, vari_codigo, ccag_codigo, ccin_codigo, ccpe_fechin, &
			ccti_codigo, ccpe_estado, max(ccpe_noguia)
	INTO	:li_zona, :ll_productor, :li_especie, &
			:li_variedad, :li_agronomo, :li_inspector, :ld_fecins, :li_tipoins,&
			:li_estado, :ll_noguia
	FROM	dbo.CTLCALPLACUANINSPUVAENC
	WHERE clie_codigo	=	:gstr_parempresa.empr_codexp
	AND	plde_codigo	=	:li_Planta
	AND	ccpe_numero	=	:ll_numero
	GROUP BY zona_codigo, prod_codigo, &
         espe_codigo, vari_codigo, ccag_codigo, ccin_codigo, ccpe_fechin, &
			ccti_codigo, ccpe_estado;		

IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla CTLCALPLACUANINSPUVADET")
	RETURN True
ELSEIF sqlca.sqlcode	=	0 THEN
	//istr_mant.argumento[1]	= String(li_lote)
	istr_mant.argumento[2]	= String(ll_numero)
	istr_mant.argumento[3]	= String(li_zona)		
	istr_mant.argumento[4]	= String(li_planta)
	istr_mant.argumento[5]	= String(ld_fecins)
	istr_mant.argumento[6]	= String(li_tipoins)
	istr_mant.argumento[7]	= String(gi_CodExport)
	istr_mant.argumento[8]	= String(ll_productor)
	istr_mant.argumento[9]	= String(li_especie)
	istr_mant.argumento[10]	= String(li_variedad)
	istr_mant.argumento[11]	= String(li_agronomo)
	istr_mant.argumento[12]	= String(li_inspector)
	istr_mant.argumento[13]	= String(ll_noguia)
	istr_mant.argumento[14]	= String(li_estado)
	//istr_mant.argumento[15]	= ls_Reslot

	dw_2.SetItem(1, "zona_codigo", li_zona)
	dw_2.SetItem(1, "plde_codigo", li_planta)
	dw_2.SetItem(1, "ccpe_fechin", ld_fecins)		
	dw_2.SetItem(1, "prod_codigo", ll_productor)		
	dw_2.SetItem(1, "espe_codigo", li_especie)
	dw_2.SetItem(1, "vari_codigo", li_variedad)
	dw_2.SetItem(1, "ccag_codigo", li_agronomo)		
	dw_2.SetItem(1, "ccin_codigo", li_inspector)		
	dw_2.SetItem(1, "ccpe_noguia", ll_noguia)	
	dw_2.SetItem(1, "ccti_codigo", li_tipoins)	
	dw_2.SetItem(1, "ccpe_estado", li_estado)
	//dw_2.SetItem(1, "ccpe_reslot", ls_reslot)	

	dw_2.GetChild("plde_codigo", idwc_plantas)
	idwc_plantas.SetTransObject(sqlca)
	idwc_plantas.Retrieve(1)

	dw_3.GetChild("plde_codpak", idwc_packings)
	idwc_packings.SetTransObject(sqlca)
	idwc_packings.Retrieve(2,Integer(istr_mant.argumento[3]))

	dw_2.GetChild("ccag_codigo", idwc_agronomos)
	idwc_agronomos.SetTransObject(sqlca)
	idwc_agronomos.Retrieve(0,Integer(istr_mant.argumento[3]))

	dw_2.GetChild("ccin_codigo", idwc_inspectores)
	idwc_inspectores.SetTransObject(sqlca)
	idwc_inspectores.Retrieve(Integer(istr_mant.argumento[3]))

	dw_2.GetChild("prod_codigo", idwc_productores)
	idwc_productores.SetTransObject(sqlca)
	idwc_productores.Retrieve(Integer(istr_mant.argumento[3]))
	RETURN True
ELSE	
	RETURN False
END IF
end function

public function boolean existenumeroplanilla (string as_columna, string as_valor);Integer	li_planta, li_especie, li_variedad, li_packing, li_zona, &
         li_tecnico, li_inspector, li_tipoins, li_agronomo, li_estado
Long		ll_lote,ll_numero, ll_noguia, ll_productor
String	ls_Reslot
Date		ld_Fecemb, ld_fecins

ll_numero 		=	dw_2.Object.ccpe_numero[1]
li_Planta		=	dw_2.Object.plde_codigo[1]


CHOOSE CASE as_columna
	CASE "ccpe_numero"
		ll_numero 		=	Long(as_valor)

	CASE "plde_codigo"
		li_Planta 		=	Integer(as_valor)
		
END CHOOSE

SELECT   zona_codigo, prod_codigo, &
         espe_codigo, vari_codigo, ccag_codigo, ccin_codigo, ccpe_fechin, &
			ccti_codigo, ccpe_estado, max(ccpe_noguia)
	INTO	:li_zona, :ll_productor, :li_especie, &
			:li_variedad, :li_agronomo, :li_inspector, :ld_fecins, :li_tipoins,&
			:li_estado, :ll_noguia
	FROM	dbo.ctlcalplacuaninspuvaenc
	WHERE clie_codigo	=	:gstr_parempresa.empr_codexp
	AND	plde_codigo	=	:li_Planta
	AND	ccpe_numero	=	:ll_numero
	GROUP BY zona_codigo, prod_codigo, &
         espe_codigo, vari_codigo, ccag_codigo, ccin_codigo, ccpe_fechin, &
			ccti_codigo, ccpe_estado;	
	

IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla CTLCALPLACUANINSPUVAENC")
	RETURN True
ELSEIF sqlca.sqlcode	=	0 THEN
	
	Messagebox("Atención","Número de Planilla ya Existe Para Planta Digitada",Exclamation!)
//	//istr_mant.argumento[1]	= String(li_lote)
//	istr_mant.argumento[2]	= String(ll_numero)
//	istr_mant.argumento[3]	= String(li_zona)		
//	istr_mant.argumento[4]	= String(li_planta)
//	istr_mant.argumento[5]	= String(ld_fecins)
//	istr_mant.argumento[6]	= String(li_tipoins)
//	istr_mant.argumento[7]	= String(gi_CodExport)
//	istr_mant.argumento[8]	= String(ll_productor)
//	istr_mant.argumento[9]	= String(li_especie)
//	istr_mant.argumento[10]	= String(li_variedad)
//	istr_mant.argumento[11]	= String(li_agronomo)
//	istr_mant.argumento[12]	= String(li_inspector)
//	istr_mant.argumento[13]	= String(ll_noguia)
//	istr_mant.argumento[14]	= String(li_estado)
//	//istr_mant.argumento[15]	= ls_Reslot
//
//	dw_2.SetItem(1, "zona_codigo", li_zona)
//	dw_2.SetItem(1, "plde_codigo", li_planta)
//	dw_2.SetItem(1, "ccpe_fechin", ld_fecins)		
//	dw_2.SetItem(1, "prod_codigo", ll_productor)		
//	dw_2.SetItem(1, "espe_codigo", li_especie)
//	dw_2.SetItem(1, "vari_codigo", li_variedad)
//	dw_2.SetItem(1, "ccag_codigo", li_agronomo)		
//	dw_2.SetItem(1, "ccin_codigo", li_inspector)		
//	dw_2.SetItem(1, "ccpe_noguia", ll_noguia)	
//	dw_2.SetItem(1, "ccti_codigo", li_tipoins)	
//	dw_2.SetItem(1, "ccpe_estado", li_estado)
//	//dw_2.SetItem(1, "ccpe_reslot", ls_reslot)	
//	
//
//	dw_2.GetChild("plde_codigo", idwc_plantas)
//	idwc_plantas.SetTransObject(sqlca)
//	idwc_plantas.Retrieve(Integer(istr_mant.argumento[7]),1)
//
//	dw_3.GetChild("plde_codpak", idwc_packings)
//	idwc_packings.SetTransObject(sqlca)
//	idwc_packings.Retrieve(Integer(istr_mant.argumento[7]),2,Integer(istr_mant.argumento[3]))
//
//	dw_2.GetChild("ccag_codigo", idwc_agronomos)
//	idwc_agronomos.SetTransObject(sqlca)
//	idwc_agronomos.Retrieve(0,Integer(istr_mant.argumento[3]))
//
//	dw_2.GetChild("ccin_codigo", idwc_inspectores)
//	idwc_inspectores.SetTransObject(sqlca)
//	idwc_inspectores.Retrieve(Integer(istr_mant.argumento[3]))
//
//	dw_2.GetChild("prod_codigo", idwc_productores)
//	idwc_productores.SetTransObject(sqlca)
//	idwc_productores.Retrieve(Integer(istr_mant.argumento[7]),Integer(istr_mant.argumento[3]))
	RETURN True
ELSE	
	RETURN False
END IF
end function

public function integer nuevolote (integer cliente, integer planta);Integer  li_Contador, li_planta, li_especie, li_variedad, li_packing, li_tipoinsp
Long		ll_lote, ll_numero, ll_noguia, ll_productor
String	ls_Embalaje, ls_calibre
Date		ld_Fecemb

li_planta		=	dw_2.Object.plde_codigo[1]
ll_lote 			=	dw_2.Object.cclo_numero[1]
ll_numero 		=	dw_2.Object.ccpe_numero[1]
ll_productor	=	dw_2.Object.prod_codigo[1]
li_especie		=	dw_2.Object.espe_codigo[1]
li_variedad		=	dw_2.Object.vari_codigo[1] 
ls_embalaje		=	dw_3.Object.emba_codigo[1]
li_packing		=	dw_3.Object.plde_codpak[1]
ls_calibre		=	dw_3.Object.vaca_calibr[1]
ld_fecemb		=	dw_3.Object.cclo_fecemb[1]
ll_noguia		=	dw_3.Object.cclo_noguia[1]
li_tipoinsp		=	dw_2.Object.ccti_codigo[1]

IF NOT IsNull(li_Especie) AND NOT IsNull(li_Variedad) &
	AND NOT IsNull(ls_Calibre) AND NOT IsNull(ll_Productor) AND NOT IsNull(li_Packing) &
	AND NOT IsNull(ls_Embalaje) AND ld_FecEmb <> Date('01/01/1900') THEN

	IF li_tipoinsp	=	2	THEN
	
		SELECT	cclo_numero
			INTO	:ll_lote
			FROM	dbo.ctlcallotes
			WHERE	plde_codigo	=	:li_Planta
			AND   prod_codigo =  :ll_Productor
			AND   espe_codigo =  :li_Especie
			AND   vari_codigo =  :li_Variedad
			AND   emba_codigo =  :ls_Embalaje
			AND   vaca_calibr =  :ls_Calibre
			AND   plde_codpak =  :li_Packing
			AND   cclo_fecemb =  :ld_FecEmb
			AND   cclo_numpla	=  :ll_numero;
	ELSE
		SELECT	cclo_numero
			INTO	:ll_lote
			FROM	dbo.ctlcallotes
			WHERE	plde_codigo	=	:li_Planta
			AND   prod_codigo =  :ll_Productor
			AND   espe_codigo =  :li_Especie
			AND   vari_codigo =  :li_Variedad
			AND   emba_codigo =  :ls_Embalaje
			AND   vaca_calibr =  :ls_Calibre
			AND   plde_codpak =  :li_Packing
			AND   cclo_fecemb =  :ld_FecEmb
			AND   cclo_numpla	=  :ll_numero
			AND 	cclo_numero =  :ll_noguia;
	END IF
	
	IF Sqlca.SQLCode = -1 THEN	
		F_errorbasedatos(sqlca,"Lectura tabla CTLCALLOTES")
	ELSEIF ll_lote > 0 THEN	
		li_Contador = ll_lote
	ELSE
			SELECT	IsNull(Max(cclo_numero),0) +1
				INTO  :li_Contador
				FROM  dbo.ctlcallotes
				WHERE clie_codigo = :cliente
				AND   plde_codigo = :planta;

		IF sqlca.sqlcode = -1 THEN
			F_ErrorBaseDatos(sqlca,"Problemas en tabla CTLCALLOTES")
			RETURN 1
		END IF
	END IF
END IF

RETURN li_Contador
end function

public subroutine habilitaencab (boolean habilita);IF Habilita THEN
	
	dw_2.Object.zona_codigo.Protect				=	0
	dw_2.Object.plde_codigo.Protect				=	0
	dw_2.Object.ccpe_numero.Protect				=	0
	dw_2.Object.ccpe_fechin.Protect				=	0
	dw_2.Object.ccpe_noguia.Protect				=	0
	dw_2.Object.ccti_codigo.Protect				=	0
	dw_2.Object.prod_codigo.Protect				=	0
	dw_2.Object.espe_codigo.Protect				=	0
	dw_2.Object.vari_codigo.Protect				=	0
	dw_2.Object.ccag_codigo.Protect				=	0
	dw_2.Object.ccin_codigo.Protect				=	0
	dw_3.Object.plde_codpak.Protect				=	0
	dw_3.Object.emba_codigo.Protect				=	0
	dw_3.Object.cclo_fecemb.Protect				=	0
	dw_3.Object.cclo_tamlot.Protect				=	0
	dw_3.Object.vaca_calibr.Protect				=	0
	dw_2.Object.zona_codigo.BackGround.Color	=	RGB(255,255,255)
	dw_2.Object.plde_codigo.BackGround.Color	=	RGB(255,255,255)
	dw_2.Object.ccpe_numero.BackGround.Color	=	RGB(255,255,255)
	dw_2.Object.ccpe_fechin.BackGround.Color	=	RGB(255,255,255)
	dw_2.Object.ccpe_noguia.BackGround.Color	=	RGB(255,255,255)
	dw_2.Object.ccti_codigo.BackGround.Color	=	RGB(255,255,255)
	dw_2.Object.prod_codigo.BackGround.Color	=	RGB(255,255,255)
	dw_2.Object.espe_codigo.BackGround.Color	=	RGB(255,255,255)
	dw_2.Object.vari_codigo.BackGround.Color	=	RGB(255,255,255)
	dw_2.Object.ccag_codigo.BackGround.Color	=	RGB(255,255,255)
	dw_2.Object.ccin_codigo.BackGround.Color	=	RGB(255,255,255)
	dw_3.Object.vaca_calibr.BackGround.Color	=	RGB(255,255,255)
	dw_3.Object.plde_codpak.BackGround.Color	=	RGB(255,255,255)
	dw_3.Object.emba_codigo.BackGround.Color	=	RGB(255,255,255)
	dw_3.Object.cclo_fecemb.BackGround.Color	=	RGB(255,255,255)
	
ELSE
	
	dw_2.Object.zona_codigo.Protect				=	1
	dw_2.Object.plde_codigo.Protect				=	1
	dw_2.Object.ccpe_numero.Protect				=	1
	dw_2.Object.ccpe_fechin.Protect				=	1
	dw_2.Object.ccpe_noguia.Protect				=	1
	dw_2.Object.ccti_codigo.Protect				=	1
	dw_2.Object.prod_codigo.Protect				=	1
	dw_2.Object.espe_codigo.Protect				=	1
	dw_2.Object.vari_codigo.Protect				=	1
	dw_2.Object.ccag_codigo.Protect				=	1
	dw_2.Object.ccin_codigo.Protect				=	1
	dw_3.Object.plde_codpak.Protect				=	1
	dw_3.Object.emba_codigo.Protect				=	1
	dw_3.Object.cclo_fecemb.Protect				=	1	
	dw_3.Object.vaca_calibr.Protect				=	1
	dw_2.Object.zona_codigo.BackGround.Color	=	553648127
	dw_2.Object.plde_codigo.BackGround.Color	=	553648127
	dw_2.Object.ccpe_numero.BackGround.Color	=	553648127
	dw_2.Object.ccpe_fechin.BackGround.Color	=	553648127
	dw_2.Object.ccpe_noguia.BackGround.Color	=	553648127
	dw_2.Object.ccti_codigo.BackGround.Color	=	553648127
	dw_2.Object.prod_codigo.BackGround.Color	=	553648127
	dw_2.Object.espe_codigo.BackGround.Color	=	553648127
	dw_2.Object.vari_codigo.BackGround.Color	=	553648127
	dw_2.Object.ccag_codigo.BackGround.Color	=	553648127
	dw_2.Object.ccin_codigo.BackGround.Color	=	553648127
	dw_3.Object.vaca_calibr.BackGround.Color	=	553648127
	dw_3.Object.plde_codpak.BackGround.Color	=	553648127
	dw_3.Object.emba_codigo.BackGround.Color	=	553648127	
	dw_3.Object.cclo_fecemb.BackGround.Color	=	553648127
END IF
end subroutine

public subroutine habilitaingreso (string ls_columna);Boolean	lb_Estado = True

dw_2.AcceptText()
dw_3.AcceptText()
	
IF ls_Columna <> "ccti_codigo" AND &
	(dw_2.Object.ccti_codigo[1]) = 0 OR IsNull(dw_2.Object.ccti_codigo[1]) THEN
	lb_Estado	=	False
END IF

IF ls_Columna <> "prod_codigo" AND &
	(dw_2.Object.prod_codigo[1]) = 0 OR IsNull(dw_2.Object.prod_codigo[1]) THEN
	lb_Estado	=	False
END IF

//IF ls_Columna <> "ccag_codigo" AND &
//	(dw_2.Object.ccag_codigo[1]) = 0 OR IsNull(dw_2.Object.ccag_codigo[1]) THEN
//	lb_Estado	=	False
//END IF

IF ls_Columna <> "ccin_codigo" AND &
	(dw_2.Object.ccin_codigo[1]) = 0 OR IsNull(dw_2.Object.ccin_codigo[1]) THEN
	lb_Estado	=	False
END IF

IF ls_Columna <> "ccpe_numero" AND &
	(dw_2.Object.ccpe_numero[1]) = 0 OR IsNull(dw_2.Object.ccpe_numero[1]) THEN
	lb_Estado	=	False
END IF

IF ls_Columna <> "emba_codigo" AND &
	(dw_3.Object.emba_codigo[1]) = "" OR IsNull(dw_3.Object.emba_codigo[1]) THEN
	lb_Estado	=	False
END IF

IF ls_Columna <> "cclo_fecemb" AND &
	Date(istr_mant.argumento[5]) < Date(dw_3.Object.cclo_fecemb[1]) THEN
	lb_Estado	= False	
END IF 

IF ls_Columna <> "plde_codpak" AND &
	(dw_3.Object.plde_codpak[1]) = 0 OR IsNull(dw_3.Object.plde_codpak[1]) THEN
	lb_Estado	=	False
END IF

IF ls_Columna <> "vaca_calibr" AND &
	(dw_3.Object.vaca_calibr[1]) = '' OR IsNull(dw_3.Object.vaca_calibr[1]) THEN
	lb_Estado	=	False
END IF

pb_ins_det.Enabled	=	lb_Estado
pb_grabar.Enabled		=	lb_Estado
end subroutine

protected function boolean wf_actualiza_db (boolean borrando);Boolean	lb_AutoCommit, lb_Retorno

IF Not dw_2.uf_check_required(0) THEN RETURN False

IF Not dw_1.uf_validate(0) THEN RETURN False

lb_AutoCommit		=	sqlca.AutoCommit
sqlca.AutoCommit	=	False

IF Borrando THEN
	IF dw_1.Update(True, False) = 1 THEN
		IF dw_2.Update(True,False) =	1	THEN
			IF dw_3.Update(True, False) = 1 THEN
				Commit;
				
				IF sqlca.SQLCode <> 0 THEN
					F_ErrorBaseDatos(sqlca, This.Title)
				ELSE
					lb_Retorno	=	True
					
					dw_1.ResetUpdate()
					dw_2.ResetUpdate()
					dw_3.ResetUpdate()
				END IF
			ELSE
				F_ErrorBaseDatos(sqlca, This.Title)
				
				RollBack;
			END IF
		ELSE
			F_ErrorBaseDatos(sqlca, This.Title)
			
			RollBack;
		END IF
	ELSE
		F_ErrorBaseDatos(sqlca, This.Title)
		
		RollBack;
	END IF
ELSE
  
	IF dw_3.Update(True, False) = 1 THEN
		IF dw_2.Update(True,False) =	1	THEN
			IF dw_1.Update(True, False) = 1 THEN
				Commit;
				
				IF sqlca.SQLCode <> 0 THEN
					F_ErrorBaseDatos(sqlca, This.Title)
				ELSE
					lb_Retorno	=	True
					
					dw_1.ResetUpdate()
					dw_3.ResetUpdate()
					dw_2.ResetUpdate()
				END IF
			ELSE
				F_ErrorBaseDatos(sqlca, This.Title)
				
				RollBack;
			END IF
		ELSE
			F_ErrorBaseDatos(sqlca, This.Title)
			
			RollBack;
		END IF
	ELSE
		F_ErrorBaseDatos(sqlca, This.Title)
		
		RollBack;
	END IF
END IF

sqlca.AutoCommit	=	lb_AutoCommit

RETURN lb_Retorno
end function

public function boolean duplicado (string data);Integer li_grupo, Li_contador

li_grupo = Integer(data)		

SELECT	Count(*)
	INTO	:li_Contador
	FROM	dbo.admagrupousuario
	WHERE	grpo_codigo	=	:li_grupo;

IF sqlca.sqlcode = -1 THEN
	F_ErrorBaseDatos(sqlca,"No se pudo leer la tabla admagruposuario")
ELSEIF li_Contador = 0 THEN
	RETURN FALSE
ELSE
	RETURN TRUE
END IF
end function

public function boolean existenumerolote (string as_columna, string as_valor);Integer	 li_planta, li_especie, li_variedad, li_packing, li_zona,&
			li_NeoLote, li_TamLot, li_Contador, li_tipins, respuesta, li_Etiqueta
Long		ll_lote,ll_nfolio, ll_numero, ll_Fila, ll_noguia, ll_productor
String		ls_Embalaje, ls_calibre, ls_Null
Date		ld_Fecemb
Boolean	lb_Retorno = False

SetNull(ls_Null)

li_Zona			=	dw_2.Object.zona_codigo[1]
li_planta			=	dw_2.Object.plde_codigo[1]
ll_lote 			=	dw_2.Object.cclo_numero[1]
ll_numero 		=	dw_2.Object.ccpe_numero[1]
ll_productor		=	dw_2.Object.prod_codigo[1]
li_especie		=	dw_2.Object.espe_codigo[1]
li_variedad		=	dw_2.Object.vari_codigo[1]
ls_embalaje		=	dw_3.Object.emba_codigo[1]
li_packing		=	dw_3.Object.plde_codpak[1]
li_Etiqueta		=	dw_3.Object.etiq_codigo[1]
ls_calibre			=	dw_3.Object.vaca_calibr[1]
ld_fecemb		=	dw_3.Object.cclo_fecemb[1]
ll_noguia			=	dw_2.Object.ccpe_noguia[1]
li_tipins			=	dw_2.Object.ccti_codigo[1]

Choose Case as_columna
	Case "plde_codigo"
		li_planta		=	Integer(as_valor)
		
	Case "cclo_numero"
		ll_nfolio 		=	Long(as_valor)

	Case "ccpe_numero"
		ll_numero 		=	Long(as_valor)

	Case "prod_codigo"
		ll_productor	=	Long(as_valor)
		
	Case "espe_codigo"
		li_especie		=	Integer(as_valor)
		
	Case "vari_codigo"
		li_variedad		=	Integer(as_valor)
		
	Case "emba_codigo"
		ls_embalaje		=	as_valor
		
	Case "plde_codpak"
		li_packing		=	Integer(as_valor)
		
	Case "vaca_calibr"
		ls_calibre		=	as_valor
		
	Case "cclo_fecemb"
		ld_fecemb		=	Date(as_valor)		
			
End Choose

IF NOT IsNull(li_Especie) AND NOT IsNull(li_Variedad) &
	AND NOT IsNull(ls_Calibre) AND NOT IsNull(ll_Productor) &
	AND NOT IsNull(li_Packing) AND NOT IsNull(ls_Embalaje) AND &
	ld_FecEmb <> Date('01/01/1900') THEN

	istr_mant.argumento[2]	=	String(ll_Numero)
	istr_mant.argumento[3]	=	String(li_Zona)							
	istr_mant.argumento[4]	=	String(li_Planta)
	istr_mant.argumento[7]	=	String(gi_CodExport)
	istr_mant.argumento[8]	=	String(ll_Productor)
	istr_mant.argumento[9]	=	String(li_Especie)
	istr_mant.argumento[10]	=	String(li_Variedad)
	istr_mant.argumento[17]	=	ls_embalaje
	istr_mant.argumento[18]	=	String(ld_FecEmb)	
	istr_mant.argumento[20]	=	String(li_Packing)
	istr_mant.argumento[21]	=	ls_Calibre
	/* 
	Gener Lote: 1	====>	SI
	*/
	IF gstr_parlote.codgen = 1 THEN	
		IF li_tipins	=	1	THEN
		
			SELECT	cclo_numero
				INTO	:ll_lote
				FROM	dbo.ctlcallotes
				WHERE	plde_codigo	=	:li_Planta
				AND   prod_codigo =  :ll_Productor
				AND   espe_codigo =  :li_Especie
				AND   vari_codigo =  :li_Variedad
				AND   emba_codigo =  :ls_Embalaje
				AND   vaca_calibr =  :ls_Calibre
				AND   plde_codpak =  :li_Packing
				AND   cclo_fecemb =  :ld_FecEmb
				AND   cclo_numpla	=  :ll_numero
				And 	cclo_noguia =  :ll_noguia 
				And	etiq_codigo = :li_Etiqueta;
		ELSE
			SELECT	cclo_numero
				INTO	:ll_lote
				FROM	dbo.ctlcallotes
				WHERE	plde_codigo	=	:li_Planta
				AND   prod_codigo =  :ll_Productor
				AND   espe_codigo =  :li_Especie
				AND   vari_codigo =  :li_Variedad
				AND   emba_codigo =  :ls_Embalaje
				AND   vaca_calibr =  :ls_Calibre
				AND   plde_codpak =  :li_Packing
				AND   cclo_fecemb =  :ld_FecEmb
				AND   cclo_numpla	=  :ll_numero 
				And	etiq_codigo = :li_Etiqueta;
		END IF			
		
		IF Sqlca.SQLCode = -1 THEN	
			F_errorbasedatos(sqlca,"Lectura tabla CTLCALLOTES")
		ELSEIF ll_lote > 0 THEN		
			
			SELECT	Count(*)
				INTO	:li_Contador
				FROM	dbo.ctlcalplacuaninspuvaenc
				WHERE	plde_codigo =	:li_Planta
				AND   cclo_numero = :ll_lote
				AND   ccpe_numero = :ll_Numero;
			
			IF sqlca.SQLCode = -1 THEN
				F_errorbasedatos(sqlca,"Lectura tabla CTLCALPLACUANINSPUVAENC")
			ELSEIF li_Contador	>	0	THEN						
				istr_mant.argumento[1]	=	String(ll_lote)
				
				This.TriggerEvent("ue_recuperadatos")			
				lb_Retorno	=	True				
			ELSE
				IF dw_2.Object.ccti_codigo[1]	<>	3	THEN				
	//				li_NeoLote		=	NuevoLote(gi_CodExport,li_Planta)
	//				dw_2.SetItem(1,"cclo_numero",li_NeoLote)
					dw_3.SetITem(1,"clie_codigo",gi_CodExport)
					dw_3.SetItem(1,"plde_codigo",li_Planta)
	//				dw_3.SetItem(1,"cclo_numero",li_NeoLote)
					dw_3.SetITem(1,"prod_codigo",ll_Productor)
					dw_3.SetITem(1,"espe_codigo",li_Especie)
					dw_3.SetITem(1,"vari_codigo",li_Variedad)
					dw_3.SetITem(1,"emba_codigo",ls_Embalaje)
					dw_3.SetITem(1,"vaca_calibr",ls_Calibre)
					dw_3.SetITem(1,"cclo_fecemb",ld_FecEmb)					
					
					istr_mant.argumento[1]	=	String(li_NeoLote)
					
					lb_Retorno	=	False
				END IF			
			END IF
		ELSE
	//		li_NeoLote	=	NuevoLote(gi_CodExport,li_Planta)				
	//		istr_mant.Argumento[1]	=	String(li_NeoLote)			
	//		dw_2.SetItem(1,"cclo_numero",li_NeoLote)
			dw_3.SetITem(1,"clie_codigo",gi_CodExport)
			dw_3.SetITem(1,"plde_codigo",li_Planta)
	//		dw_3.SetItem(1,"cclo_numero",li_NeoLote)
			dw_3.SetItem(1,"prod_codigo",ll_Productor)
			dw_3.SetItem(1,"espe_codigo",li_Especie)
			dw_3.SetItem(1,"vari_codigo",li_Variedad)
			dw_3.SetItem(1,"emba_codigo",ls_Embalaje)
			dw_3.SetItem(1,"vaca_calibr",ls_Calibre)
			dw_3.SetItem(1,"cclo_fecemb",ld_FecEmb)
			lb_Retorno	=	False
		END IF
	ELSE
		SELECT	cclo_numero, cclo_tamlot
			INTO	:ll_lote, :li_Tamlot
			FROM	dbo.ctlcallotes
			WHERE	plde_codigo	=	:li_Planta
			AND   prod_codigo =  :ll_Productor
			AND   espe_codigo =  :li_Especie
			AND   vari_codigo =  :li_Variedad
			AND   emba_codigo =  :ls_Embalaje
			AND   vaca_calibr =  :ls_Calibre
			AND   plde_codpak =  :li_Packing
			AND   cclo_fecemb =  :ld_FecEmb 
			And	etiq_codigo = :li_Etiqueta;
			
			IF Sqlca.SQLCode = -1 THEN	
				F_errorbasedatos(sqlca,"Lectura tabla CTLCALLOTES")
			ELSEIF ll_lote > 0 THEN	
				SELECT	Count(*)
					INTO	:li_Contador
					FROM	dbo.ctlcalplacuaninspuvaenc
					WHERE	plde_codigo = :li_Planta
					AND   cclo_numero = :ll_lote
					AND   ccpe_numero = :ll_Numero;
			
				IF sqlca.SQLCode = -1 THEN
					F_errorbasedatos(sqlca,"Lectura tabla CTLCALPLACUANINSPUVAENC")
				ELSEIF li_Contador	>	0	THEN						
					istr_mant.argumento[1]	=	String(ll_lote)
					
					This.TriggerEvent("ue_recuperadatos")			
							
					lb_Retorno	=	True
				ELSE
					ll_fila	= dw_3.Retrieve(gi_CodExport,li_Planta,ll_lote)
		
					IF ll_fila = -1 THEN
						respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
														Information!, RetryCancel!)
						lb_Retorno	=	False
					ELSE
						dw_1.Reset()
						lb_Retorno	=	True
					END IF
				END IF
			END IF
		END IF
END IF
RETURN lb_Retorno		
end function

public subroutine buscalote ();Integer	li_Zona, li_Planta, respuesta
Long		ll_Fila,ll_Lote,ll_productor

li_Zona      = dw_2.Object.zona_codigo[1]
li_Planta    = dw_2.Object.plde_codigo[1]
ll_Productor = dw_2.Object.prod_codigo[1]

IF (Isnull(li_zona) OR li_zona = 0) OR (Isnull(li_planta) OR li_planta = 0) OR (Isnull(ll_productor) OR ll_productor=0) THEN 
   Messagebox("Atención","Debe Ingresar Zona, Frigorífico y Productor")
	dw_2.setfocus()
	RETURN
ELSE
	
	istr_busq.argum[1]  = String(dw_2.object.plde_codigo[1])
	istr_busq.argum[2]  = String(dw_2.object.prod_codigo[1])
	istr_busq.argum[3]  = String(dw_2.object.clie_codigo[1])
 	istr_busq.argum[12] = String(dw_2.Object.vari_codigo[1])
	
	istr_busq.argum[1]  = String(Tab_1.TabPage_1.dw_4.object.plde_codigo[1])
	istr_busq.argum[2]  = String(Tab_1.TabPage_1.dw_4.object.prod_codigo[1])
	istr_busq.argum[3]  = String(Tab_1.TabPage_1.dw_4.object.clie_codigo[1])
 	istr_busq.argum[12] = String(Tab_1.TabPage_1.dw_4.Object.vari_codigo[1])
	istr_busq.argum[13] = String(dw_3.Object.vaca_calibr[1])
	istr_busq.argum[14] = String(dw_3.Object.cclo_fecemb[1])
	
	
	OpenWithParm(w_busc_lotes_cuantitativas, istr_busq)
	
	istr_busq	= Message.PowerObjectParm
	
	IF istr_busq.argum[2] <> "" THEN
		dw_2.Object.espe_codigo[1] =	Integer(istr_busq.argum[3])
		dw_2.Object.vari_codigo[1] =	Integer(istr_busq.argum[4])
		li_Planta							=	Integer(istr_busq.argum[10])
		ll_Lote							=	Long(istr_busq.argum[11])
		istr_mant.Argumento[1]		=	String(istr_busq.argum[11])		
		istr_mant.argumento[9]		=	istr_busq.argum[3]
		istr_mant.argumento[10]	=	istr_busq.argum[4]
		istr_mant.argumento[8]		=	istr_busq.argum[2]	
		istr_mant.argumento[17]	=	istr_busq.argum[5]
			
		ll_fila	= dw_3.Retrieve(gi_CodExport,li_Planta,ll_Lote)
		
		IF ll_fila = -1 THEN
			respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
											Information!, RetryCancel!)
			RETURN
		ELSE
			dw_1.Reset()
			RETURN
		END IF
		
	END IF
END IF

end subroutine

public subroutine limpia_data ();String ls_Nula
Setnull(ls_Nula)

dw_3.SetItem(1, "emba_codigo", ls_Nula)
dw_3.SetItem(1, "plde_codpak", Integer (ls_Nula))
dw_3.SetItem(1, "cclo_fecemb", Integer (ls_Nula))
dw_3.SetItem(1, "vaca_calibr", ls_Nula)
dw_3.SetItem(1, "cclo_tamlot", Integer (ls_Nula))
end subroutine

public subroutine trae_planilla (long al_planilla);Long 		ll_planilla, ll_lote
Integer	li_grupo, li_cliente, li_planta, li_estado, li_zona

SELECT clie_codigo, plde_codigo, ccpe_numero,
		 cclo_numero, ccpe_estado
  INTO :li_cliente,:li_planta, :ll_planilla,
  		 :ll_lote, :li_estado
  FROM dbo.ctlcalplacuaninspuvaenc
 WHERE ccpe_numero =	:al_planilla; 

IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla CTLCALPLACUANINSPUVAENC")
ELSEIF sqlca.sqlcode	=	0 THEN
		
	dw_2.retrieve(li_cliente,li_planta,ll_planilla,ll_lote)
	
	Tab_1.TabPage_1.dw_4.GetChild("plde_codigo", idwc_plantas)
	idwc_plantas.SetTransObject(sqlca)
	idwc_plantas.Retrieve(1)

	dw_3.GetChild("plde_codpak", idwc_packings)
	idwc_packings.SetTransObject(sqlca)
	idwc_packings.Retrieve(2,dw_2.Object.zona_codigo[1])

	Tab_1.TabPage_1.dw_4.GetChild("ccag_codigo", idwc_agronomos)
	idwc_agronomos.SetTransObject(sqlca)
	idwc_agronomos.Retrieve(dw_2.Object.zona_codigo[1])

	Tab_1.TabPage_1.dw_4.GetChild("ccin_codigo", idwc_inspectores)
	idwc_inspectores.SetTransObject(sqlca)
	idwc_inspectores.Retrieve(dw_2.Object.zona_codigo[1])

	Tab_1.TabPage_1.dw_4.GetChild("prod_codigo", idwc_productores)
	idwc_productores.SetTransObject(sqlca)
	idwc_productores.Retrieve(dw_2.Object.zona_codigo[1])
 
   	dw_3.Retrieve(li_cliente,li_planta,ll_lote)	
	dw_1.Retrieve(li_cliente,li_planta,ll_lote,ll_planilla)
END IF
end subroutine

on w_maed_ctlcalplanillauvas.create
int iCurrent
call super::create
this.dw_3=create dw_3
this.cb_aprobar=create cb_aprobar
this.tab_1=create tab_1
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_3
this.Control[iCurrent+2]=this.cb_aprobar
this.Control[iCurrent+3]=this.tab_1
end on

on w_maed_ctlcalplanillauvas.destroy
call super::destroy
destroy(this.dw_3)
destroy(this.cb_aprobar)
destroy(this.tab_1)
end on

event ue_seleccion;call super::ue_seleccion;String	ls_nula

SetNull(ls_nula)

istr_busq.argum[1]	=	String(gi_CodExport)
istr_busq.argum[2]	=	String(dw_2.Object.plde_codigo[1])

OpenWithParm(w_busc_ctlcalplacuaninspuvaenc, istr_busq)
istr_busq = Message.PowerObjectParm

	
IF istr_busq.argum[30] = '0' THEN
	istr_mant.argumento[1] 	= istr_busq.argum[1]
	istr_mant.argumento[2] 	= istr_busq.argum[2]
	istr_mant.argumento[3] 	= istr_busq.argum[3]
	istr_mant.argumento[4] 	= istr_busq.argum[4]
	istr_mant.argumento[5] 	= istr_busq.argum[5]	
	istr_mant.argumento[7] 	= istr_busq.argum[7]
	istr_mant.argumento[8] 	= istr_busq.argum[8]
	istr_mant.argumento[9] 	= istr_busq.argum[9]
	istr_mant.argumento[10] = istr_busq.argum[10]
	istr_mant.argumento[14]	= istr_busq.argum[14]
	istr_mant.argumento[17] = istr_busq.argum[17]
	istr_mant.argumento[18] = istr_busq.argum[18]
	istr_mant.argumento[20] = istr_busq.argum[20]
	istr_mant.argumento[21] = istr_busq.argum[21]
		
	dw_2.GetChild("plde_codigo", idwc_plantas)
	idwc_plantas.SetTransObject(sqlca)
	idwc_plantas.Retrieve(1)

	dw_3.GetChild("plde_codpak", idwc_packings)
	idwc_packings.SetTransObject(sqlca)
	idwc_packings.Retrieve(2,Integer(istr_mant.argumento[3]))

	dw_2.GetChild("ccag_codigo", idwc_agronomos)
	idwc_agronomos.SetTransObject(sqlca)
	idwc_agronomos.Retrieve(Integer(istr_mant.argumento[3]))

	dw_2.GetChild("ccin_codigo", idwc_inspectores)
	idwc_inspectores.SetTransObject(sqlca)
	idwc_inspectores.Retrieve()

	dw_2.GetChild("prod_codigo", idwc_productores) 
	idwc_productores.SetTransObject(sqlca)
	idwc_productores.Retrieve(Integer(istr_mant.argumento[3]))	
	This.TriggerEvent("ue_recuperadatos")
	//HabilitaEncab(False)
ELSE
	pb_buscar.SetFocus()
	HabilitaEncab(True)
END IF	
end event

event ue_recuperadatos;Long ll_fila_e, ll_fila_d, ll_fila_f, respuesta

DO
	dw_2.SetRedraw(False)
	dw_2.Reset()
	
	ll_fila_e	= dw_2.Retrieve(gi_CodExport,Integer(istr_mant.argumento[4]), & 
										Long(istr_mant.argumento[2]),Integer(istr_mant.argumento[1]))

	HabilitaEncab(True)
	dw_2.Object.prod_codigo.Protect				=	1
	dw_2.Object.espe_codigo.Protect				=	1
	dw_2.Object.vari_codigo.Protect				=	1
	dw_2.Object.prod_codigo.BackGround.Color	=	553648127
	dw_2.Object.espe_codigo.BackGround.Color	=	553648127
	dw_2.Object.vari_codigo.BackGround.Color	=	553648127
									
	dw_2.Object.ccpe_numero.protect = 1            
	dw_2.Object.ccpe_numero.BackGround.Color	=	553648127
	dw_2.Object.plde_codigo.protect = 1             
	dw_2.Object.plde_codigo.BackGround.Color	=	553648127	
	istr_mant.argumento[10] = String(dw_2.Object.vari_codigo[1])
	
	IF ll_fila_e = -1 THEN
		respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
										Information!, RetryCancel!)
	ELSE
		DO
		
			ll_fila_d	= dw_1.Retrieve(gi_CodExport,Integer(istr_mant.argumento[4]),&
												Integer(istr_mant.argumento[1]),Long(istr_mant.argumento[2]))

			IF ll_fila_d = -1 THEN
				respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
												Information!, RetryCancel!)
			ELSE
				
				DO
					ll_fila_f	= dw_3.Retrieve(gi_CodExport,Integer(istr_mant.argumento[4]),&
														 Integer(istr_mant.argumento[1]))
		
					IF ll_fila_f = -1 THEN
						respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
														Information!, RetryCancel!)
					END IF		
					
					IF dw_3.Rowcount() > 0 THEN	
						istr_mant.Argumento[19]	=	String(dw_3.Object.cclo_tamlot[1])
						pb_grabar.Enabled	=	True
						pb_eli_det.Enabled	=	True
						pb_imprimir.Enabled  =	True
						cb_aprobar.Enabled	=	True
						dw_1.SetRow(1)
						dw_1.SelectRow(1,True)
						dw_1.SetFocus()	
						dw_2.Enabled	=	True
						dw_3.Enabled	=	True
						IF istr_mant.argumento[14] = '2'THEN							
							dw_2.Enabled = False							
							dw_3.Enabled = False									
							istr_mant.Solo_Consulta = True 											
						END IF 
						
						Tab_1.TabPage_1.dw_4.GetChild("ccag_codigo", idwc_agronomos)
						idwc_agronomos.SetTransObject(sqlca)
						idwc_agronomos.Retrieve(Tab_1.TabPage_1.dw_4.Object.zona_codigo[1])
						
						Tab_1.TabPage_1.dw_4.GetChild("prod_codigo", idwc_productores)
						idwc_productores.SetTransObject(sqlca)
						idwc_productores.Retrieve(Tab_1.TabPage_1.dw_4.Object.zona_codigo[1])		
						
						istr_mant.argumento[8]	=	String(Tab_1.TabPage_1.dw_4.Object.prod_codigo[1])
					END IF 	
					
				LOOP WHILE respuesta = 1
				pb_eliminar.Enabled	= True
				pb_grabar.Enabled		= True
				pb_ins_det.Enabled	= True
			END IF
			
		IF ll_fila_d > 0 THEN	
			
			pb_grabar.Enabled	=	True
			pb_eli_det.Enabled	=	True
			pb_imprimir.Enabled 	=	True
			cb_aprobar.Enabled	=	True
			dw_1.SetRow(1)
			dw_1.SelectRow(1,True)
			dw_1.SetFocus()
			dw_2.Enabled = True
			dw_1.Enabled = True
			IF istr_mant.argumento[14]	=	'2'	THEN				
				dw_2.Enabled =	False
				dw_3.Enabled = False
				istr_mant.Solo_Consulta = True

				pb_eliminar.Enabled 	=	False
				pb_grabar.Enabled 	=	False
				pb_imprimir.Enabled 	=	True
				pb_ins_det.Enabled 	=	False
				pb_eli_det.Enabled 	=	False
				cb_aprobar.Enabled	=	False
				HabilitaEncab(False)
			END IF
		ELSE
			pb_ins_det.SetFocus()
		END IF
			
		LOOP WHILE respuesta = 1

		IF respuesta = 2 THEN Close(This)
	END IF
	dw_2.SetRedraw(True)
LOOP WHILE respuesta = 1

IF respuesta = 2 THEN Close(This)

end event

event ue_nuevo;Integer  li_Grupo
String   ls_Usuario
Long		ll_modif1, ll_modif2, ll_modif3

ls_Usuario	=	Upper(Gstr_Us.Nombre)
ib_ok	= True

istr_mant.Solo_Consulta	=	istr_mant.UsuarioSoloConsulta

IF Not istr_mant.Solo_Consulta THEN
	CHOOSE CASE wf_modifica()
		CASE -1
			ib_ok = False
		CASE 0
			ll_modif1	=	dw_1.GetNextModified(0, Primary!)
			ll_modif2	=	dw_2.GetNextModified(0, Primary!)
			ll_modif3	=	dw_3.GetNextModified(0, Primary!)		

			IF dw_1.RowCount() > 0 THEN
				CHOOSE CASE MessageBox("Grabar registro(s)","Desea Grabar la información ?", Question!, YesNoCancel!)
					CASE 1
						Message.DoubleParm = 0
						This.TriggerEvent("ue_guardar")
						IF message.DoubleParm = -1 THEN ib_ok = False
					CASE 3
						ib_ok	= False
						RETURN
				END CHOOSE
			END IF
	END CHOOSE
END IF

IF Not ib_ok THEN RETURN

dw_1.Reset()

pb_eli_det.Enabled		=	False
pb_ins_det.Enabled		=	False
pb_grabar.Enabled			=	False
pb_eliminar.Enabled		=	False
pb_imprimir.Enabled		=	False
dw_2.Enabled				=	True
dw_3.Enabled				=	True

dw_2.SetRedraw(False)
dw_2.Reset()
dw_2.InsertRow(0)
dw_2.SetRedraw(True)
dw_2.SetFocus()
dw_2.SetColumn(0)

dw_3.SetRedraw(False)
dw_3.Reset()
dw_3.InsertRow(0)
dw_3.SetRedraw(True)

dw_2.SetItem(1, "clie_codigo",integer(istr_mant.argumento[7]))
dw_2.SetItem(1, "plde_codigo",integer(istr_mant.argumento[4]))
dw_2.SetItem(1, "zona_codigo",integer(istr_mant.argumento[3]))

dw_2.SetItem(1, "espe_codigo",integer(istr_mant.argumento[9]))
dw_2.SetItem(1, "vari_codigo",integer(istr_mant.argumento[10]))
dw_2.SetItem(1, "ccpe_fechin",date(istr_mant.argumento[5]))

dw_3.SetItem(1, "emba_codigo",istr_mant.argumento[17])

dw_2.GetChild("prod_codigo", idwc_productores)
idwc_productores.SetTransObject(sqlca)
idwc_productores.Retrieve(Integer(istr_mant.Argumento[3]))
idwc_productores.SetSort("prod_nombre A")
idwc_productores.Sort()

li_Grupo = BuscaGrupo(Upper(Gstr_Us.Nombre))

If (li_Grupo > 2) Then
	pb_eliminar.Visible	=	False
	TriggerEvent('resize')
End If

HabilitaEncab(True)
end event

event ue_nuevo_detalle;call super::ue_nuevo_detalle;Integer li_grupo
String ls_Usuario
istr_mant.borra			= False
istr_mant.agrega			= True

istr_mant.argumento[2] = string(dw_2.Object.ccpe_numero[1])

OpenWithParm(iw_mantencion, istr_mant)

IF dw_1.RowCount() > 0 THEN
	pb_grabar.Enabled		= True
	cb_aprobar.enabled	=	True
END IF

 li_Grupo = BuscaGrupo(ls_Usuario)
 IF li_Grupo	=	1 OR li_Grupo	=	6 THEN
	 pb_eli_det.Enabled	= True
END IF	 
dw_1.SetRow(il_fila)
dw_1.SelectRow(il_fila,True)

end event

event open;
/*Argumentos
1:Número de Lote
2:Número de Planilla
istr_mant.Argumento[3]			=	Codigo Zona
istr_mant.argumento[4]			=	String(gi_CodPlanta)
istr_mant.argumento[5]			=	String(Today())
istr_mant.argumento[11]			=	agronomo
istr_mant.argumento[10]			=	Variedad
istr_mant.argumento[12]			=	inspector
istr_mant.argumento[13]			=	nro.guía
istr_mant.argumento[14]			=	Estado de Planilla 1=Pendiente; 2=Cerrada
istr_mant.argumento[15]			=	Resolución Lote (encabezado de planilla)
istr_mant.argumento[17]			=	Embalaje
istr_mant.argumento[18]			=	String(Today())
istr_mant.argumento[19]			=	tamaño lote
istr_mant.argumento[20]			=	packing
istr_mant.argumento[21]			=	calibre
istr_mant.Argumento[22]			=	dw_1.Retrieve > 0 => 1 ; 0
*/
x				= 0
y				= 0
This.Height	= 2470
im_menu		= m_principal

This.Icon										=	Gstr_apl.Icono
This.ParentWindow().ToolBarVisible	=	True
im_menu.Item[1].Item[6].Enabled		=	True
im_menu.Item[7].Visible					=	True

dw_1.SetTransObject(sqlca)
dw_2.SetTransObject(sqlca)
dw_3.SetTransObject(sqlca)

dw_1.Modify("datawindow.message.title='Error '+ is_titulo")
dw_1.SetRowFocusIndicator(Hand!)

istr_mant.dw						=	dw_1
istr_Mant.dw2						=	dw_2

//PROBLEMA 
istr_mant.UsuarioSoloConsulta	=	OpcionSoloConsulta()

pb_nuevo.PostEvent(Clicked!)

//GrabaAccesoAplicacion(True, id_FechaAcceso, it_HoraAcceso, This.Title, "Acceso a Aplicación", 1)

buscar	= "Código:Ncodigo,Descripción:Sconcepto"
ordenar	= "Código:codigo,Descripción:concepto"

iuo_especies			=	Create uo_especie					
iuo_ctlcalinspectores  =  Create uo_ctlcalinspectores		
iuo_plantadesp			=  Create uo_plantadesp				
iuo_productores	   	=  Create uo_productores				
iuo_ctlcaltipoinspec	=  Create uo_ctlcaltipoinspec		
iuo_variedades			=	Create uo_variedades				
iuo_zonas				=  Create uo_zonas						
iuo_embalajesprod	=  Create uo_embalajesprod
iuo_Objetados			=	Create uo_loteobjetadopendiente

dw_2.ShareData(Tab_1.TabPage_1.dw_4)
dw_2.ShareData(Tab_1.TabPage_2.dw_5)

Tab_1.TabPage_1.dw_4.GetChild("zona_codigo", idwc_zonas)
idwc_zonas.SetTransObject(sqlca)
If idwc_zonas.Retrieve(0) = 0 Then idwc_zonas.InsertRow(0)

idwc_zonas.SetSort("zona_nombre A")
idwc_zonas.Sort()
Tab_1.TabPage_1.dw_4.SetItem(1, "zona_codigo",gi_codZona)

Tab_1.TabPage_1.dw_4.GetChild("plde_codigo", idwc_plantas)
//dw_2.GetChild("plde_codigo", idwc_plantas)
idwc_plantas.SetTransObject(sqlca)
idwc_plantas.Retrieve(1)
idwc_plantas.SetSort("plde_nombre A")
idwc_plantas.Sort()
Tab_1.TabPage_1.dw_4.SetItem(1, "plde_codigo",gi_codPlanta)

Tab_1.TabPage_1.dw_4.GetChild("prod_codigo", idwc_productores)
idwc_productores.SetTransObject(sqlca)
idwc_productores.Retrieve(gi_codZona)
idwc_productores.SetSort("prod_nombre A")
idwc_productores.Sort()
Tab_1.TabPage_1.dw_4.SetItem(1, "prod_codigo",gi_codProductor)

Tab_1.TabPage_1.dw_4.GetChild("espe_codigo", idwc_especies)
idwc_especies.SetTransObject(sqlca)
idwc_especies.Retrieve()
idwc_especies.SetSort("espe_nombre A espe_codigo A")
idwc_especies.Sort()
Tab_1.TabPage_1.dw_4.SetItem(1, "espe_codigo",11)

Tab_1.TabPage_1.dw_4.GetChild("vari_codigo", idwc_variedades)
//dw_2.GetChild("vari_codigo", idwc_variedades)
idwc_variedades.SetTransObject(sqlca)
idwc_variedades.Retrieve(11,0)
idwc_variedades.SetSort("vari_nombre A")
idwc_variedades.Sort()
Tab_1.TabPage_1.dw_4.SetItem(1, "vari_codigo",2)

Tab_1.TabPage_1.dw_4.GetChild("ccag_codigo", idwc_agronomos)
idwc_agronomos.SetTransObject(sqlca)

If idwc_agronomos.Retrieve(gi_codZona) = 0 Then idwc_agronomos.InsertRow(0)
idwc_agronomos.SetSort("ccag_nombre A")
idwc_agronomos.Sort()

Tab_1.TabPage_1.dw_4.GetChild("ccin_codigo", idwc_inspectores)
//dw_2.GetChild("ccin_codigo", idwc_inspectores)
idwc_inspectores.SetTransObject(sqlca)
idwc_inspectores.Retrieve()
idwc_inspectores.SetSort("ccin_nombre A")
idwc_inspectores.Sort()
idwc_inspectores.InsertRow(0)

dw_3.GetChild("plde_codpak", idwc_packings)
idwc_packings.SetTransObject(sqlca)
/*No filtra packing por zona, solicitado por V.C.*/
idwc_packings.Retrieve(2,0)
//idwc_packings.Retrieve(2,gi_codZona)
idwc_packings.SetSort("plde_nombre A")
idwc_packings.Sort()
dw_3.SetItem(1, "emba_codigo",gs_CodEmbalaje)

dw_3.GetChild("vaca_calibr", idwc_calibres)
idwc_calibres.SetTransObject(sqlca)
idwc_calibres.Retrieve(11,2)
idwc_calibres.SetSort("vaca_calibr A")
idwc_calibres.Sort()

dw_3.GetChild("emba_codigo", idwc_embalajes)
idwc_embalajes.SetTransObject(sqlca)
idwc_embalajes.Retrieve(gi_CodExport)
idwc_embalajes.SetFilter("espe_codigo = " + String(11))
idwc_embalajes.Filter()
idwc_embalajes.SetSort("emba_codigo A")
idwc_embalajes.Sort()

dw_2.SetITem(1,"ccti_codigo",1)

istr_mant.argumento[3]	=	String(gi_CodZona)
istr_mant.argumento[4]	=	String(gi_CodPlanta)
istr_mant.argumento[5]	=	String(Today())
istr_mant.argumento[7]	=	String(gi_CodExport)
istr_mant.argumento[8]	=	String(gi_CodProductor)
istr_mant.argumento[9]	=	'11'
istr_mant.argumento[10]	=	'2'
istr_mant.argumento[17]	=	'U000'
istr_mant.argumento[18]	=	String(Today())
istr_mant.argumento[19]	=	'0'
istr_mant.argumento[20]	=	'0'
istr_mant.argumento[21]	=	''
istr_mant.argumento[22]	=	'0'

If gstr_parlote.codgen = 1 Then dw_3.Object.buscalote.visible=false

end event

event ue_modifica_detalle;IF dw_1.RowCount()>0 THEN
	istr_mant.Agrega = False
	istr_mant.Borra  = False	
	OpenWithParm(iw_mantencion,istr_mant)
END IF

IF dw_1.GetItemStatus(il_Fila,0,Primary!) = DataModified! OR & 
	dw_1.GetItemStatus(il_Fila,0,Primary!) = NotModified! THEN
	istr_mant.Argumento[22]	=	'1'
END IF

end event

event ue_borra_detalle;IF dw_1.rowcount() < 1 THEN RETURN

SetPointer(HourGlass!)

ib_borrar = True
w_main.SetMicroHelp("Validando la eliminación de detalle...")

Message.DoubleParm = 0

This.TriggerEvent ("ue_validaborrar_detalle")

IF Message.DoubleParm = -1 THEN RETURN

istr_mant.borra	= True
istr_mant.agrega	= False

OpenWithParm(iw_mantencion, istr_mant)

istr_mant = Message.PowerObjectParm

IF istr_mant.respuesta = 1 THEN
	IF dw_1.DeleteRow(0) = 1 THEN
		ib_borrar = False

		w_main.SetMicroHelp("Borrando Registro...")
		SetPointer(Arrow!)
	ELSE
		ib_borrar = False
		MessageBox(This.Title,"No se puede borrar actual registro.")
	END IF
 IF dw_1.RowCount() = 0 THEN 
	pb_eli_det.Enabled = False
	pb_grabar.Enabled = False
End If
 
END IF

istr_mant.borra	 = False
end event

event ue_imprimir;SetPointer(HourGlass!)

Long		fila
DataWindowChild  dwc_packing

istr_info.titulo	=	"INFORME DE PLANILLAS INSPECCION"
istr_info.copias	=	1

OpenWithParm(vinf,istr_info)

//vinf.dw_1.DataObject	=	"dw_info_ctlcalplacuantitativa"
vinf.dw_1.DataObject	=	"dw_info_general_ctlcalplacuantitativa"

vinf.dw_1.SetTransObject(sqlca)

vinf.dw_1.GetChild("plde_codpak",dwc_packing)
dwc_packing.SetTransObject(sqlca)
dwc_packing.Retrieve(2,integer(istr_mant.Argumento[3])) 

//fila	=	vinf.dw_1.Retrieve(Integer(istr_mant.argumento[7]), Integer(istr_mant.argumento[4]), Long(istr_mant.argumento[2]))

fila	=	vinf.dw_1.Retrieve(gi_codexport, -1, Integer(istr_mant.argumento[4]), -1,Date('19000101'), Today(), -1, -1,&
								-1, -1, -1, Date('19000101'), Today(), 'Z', -1,'*', 'Z','Z','Z','Z','Z', 'Z', 0,&
								-1,-1, Long(istr_mant.argumento[2]))

If fila	=	-1 Then
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ElseIf fila	=	0 Then
	MessageBox( "No Existe información", "No existe información para este informe.", &
					StopSign!, Ok!)
Else
	F_Membrete(vinf.dw_1)
	 vinf.dw_1.Object.DataWindow.Zoom = 72				//LRBB 19.feb.2014
	If gs_Ambiente <> 'Windows' Then F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
End If

SetPointer(Arrow!)
end event

event resize;call super::resize;Integer	Maximo

maximo	= dw_1.width

If dw_2.width > maximo Then Maximo = dw_2.width

dw_2.x			= 37 + Round((This.WorkSpaceWidth() - 400 - dw_2.width) / 2, 0)
dw_2.y			= 37

Tab_1.x			=	dw_2.x
Tab_1.y			=	dw_2.y
Tab_1.Width	=	dw_2.Width + 40

dw_3.x			= 58 + Round((This.WorkSpaceWidth() - dw_3.width - 400) / 2, 0)
dw_3.y			= 45 + Tab_1.Height

dw_1.width		= This.WorkSpaceWidth() - 400
dw_1.x			= 37 + Round((This.WorkSpaceWidth() - 400 - dw_1.width) / 2, 0)
dw_1.y			= 64 + Tab_1.Height + dw_3.Height
dw_1.Height	= This.WorkSpaceHeight() - dw_1.y - 41


end event

event ue_antesguardar;Integer	li_secuencia, li_planta, li_especie,li_variedad, li_packing, &
			li_loteplanilla, li_Lote, li_Zona, li_Inspeccion
String	ls_embalaje, ls_calibre, ls_resolu
Long		ll_fila, ll_numero, ll_planilla, ll_noguia,ll_productor
Date		ld_fecemb

ll_numero	=	dw_2.Object.ccpe_numero[1]
li_planta		=	dw_2.Object.plde_codigo[1]
ll_productor	=	dw_2.Object.prod_codigo[1]
li_especie	=	dw_2.Object.espe_codigo[1]
li_variedad	=	dw_2.Object.vari_codigo[1]
ls_resolu		=	dw_2.Object.ccpe_reslot[1]
ls_embalaje	=	dw_3.Object.emba_codigo[1]
li_packing	=	dw_3.Object.plde_codpak[1]
ls_calibre	=	dw_3.Object.vaca_calibr[1]
ld_fecemb	=	dw_3.Object.cclo_fecemb[1]
li_Zona		=	dw_2.Object.zona_codigo[1]
li_inspeccion=	dw_2.Object.ccti_codigo[1]
ll_noguia		=	dw_2.Object.ccpe_noguia[1]


// bloqueo para grabación de tablas
UPDATE	dba.parempresa
	SET	empr_disco	=	:gstr_parempresa.empr_disco
WHERE	empr_rutemp	=	:gstr_parempresa.empr_rutemp;	
/* 
	Permite controlar la generación de nº de lote, si la opción es 1 se genera el lote.Esto
se controla en tablas\generación de lotes.*/
IF gstr_parlote.codgen = 1 THEN	
   li_lote	=	NuevoLote(gi_CodExport,li_Planta)
	istr_mant.Argumento[1]= String(li_lote)
//ELSE
//	istr_mant.argumento[1] =String(dw_3.Object.cclo_numero[1])
END IF

IF li_Inspeccion <> 2 THEN // No Packing
	dw_3.Setitem(1, "cclo_noguia",ll_noguia)
ELSE
	IF ls_resolu='A'	THEN
		dw_3.SetItem(1,"cclo_noguia",1)
	ELSE 	
		SELECT	max(ccpe_noguia)
			INTO	:ll_noguia
			FROM	dba.ctlcalplacuaninspuvaenc
			WHERE	ccpe_noguia > 1
			AND	ccti_codigo=2;
		
		IF sqlca.sqlcode = -1 THEN
			F_ErrorBaseDatos(sqlca,"No se pudo obtener máximo guía desde ue_antesguardar")
		ELSEIF ll_noguia > 1 THEN
			ll_noguia++
		END IF	
		
		IF IsNull(ll_noguia) or ll_noguia = 1	THEN
			ll_noguia = 2
		END IF
	dw_3.SetItem(1,"cclo_noguia",ll_noguia)	
	END IF
END IF
			
istr_mant.Argumento[3]	=	String(li_zona)

dw_2.SetItem(1, "cclo_numero", Integer(istr_mant.argumento[1]))
dw_2.setitem(1, "ccpe_estado", 1)/*estado pendiente*/
dw_3.SetItem(1, "cclo_numero", Integer(istr_mant.argumento[1]))
dw_3.SetItem(1, "clie_codigo", Integer(istr_mant.argumento[7]))
dw_3.SetItem(1, "plde_codigo", Integer(istr_mant.argumento[4]))
dw_3.SetItem(1, "prod_codigo", Long(istr_mant.argumento[8]))
dw_3.SetItem(1, "espe_codigo", Integer(istr_mant.argumento[9]))
dw_3.SetItem(1, "vari_codigo", Integer(istr_mant.argumento[10]))
dw_3.Object.cclo_catcon[1] = dw_2.Object.ccpe_catcon[1]

li_Lote	=	Integer(istr_mant.Argumento[1])

/*
Deja tomada la transacción hasta el próximo commit.
Así pueden guardar más de una persona a la vez.
*/
UPDATE	dba.ctlcalplacuaninspuvadet
	SET	ccpe_numero=0
	WHERE	1=2;

SELECT	IsNull(Max(ccpd_secuen), 0)
	INTO	:li_Secuencia
	FROM	dba.ctlcalplacuaninspuvadet
	WHERE	ccpe_numero	=	:ll_Numero 
	AND	plde_codigo =	:li_planta
	AND	cclo_numero	=	:li_lote;

// dw_1.GetItemStatus(ll_Fila, 0, Delete!)=DataModified!

FOR ll_Fila = 1 TO dw_1.RowCount()
	IF dw_1.GetItemStatus(ll_Fila, 0, Primary!) = NewModified! THEN
		li_Secuencia ++
		dw_1.Object.ccpe_numero[ll_Fila]		=	ll_Numero
		dw_1.Object.clie_codigo[ll_Fila]		=	dw_2.Object.clie_codigo[1]
		dw_1.Object.plde_codigo[ll_Fila]		=	li_Planta
		dw_1.Object.cclo_numero[ll_Fila]		=	li_Lote
		dw_1.Object.ccpd_secuen[ll_Fila]		=	li_Secuencia
		dw_1.Object.ccpe_noguia[ll_Fila]		=	ll_noguia
		dw_1.Object.espe_codigo[ll_Fila]		=	li_especie		
	END IF
	
NEXT
end event

event ue_guardar;String	ls_Usuario
Integer	li_Grupo, li_Planta, li_Folio, li_Fila, li_ContAprob, li_TotReg
Long		ll_nropalletactual, ll_nropalletnuevo, ll_totlot, ll_ccajas, ll_cajasrevisadas, ll_ccajasfalt

li_TotReg			=	0
li_ContAprob		=	0
ll_nropalletactual=	0
ll_nropalletnuevo	=	0
ll_totlot			=	0
ll_ccajas			=	0

ls_Usuario			=	Upper(Gstr_Us.Nombre)
istr_mant.borra	= 	False
istr_mant.agrega	= 	False
li_Grupo 			=	BuscaGrupo(ls_Usuario)

IF ((li_Grupo = 6 OR li_grupo = 1)) OR dw_1.GetItemStatus(il_Fila, 0, Primary!) = NewModified!  THEN 		
	istr_mant.Solo_Consulta = False
END IF

ll_totlot	=	dw_3.Object.cclo_tamlot[1]

FOR li_Fila = 1 TO dw_1.RowCount()	
	ll_nropalletnuevo = dw_1.Object.ccpd_npalle[li_Fila]

	IF ll_nropalletnuevo <> ll_nropalletactual THEN
		ll_nropalletactual  = dw_1.Object.ccpd_npalle[li_Fila]

		IF dw_1.Object.ccpd_respal[li_Fila] = 'A' THEN
			li_ContAprob	=	li_ContAprob + 1
			IF not isnull(dw_1.Object.paen_ccajas[li_Fila]) THEN
				ll_ccajas	=	ll_ccajas + dw_1.Object.paen_ccajas[li_Fila]
			END IF
		END IF
		
		ll_cajasrevisadas = ll_cajasrevisadas + dw_1.Object.paen_ccajas[li_Fila]
		
		li_TotReg = li_TotReg + 1
	END IF
		
NEXT

IF not isnull(li_TotReg) and li_TotReg > 0 THEN
	//tab_1.tabpage_1.dw_4.Object.ccpe_porapr[1]	=	(li_ContAprob / li_TotReg) * 100

	ll_ccajasfalt = dw_3.Object.cclo_tamlot[1] - ll_cajasrevisadas 
	
	ll_ccajas = ll_ccajasfalt + ll_ccajas
	
	tab_1.tabpage_1.dw_4.Object.ccpe_porapr[1] = (100 * ll_ccajas) /dw_3.Object.cclo_tamlot[1]
END IF
//

Message.DoubleParm = 0

OpenWithParm(w_mant_ctlcalplanillauvas_resolucionlot, istr_mant)
li_Grupo = BuscaGrupo(ls_Usuario)

IF  li_Grupo	=	1 OR li_Grupo	=	6 THEN 
	HabilitaEncab(True)
	dw_2.Object.prod_codigo.Protect					=	1
	dw_2.Object.espe_codigo.Protect					=	1
	dw_2.Object.vari_codigo.Protect					=	1
	dw_2.Object.prod_codigo.BackGround.Color		=	553648127
	dw_2.Object.espe_codigo.BackGround.Color		=	553648127
	dw_2.Object.vari_codigo.BackGround.Color		=	553648127
	dw_2.Object.ccpe_numero.protect					=	1            
	dw_2.Object.ccpe_numero.BackGround.Color		=	553648127
	dw_2.Object.plde_codigo.protect					=	1             
	dw_2.Object.plde_codigo.BackGround.Color		=	553648127
ELSE
 	HabilitaEncab(False)
END IF 

IF Message.DoubleParm = -1 THEN RETURN

call super::ue_guardar
end event

event ue_validaborrar();call super::ue_validaborrar;Long		ll_Cualitativa, ll_Cuantitativa, ll_LotesObjetados, ll_NroLote
Integer	li_Cliente, li_Planta

li_Planta	= Integer(istr_mant.argumento[4])
li_Cliente	= Integer(istr_mant.argumento[7])
ll_NroLote	= Long(istr_mant.argumento[1])

IF Message.DoubleParm = 1 THEN
	/*
	Verifica existencia de Lote en otras planillas
	*/
	SELECT	"Count"(*)
		INTO	:ll_Cuantitativa
		FROM	dba.ctlcalplacuaninspuvaenc
		WHERE	clie_codigo	=	:li_Cliente
		AND	plde_codigo	=	:li_Planta
		AND	cclo_numero	=	:ll_NroLote ;
	/*
	Verifica existencia de lote en la misma planilla
	*/
	SELECT	"Count"(*)
		INTO	:ll_Cualitativa
		FROM	dba.ctlcalplacualinspuvaenc
		WHERE	clie_codigo	=	:li_Cliente
		AND	plde_codigo	=	:li_Planta
		AND	cclo_numero	=	:ll_NroLote ;
	/*
	Verifica existencia de lote objetado
	*/
	SELECT	"Count"(*)
		INTO	:ll_LotesObjetados
		FROM	dba.ctlcalotesobjetadosdet
		WHERE	clie_codigo	=	:li_Cliente
		AND	plde_codigo	=	:li_Planta
		AND	cclo_numero	=	:ll_NroLote ;
	/*
	Elimina Lotes
	*/
	IF ll_Cuantitativa = 0 AND ll_Cualitativa = 0 AND ll_LotesObjetados = 0 THEN
		dw_3.DeleteRow(1)
	END IF
END IF
end event

event ue_borrar;IF dw_2.RowCount() < 1 THEN RETURN

If iuo_Objetados.Existe(dw_2.Object.clie_codigo[1], dw_2.Object.plde_codigo[1], &
							dw_3.Object.cclo_numero[1], dw_2.Object.ccpe_numero[1], False, SQLCA) Then 
	MessageBox('Error', 'No se puede eliminar la planilla:' + String (dw_2.Object.ccpe_numero[1], '00000000') + &
				',~r~npor que tiene Lotes Objetados Pendiente.~r~nResolucion:' + String (iuo_Objetados.ccte_numero, '00000000'), StopSign!, OK!)
	Return
End If

SetPointer(HourGlass!)

ib_borrar = True
w_main.SetMicroHelp("Validando la eliminación...")

Message.DoubleParm = 0

This.TriggerEvent ("ue_validaborrar")

IF Message.DoubleParm = -1 THEN RETURN

IF dw_1.RowCount() > 0 THEN dw_1.RowsMove(1,dw_1.RowCount(),Primary!,dw_1,1,Delete!)

IF dw_2.DeleteRow(0) = 1 THEN
		ib_borrar = False
		w_main.SetMicroHelp("Borrando Registro...")
		IF wf_actualiza_db(True) THEN
			w_main.SetMicroHelp("Registro Borrado...")
			This.TriggerEvent("ue_nuevo")
			SetPointer(Arrow!)
		ELSE
			w_main.SetMicroHelp("Registro no Borrado...")
		END IF			
ELSE
	ib_borrar = False
	MessageBox(This.Title,"No se puede borrar actual registro.")
END IF
end event

type dw_1 from w_mant_encab_deta_csd`dw_1 within w_maed_ctlcalplanillauvas
integer x = 14
integer y = 780
integer width = 4635
integer height = 1236
string title = "Planilla Verificación Uva de Mesa"
string dataobject = "dw_mues_ctlcalplanillauvas"
boolean hscrollbar = false
boolean hsplitscroll = true
end type

event dw_1::sqlpreview;//
end event

type dw_2 from w_mant_encab_deta_csd`dw_2 within w_maed_ctlcalplanillauvas
integer x = 773
integer y = 36
integer width = 3154
integer height = 496
string dataobject = "dw_maed_ctlcalplanillauvas"
end type

event dw_2::itemchanged;String	ls_Columna, ls_Nula, ls_NroGuia

SetNull(ls_Nula)

ls_Columna	=	dwo.Name

CHOOSE CASE ls_Columna

	CASE "zona_codigo"
		IF NOT iuo_zonas.existe(Integer(data),True,sqlca) THEN
			This.SetItem(1, ls_Columna, Long(ls_nula))
			RETURN 1
		ELSE
			istr_mant.argumento[3]	=	data
	         This.GetChild("ccag_codigo", idwc_agronomos)
			idwc_agronomos.SetTransObject(sqlca)
			idwc_agronomos.Retrieve(Integer(data))
		   	This.SetItem(1, "ccag_codigo", Long(ls_Nula))
			
         	This.GetChild("ccin_codigo", idwc_inspectores)
			idwc_inspectores.SetTransObject(sqlca)
			idwc_inspectores.Retrieve()
		   	This.SetItem(1, "ccin_codigo", Long(ls_Nula))
			
    			This.GetChild("prod_codigo", idwc_productores)
			idwc_productores.SetTransObject(sqlca)
			idwc_productores.Retrieve(Integer(istr_mant.argumento[3]))
		   	This.SetItem(1, "prod_codigo", Long(ls_Nula))
			limpia_data()
		END IF
		
	CASE "plde_codigo"
		  	IF Not iuo_plantadesp.existefrigo(Integer(data),True,sqlca) THEN
				This.SetItem(1, ls_Columna, Long(ls_nula))
				RETURN 1
			ELSE
				IF Existenumeroplanilla(ls_columna, data) THEN
					This.SetItem(1, "ccpe_numero", Long(ls_nula))
					RETURN 1
				END IF
				
				istr_mant.argumento[4]	=	data
			   IF Existenumerolote(ls_columna, data) THEN
				   This.SetItem(1, ls_Columna, ls_Nula)
				   RETURN 1
			   END IF
				limpia_data()
				
				dw_2.GetChild("prod_codigo", idwc_productores)
				idwc_productores.SetTransObject(sqlca)
				idwc_productores.Retrieve(Integer(istr_mant.argumento[3]))
				dw_2.SetItem(1, "prod_codigo", Long(ls_Nula))				
			END IF	
			
	CASE "ccpe_numero"
		Trae_Planilla(Long(data))
   		IF Existenumerolote(ls_columna, data) THEN
		END IF
			
	CASE "cclo_numero"
			istr_mant.argumento[1]	=	data
			IF Existenumerolote(ls_columna, data) THEN
				This.SetItem(1, ls_Columna, ls_Nula)
				RETURN 1
			END IF		
	CASE "ccpe_fechin"
			istr_mant.argumento[5]	=	data			
	
	CASE "ccti_codigo"	
		  IF Not iuo_ctlcaltipoinspec.existe(Integer(data),True,sqlca) THEN
			  This.SetItem(1, ls_Columna, Long(ls_nula))
			  RETURN 1
		  ELSE
			  istr_mant.argumento[6]	=	data
		  END IF

	CASE "prod_codigo"
		  IF Not iuo_productores.existe(Long(data),dw_2.Object.zona_codigo[1],True,sqlca) THEN
			  This.SetItem(1, ls_Columna, Long(ls_nula))
			  RETURN 1
		  ELSE
			  istr_mant.argumento[8]	=	data
			  limpia_data()
			  IF Existenumerolote(ls_columna, data) THEN
				   This.SetItem(1, ls_Columna, ls_Nula)				
				   RETURN 1
			  END IF							  
		  END IF

	CASE "espe_codigo"
		  IF Not iuo_especies.existe(Integer(data),True,sqlca) THEN
			  This.SetItem(1, ls_Columna, Integer(ls_nula))
			  RETURN 1
		  ELSE
			  istr_mant.argumento[9]	=	data
			  IF Existenumerolote(ls_columna, data) THEN
				   This.SetItem(1, ls_Columna, ls_Nula)
				   RETURN 1
			  END IF							  
           dw_2.GetChild("vari_codigo", idwc_variedades)
			  idwc_variedades.SetTransObject(sqlca)
			  idwc_variedades.Retrieve(Integer(istr_mant.argumento[9]))
			  dw_2.SetItem(1, "vari_codigo", Long(ls_Nula))
			  limpia_data()
        END IF
		  
	CASE "vari_codigo"
		  IF NOT iuo_variedades.existe(Integer(istr_mant.argumento[9]),Integer(data),True,sqlca) THEN
			  This.SetItem(1, ls_Columna, Long(ls_nula))
			  RETURN 1
		  ELSE
			  dw_3.SetItem(1, "vaca_calibr", ls_Nula)	
			  istr_mant.argumento[10]	=	data
			  IF Existenumerolote(ls_columna, data) THEN
				   This.SetItem(1, ls_Columna, ls_Nula)
				   RETURN 1
			  END IF							  
		     dw_3.GetChild("vaca_calibr", idwc_calibres)
			  idwc_calibres.SetTransObject(sqlca)
			  idwc_calibres.Retrieve(Integer(istr_mant.argumento[9]),&
										    Integer(istr_mant.argumento[10]))
			  limpia_data()
		     	
		  END IF							  

	CASE "ccag_codigo"
		  IF NoExisteAgronomo(Integer(data)) THEN
			  This.SetItem(1, ls_Columna, Long(ls_nula))
			  RETURN 1
		  ELSE  
		     istr_mant.argumento[11]	=	data			
	     END IF	 			  

	CASE "ccin_codigo"
		  IF NOT iuo_ctlcalinspectores.existe(sqlca,Integer(data),True) THEN
			  This.SetItem(1, ls_Columna, Long(ls_nula))
			  RETURN 1
		  ELSE  
		     istr_mant.argumento[12]	=	data	
		  END IF
					
	CASE "ccpe_noguia"	
			ls_NroGuia = Data
	      istr_mant.argumento[13]	=	data					
	
END CHOOSE

habilitaingreso(ls_Columna)
end event

type pb_nuevo from w_mant_encab_deta_csd`pb_nuevo within w_maed_ctlcalplanillauvas
integer x = 4741
integer y = 460
end type

type pb_eliminar from w_mant_encab_deta_csd`pb_eliminar within w_maed_ctlcalplanillauvas
integer x = 4741
integer y = 640
end type

type pb_grabar from w_mant_encab_deta_csd`pb_grabar within w_maed_ctlcalplanillauvas
integer x = 4741
integer y = 824
end type

type pb_imprimir from w_mant_encab_deta_csd`pb_imprimir within w_maed_ctlcalplanillauvas
integer x = 4741
integer y = 1000
end type

type pb_salir from w_mant_encab_deta_csd`pb_salir within w_maed_ctlcalplanillauvas
integer x = 4741
integer y = 1180
end type

type pb_ins_det from w_mant_encab_deta_csd`pb_ins_det within w_maed_ctlcalplanillauvas
integer x = 4745
integer y = 1472
integer taborder = 100
end type

type pb_eli_det from w_mant_encab_deta_csd`pb_eli_det within w_maed_ctlcalplanillauvas
integer x = 4745
integer y = 1644
integer taborder = 110
end type

type pb_buscar from w_mant_encab_deta_csd`pb_buscar within w_maed_ctlcalplanillauvas
integer x = 4741
integer y = 276
end type

type dw_3 from datawindow within w_maed_ctlcalplanillauvas
integer x = 754
integer y = 540
integer width = 3200
integer height = 224
integer taborder = 80
boolean bringtotop = true
string dataobject = "dw_mues_ctlcallotes"
boolean border = false
borderstyle borderstyle = stylelowered!
end type

event itemchanged;String	ls_Columna, ls_Nula
Integer	li_cont 
String	ls_Null

SetNull(ls_Nula)

ls_Columna	=	dwo.Name
dw_3.Object.cclo_numpla[1] = long(istr_mant.argumento[2])

CHOOSE CASE ls_Columna	
	CASE "emba_codigo"
		IF NOT iuo_embalajesprod.existe(Integer(istr_mant.argumento[7]),data,True,sqlca) THEN
			This.SetItem(1, ls_Columna, ls_Nula)			
			RETURN 1
		ELSE
			istr_mant.argumento[17]	=	data
			ExisteNumeroLote(ls_columna, data)
		END IF		
		
	CASE "cclo_fecemb"
		IF Date(istr_mant.Argumento[5]) < Date(Data) THEN
			MessageBox("ERROR"," Debe Ingrese Otra Fecha." + "~nLa Fecha de Embalaje no Puede " + & 
							"ser Superior a la Fecha de Inspección.", StopSign!, Ok!)
			pb_grabar.Enabled		= False
			pb_ins_det.Enabled	=	False
			dw_3.Object.vaca_calibr.Protect = 1
			dw_3.Object.plde_codpak.Protect = 1
			dw_3.Object.cclo_tamlot.Protect = 1
			dw_3.SetColumn(2)
			dw_3.SetFocus()
			Return 1		
		ELSE
			dw_3.Object.vaca_calibr.Protect = 0
			dw_3.Object.plde_codpak.Protect = 0
			dw_3.Object.cclo_tamlot.Protect = 0
			istr_mant.argumento[18]	=	data
			ExisteNumeroLote(ls_columna, data)
		END IF 	
		
	CASE "vaca_calibr"
		istr_mant.argumento[21]	=	data	
	   ExisteNumeroLote(ls_columna, data)
		
	CASE "plde_codpak"	
		
		IF NOT iuo_plantadesp.ExistePacking(Integer(data),True,sqlca) THEN
			This.SetItem(1, ls_Columna, Long(ls_nula))
			RETURN 1
		ELSE  	
			istr_mant.argumento[20]=data
			IF ExisteNumeroLote(ls_columna, data) THEN
				 This.Triggerevent("ue_recuperadatos")
			END IF 	 
		END IF					   		
		
	CASE "cclo_tamlot"	
		istr_mant.argumento[19]	=	Data		
		
END CHOOSE
HabilitaIngreso(ls_Columna)
end event

event losefocus;AcceptText()

RETURN 0
end event

event itemfocuschanged;IF IsValid(w_main) THEN
	w_main.SetMicroHelp(dwo.Tag)
END IF
end event

event rowfocuschanged;ib_datos_ok = true
if rowcount() < 1 or getrow() = 0 or ib_borrar then 
	ib_datos_ok = false
else
	il_fila = getrow()
end if

RETURN 0
end event

event itemerror;RETURN 1
end event

event buttonclicked;String	ls_columna

ls_columna = dwo.name

CHOOSE CASE ls_columna
	CASE "buscalote"
		buscalote()
		habilitaingreso(ls_Columna)
END CHOOSE
end event

type cb_aprobar from commandbutton within w_maed_ctlcalplanillauvas
boolean visible = false
integer x = 4681
integer y = 84
integer width = 265
integer height = 96
integer taborder = 90
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "Cerrar"
end type

event clicked;Integer	li_Grupo 

String ls_Usuario
ls_Usuario	=	Upper(Gstr_Us.Nombre)

dw_2.SetItem(1,"ccpe_estado",2)
dw_3.SetItem(1,"cclo_tamlot",Integer(istr_mant.Argumento[19]))


li_Grupo = BuscaGrupo(ls_Usuario)

IF li_Grupo	=	6 OR li_Grupo	=	1 THEN 
	dw_3.Object.cclo_tamlot.Protect				=	0
	dw_3.Object.cclo_tamlot.BackGround.Color	=	RGB(255,255,255)
ELSE
	dw_3.Object.cclo_tamlot.Protect				=	1
	dw_3.Object.cclo_tamlot.BackGround.Color	=	553648127
END IF	

IF dw_1.AcceptText() = -1 THEN RETURN

SetPointer(HourGlass!)

w_main.SetMicroHelp("Grabando información...")

Message.DoubleParm = 0

IF Message.DoubleParm = -1 THEN RETURN

IF wf_actualiza_db(False) THEN
	w_main.SetMicroHelp("Información Grabada.")
	pb_eliminar.Enabled	= True
	pb_imprimir.Enabled	= True
ELSE
	w_main.SetMicroHelp("No se puede Grabar información.")
	Message.DoubleParm = -1
	RETURN
END IF

MessageBox("","Planilla cerrada, el tamaño del lote no se podrá modificar")
end event

type tab_1 from tab within w_maed_ctlcalplanillauvas
integer x = 741
integer y = 24
integer width = 3186
integer height = 600
integer taborder = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
boolean italic = true
long backcolor = 16777215
boolean raggedright = true
boolean focusonbuttondown = true
integer selectedtab = 1
tabpage_1 tabpage_1
tabpage_2 tabpage_2
end type

on tab_1.create
this.tabpage_1=create tabpage_1
this.tabpage_2=create tabpage_2
this.Control[]={this.tabpage_1,&
this.tabpage_2}
end on

on tab_1.destroy
destroy(this.tabpage_1)
destroy(this.tabpage_2)
end on

type tabpage_1 from userobject within tab_1
integer x = 18
integer y = 112
integer width = 3150
integer height = 472
long backcolor = 30586022
string text = "Planilla"
long tabtextcolor = 33554432
long tabbackcolor = 12632256
long picturemaskcolor = 536870912
dw_4 dw_4
end type

on tabpage_1.create
this.dw_4=create dw_4
this.Control[]={this.dw_4}
end on

on tabpage_1.destroy
destroy(this.dw_4)
end on

type dw_4 from uo_dw within tabpage_1
integer x = 5
integer y = 8
integer width = 3145
integer height = 456
integer taborder = 11
string dataobject = "dw_maed_ctlcalplanillauvas_enca"
boolean vscrollbar = false
boolean border = false
end type

event itemchanged;call super::itemchanged;String	ls_Columna, ls_Nula, ls_NroGuia

SetNull(ls_Nula)

ls_Columna	=	dwo.Name

Choose Case ls_Columna
	Case "zona_codigo"
		If Not iuo_zonas.existe(Integer(data),True,sqlca) Then
			This.SetItem(1, ls_Columna, Long(ls_nula))
			Return 1
		ELSE
			istr_mant.argumento[3]	=	data
	         This.GetChild("ccag_codigo", idwc_agronomos)
			idwc_agronomos.SetTransObject(sqlca)
			idwc_agronomos.Retrieve(Integer(data))
		   	This.SetItem(1, "ccag_codigo", Long(ls_Nula))
			
         	This.GetChild("ccin_codigo", idwc_inspectores)
			idwc_inspectores.SetTransObject(sqlca)
			idwc_inspectores.Retrieve()
		   	This.SetItem(1, "ccin_codigo", Long(ls_Nula))
			
    			This.GetChild("prod_codigo", idwc_productores)
			idwc_productores.SetTransObject(sqlca)
			idwc_productores.Retrieve(Integer(istr_mant.argumento[3]))
		   	This.SetItem(1, "prod_codigo", Long(ls_Nula))
			limpia_data()
		End If
		
	Case "plde_codigo"
		  	If Not iuo_plantadesp.existefrigo(Integer(data),True,sqlca) Then
				This.SetItem(1, ls_Columna, Long(ls_nula))
				Return 1
			ELSE
				If Existenumeroplanilla(ls_columna, data) Then
					This.SetItem(1, "ccpe_numero", Long(ls_nula))
					Return 1
				End If
				
				istr_mant.argumento[4]	=	data
			   If Existenumerolote(ls_columna, data) Then
				   This.SetItem(1, ls_Columna, ls_Nula)
				   Return 1
			   End If
				limpia_data()
				
				This.GetChild("prod_codigo", idwc_productores)
				idwc_productores.SetTransObject(sqlca)
				idwc_productores.Retrieve(Integer(istr_mant.argumento[3]))
				This.SetItem(1, "prod_codigo", Long(ls_Nula))				
			End If	
			
	Case "ccpe_numero"
		Trae_Planilla(Long(data))
   		If Existenumerolote(ls_columna, data) Then
		End If
			
	Case "cclo_numero"
			istr_mant.argumento[1]	=	data
			If Existenumerolote(ls_columna, data) Then
				This.SetItem(1, ls_Columna, ls_Nula)
				Return 1
			End If		
	Case "ccpe_fechin"
			istr_mant.argumento[5]	=	data		

		     IF date(istr_mant.Argumento[5]) > date(f_fechahora()) THEN
                  MessageBox("ERROR"," La fecha de Inspección No Debe Ser Mayor a la Fecha de Digitación.", StopSign!, Ok!)
				Return 1		
			END IF
	
	Case "ccti_codigo"	
		  If Not iuo_ctlcaltipoinspec.existe(Integer(data),True,sqlca) Then
			  This.SetItem(1, ls_Columna, Long(ls_nula))
			  Return 1
		  ELSE
			  istr_mant.argumento[6]	=	data
		  End If

	Case "prod_codigo"
		  If Not iuo_productores.existe(Long(data),This.Object.zona_codigo[1],True,sqlca) Then
			  This.SetItem(1, ls_Columna, Long(ls_nula))
			  Return 1
		  ELSE
			  istr_mant.argumento[8]	=	data
			  limpia_data()
			  If Existenumerolote(ls_columna, data) Then
				   This.SetItem(1, ls_Columna, ls_Nula)				
				   Return 1
			  End If							  
		  End If

	Case "espe_codigo"
		  If Not iuo_especies.existe(Integer(data),True,sqlca) Then
			  This.SetItem(1, ls_Columna, Integer(ls_nula))
			  Return 1
		  ELSE
			  istr_mant.argumento[9]	=	data
			  If Existenumerolote(ls_columna, data) Then
				   This.SetItem(1, ls_Columna, ls_Nula)
				   Return 1
			  End If							  
           This.GetChild("vari_codigo", idwc_variedades)
			  idwc_variedades.SetTransObject(sqlca)
			  idwc_variedades.Retrieve(Integer(istr_mant.argumento[9]))
			  This.SetItem(1, "vari_codigo", Long(ls_Nula))
			  limpia_data()
        End If
		  
	Case "vari_codigo"
		  If Not iuo_variedades.existe(Integer(istr_mant.argumento[9]),Integer(data),True,sqlca) Then
			  This.SetItem(1, ls_Columna, Long(ls_nula))
			  Return 1
		  ELSE
			  dw_3.SetItem(1, "vaca_calibr", ls_Nula)	
			  istr_mant.argumento[10]	=	data
			  If Existenumerolote(ls_columna, data) Then
				   This.SetItem(1, ls_Columna, ls_Nula)
				   Return 1
			  End If							  
		     dw_3.GetChild("vaca_calibr", idwc_calibres)
			  idwc_calibres.SetTransObject(sqlca)
			  idwc_calibres.Retrieve(Integer(istr_mant.argumento[9]),&
										    Integer(istr_mant.argumento[10]))
			  limpia_data()
		     	
		  End If							  

	Case "ccag_codigo"
		  If NoExisteAgronomo(Integer(data)) Then
			  This.SetItem(1, ls_Columna, Long(ls_nula))
			  Return 1
		  ELSE  
		     istr_mant.argumento[11]	=	data			
	     End If	 			  

	Case "ccin_codigo"
		  If Not iuo_ctlcalinspectores.existe(sqlca,Integer(data),True) Then
			  This.SetItem(1, ls_Columna, Long(ls_nula))
			  Return 1
		  ELSE  
		     istr_mant.argumento[12]	=	data	
		  End If
					
	Case "ccpe_noguia"	
		ls_NroGuia = Data
	   	istr_mant.argumento[13]	=	data					
	
End Choose

habilitaingreso(ls_Columna)
end event

event itemerror;call super::itemerror;return 1
end event

type tabpage_2 from userobject within tab_1
integer x = 18
integer y = 112
integer width = 3150
integer height = 472
long backcolor = 30586022
string text = "Observación"
long tabtextcolor = 33554432
long tabbackcolor = 12632256
long picturemaskcolor = 536870912
dw_5 dw_5
end type

on tabpage_2.create
this.dw_5=create dw_5
this.Control[]={this.dw_5}
end on

on tabpage_2.destroy
destroy(this.dw_5)
end on

type dw_5 from uo_dw within tabpage_2
integer x = 9
integer y = 8
integer width = 3131
integer height = 456
integer taborder = 11
string dataobject = "dw_maed_ctlcalplanillauvas_obse"
boolean vscrollbar = false
boolean border = false
end type

