$PBExportHeader$w_maed_planilla_cualitativa_uva.srw
$PBExportComments$Mantenedor de Planilla Cualitativa
forward
global type w_maed_planilla_cualitativa_uva from w_mant_encab_deta_csd
end type
type dw_3 from uo_dw within w_maed_planilla_cualitativa_uva
end type
end forward

global type w_maed_planilla_cualitativa_uva from w_mant_encab_deta_csd
integer width = 3589
string title = "Planilla Cualitativa de Inspección de Uvas"
string menuname = ""
dw_3 dw_3
end type
global w_maed_planilla_cualitativa_uva w_maed_planilla_cualitativa_uva

type variables
w_mant_deta_ctlcalplacualinspuvadet	iw_mantencion

DataWindowChild idwc_zonas,idwc_plantas,idwc_productores,idwc_productor, idwc_especie, &
					 idwc_variedad,idwc_tecnicos,idwc_inspectores,idwc_packing,idwc_calibre, &
					 idwc_embalaje, idwc_embarques, idwc_recibidores

uo_ctlcalinspectores		iuo_ctlcalinspectores

str_ctlcallotes	istr_Lotes


String	is_Recibidor
Integer	ii_Planta			
end variables

forward prototypes
public subroutine deshabilit ()
public function boolean noexistezona (integer zona)
public function boolean noexisteplanta (integer planta)
public function boolean existevariedad (integer ai_cliente, integer ai_especie, integer ai_variedad)
public function boolean existecalibre (integer ai_cliente, integer ai_especie, integer ai_variedad, string as_calibre)
public function boolean existeembalaje (integer ai_cliente, string as_embalaje)
public function boolean existeembarqueprod (integer ai_cliente, string as_embarque)
public function boolean existeespecie (integer ai_cliente, integer ai_especie)
public function boolean existepacking (integer ai_packing)
public subroutine habilit ()
public subroutine habilitaencab (boolean habilita)
public subroutine noexisteplanilla (string as_columna, string as_valor)
public subroutine habilitaingreso (string columna)
public function boolean existenumerolote (string as_columna, string as_valor)
public function boolean existeproductores (integer ai_cliente, long al_productor)
public function boolean existerecibidores (integer ai_cliente, long al_recibidor)
protected subroutine buscalotes (long al_productor, integer ai_especie, integer ai_variedad, string as_embalaje, string as_calibre, integer ai_packing, date ad_fechaemb)
end prototypes

public subroutine deshabilit ();		dw_2.Object.zona_codigo.Protect				=	1
		dw_2.Object.ccce_lugari.Protect				=	1
		dw_2.Object.plde_codigo.Protect				=	1
		dw_2.Object.embq_codigo.Protect				=	1
		dw_2.Object.ccce_numero.Protect				=	1
		dw_2.Object.ccin_codigo.Protect				=	1
		dw_2.Object.ccce_agente.Protect				=	1
		dw_2.Object.ccce_nomrep.Protect				=	1
		dw_2.Object.ccce_fecarr.Protect				=	1				
		dw_3.Object.espe_codigo.Protect				=	1
		dw_3.Object.vari_codigo.Protect				=	1
		dw_3.Object.vaca_calibr.Protect				=	1
		dw_3.Object.prod_codigo.Protect				=	1
		dw_3.Object.plde_codpak.Protect				=	1
		dw_3.Object.emba_codigo.Protect				=	1
		dw_3.Object.cclo_fecemb.Protect				=	1
		dw_3.Object.cclo_tamlot.Protect				=	1
		dw_3.object.cclo_fecemb.Protect				=	1				
		dw_2.Object.zona_codigo.BackGround.Color	=	RGB(192,192,192)
		dw_2.Object.plde_codigo.BackGround.Color	=	RGB(192,192,192)
		dw_2.Object.embq_codigo.BackGround.Color	=	RGB(192,192,192)
		dw_2.Object.ccce_numero.BackGround.Color	=	RGB(192,192,192)
		dw_2.Object.ccin_codigo.BackGround.Color	=	RGB(192,192,192)
		dw_2.Object.ccce_agente.BackGround.Color	=	RGB(192,192,192)
		dw_2.Object.ccce_nomrep.BackGround.Color	=	RGB(192,192,192)
		dw_2.Object.ccce_fecarr.BackGround.Color	=	RGB(192,192,192)		
		dw_3.Object.espe_codigo.BackGround.Color	=	RGB(192,192,192)
		dw_3.Object.vari_codigo.BackGround.Color	=	RGB(192,192,192)
		dw_3.Object.vaca_calibr.BackGround.Color	=	RGB(192,192,192)
		dw_3.Object.prod_codigo.BackGround.Color	=	RGB(192,192,192)
		dw_3.Object.plde_codpak.BackGround.Color	=	RGB(192,192,192)
		dw_3.Object.emba_codigo.BackGround.Color	=	RGB(192,192,192)
		dw_3.Object.cclo_fecemb.BackGround.Color	=	RGB(192,192,192)
		dw_3.Object.cclo_fecemb.BackGround.Color	=	RGB(192,192,192)	
end subroutine

public function boolean noexistezona (integer zona);Integer li_Contador

SELECT Count(*)
	INTO :li_Contador
	FROM dba.zonas
	WHERE zona_codigo = :zona;

IF SqlCa.SqlCode = -1 THEN
	F_ErrorBaseDatos(SqlCa,"No se pudo leer la tabla Zonas")
	RETURN TRUE
ELSEIF li_Contador = 0 THEN
	Messagebox("Atención","Código de Zona No Existe. Ingrese o seleccione otro Código",Exclamation!)
	RETURN TRUE
ELSE
	RETURN FALSE
END IF
end function

public function boolean noexisteplanta (integer planta);Integer li_Contador, li_cliente,li_zona

li_cliente	=	gi_codexport

SELECT Count(*)
	INTO  :li_Contador
	FROM  dba.plantadesp
	WHERE clie_codigo = :li_cliente
	AND	plde_codigo = :planta
	AND	plde_tipopl	=	1;

IF sqlca.sqlcode = -1 THEN
	F_ErrorBaseDatos(sqlca,"No se pudo leer la tabla Plantas de Despachos")
	RETURN TRUE
ELSEIF li_Contador = 0 THEN
	MessageBox("Atención", "Código de Planta (" + String(planta, '0000') + &
	  			  "), para Zona especificada no ha sido~r" + "Ingresada en tabla respectiva." + &
				  "~r~rIngrese o seleccione otro Código de Planta.")
	RETURN TRUE
ELSE
	RETURN FALSE
END IF
end function

public function boolean existevariedad (integer ai_cliente, integer ai_especie, integer ai_variedad);String	ls_NombreVariedad

SELECT vari_nombre
	INTO	:ls_NombreVariedad
	FROM	dba.variedades
	WHERE	clie_codigo	=	:ai_Cliente
	AND	espe_codigo	=	:ai_Especie
	AND	vari_codigo	=	:ai_Variedad ;

IF SqlCa.SqlCode = -1 THEN
	F_ErrorBaseDatos(SqlCa,"Lectura de Tabla de Variedades")
	RETURN False
ELSEIF SqlCa.SQLCode = 100 THEN
	MessageBox("Atención", "Código de Especie/Variedad (" + String(ai_Especie, '00') + &
	"/" + String(ai_Variedad, '0000') + "), no ha sido ingresado en tabla respectiva" + &
	".~r~rIngrese o seleccione otra Variedad.")
	RETURN False
END IF

RETURN True
end function

public function boolean existecalibre (integer ai_cliente, integer ai_especie, integer ai_variedad, string as_calibre);String	ls_Calibre

SELECT vaca_calibr
	INTO	:ls_Calibre
	FROM	dba.variecalibre
	WHERE	clie_codigo	=	:ai_Cliente
	AND	espe_codigo	=	:ai_Especie
	AND	vari_codigo	=	:ai_Variedad
	AND	vaca_calibr =  :as_Calibre;

IF SqlCa.SqlCode = -1 THEN
	F_ErrorBaseDatos(SqlCa,"Lectura de Tabla de Variedades de Calibres")
	RETURN False
ELSEIF SqlCa.SQLCode = 100 THEN
	MessageBox("Atención", "Código de Variedad/Calibre (" + String(ai_Variedad, '0000') + &
	"/" + as_Calibre + "), no ha sido ingresado en tabla respectiva" + &
	".~r~rIngrese o seleccione otra Variedad.")
	RETURN False
END IF

RETURN True
end function

public function boolean existeembalaje (integer ai_cliente, string as_embalaje);String	ls_Embalaje

SELECT emba_nombre
	INTO	:ls_Embalaje
	FROM	dba.embalajes
	WHERE	clie_codigo	=	:ai_Cliente
	AND	emba_codigo	=	:as_Embalaje ;

IF SqlCa.SqlCode = -1 THEN
	F_ErrorBaseDatos(SqlCa,"Lectura de Tabla de Embalajes")
	RETURN False
ELSEIF SqlCa.SQLCode = 100 THEN
	MessageBox("Atención", "Código de Cliente/Embalaje (" + String(ai_Cliente, '000') + &
	"/" + as_Embalaje + "), no ha sido ingresado en tabla respectiva" + &
	".~r~rIngrese o seleccione otro Embalaje.")
	RETURN False
END IF

RETURN True
end function

public function boolean existeembarqueprod (integer ai_cliente, string as_embarque);String	ls_NombreNave

SELECT embq_nomnav
	INTO	:ls_NombreNave
	FROM	dba.embarqueprod
	WHERE	clie_codigo	=	:ai_Cliente
	AND	embq_codigo	=	:as_Embarque ;

IF SqlCa.SqlCode = -1 THEN
	F_ErrorBaseDatos(SqlCa,"Lectura de Tabla de EmbarqueProd")
	RETURN False
ELSEIF SqlCa.SQLCode = 100 THEN
	MessageBox("Atención", "Código de Cliente/Embarque (" + String(ai_Cliente, '000') + &
	"/" + as_Embarque + "), no ha sido ingresado en tabla respectiva" + &
	".~r~rIngrese o seleccione otro Embarque.")
	RETURN False
END IF

RETURN True
end function

public function boolean existeespecie (integer ai_cliente, integer ai_especie);Integer li_Contador

SELECT Count(*)
	INTO :li_Contador
	FROM dba.especies
	WHERE clie_codigo	= :ai_Cliente
	AND	espe_codigo = :ai_Especie;

IF SqlCa.SqlCode = -1 THEN
	F_ErrorBaseDatos(SqlCa,"No se puede leer la Tabla Especies")
	RETURN False
ELSEIF li_Contador = 0 THEN
	Messagebox("Atención","Código de Especie " + String(ai_Especie,'00') + &
				  "~rno ha sido ingresada en tabla respectiva." + &
				  "~r~rIngrese o seleccione otro Código", Exclamation!)
	RETURN False
ELSE
	RETURN True
END IF
end function

public function boolean existepacking (integer ai_packing);Integer	li_Contador, li_Cliente, li_Zona

li_Cliente	=	gi_codexport
li_Zona		=	dw_2.Object.zona_codigo[1]

SELECT Count(*)
	INTO  :li_Contador
	FROM  dba.plantadesp
	WHERE clie_codigo = :li_cliente
	AND	plde_codigo = :ai_Packing
	AND	zona_codigo	= :li_zona
	AND	plde_tipopl	=	2;

IF sqlca.sqlcode = -1 THEN
	F_ErrorBaseDatos(sqlca,"No se pudo leer la tabla Plantas de Despachos")
	RETURN FALSE
ELSEIF li_Contador = 0 THEN
	MessageBox("Atención", "Código de Packing (" + String(ai_Packing, '0000') + &
	  			  "), para Zona especificada no ha sido~r" + "Ingresada en tabla respectiva." + &
				  "~r~rIngrese o seleccione otro Código de Planta.")
	RETURN FALSE
ELSE
	RETURN TRUE
END IF
end function

public subroutine habilit ();	dw_2.Object.zona_codigo.Protect				=	0
	dw_2.Object.ccce_lugari.Protect				=	0
	dw_2.Object.plde_codigo.Protect				=	0
	dw_2.Object.embq_codigo.Protect				=	0
	dw_2.Object.ccce_numero.Protect				=	0
	dw_2.Object.ccin_codigo.Protect				=	0
	dw_2.Object.ccce_agente.Protect				=	0
	dw_2.Object.ccce_nomrep.Protect				=	0
	dw_3.Object.espe_codigo.Protect				=	0
	dw_3.Object.vari_codigo.Protect				=	0
	dw_3.Object.vaca_calibr.Protect				=	0
	dw_3.Object.prod_codigo.Protect				=	0
	dw_3.Object.plde_codpak.Protect				=	0
	dw_3.Object.emba_codigo.Protect				=	0
	dw_3.Object.cclo_fecemb.Protect				=	0
	dw_3.Object.cclo_tamlot.Protect				=	0
	dw_2.Object.zona_codigo.BackGround.Color	=	RGB(255,255,255)
	dw_2.Object.plde_codigo.BackGround.Color	=	RGB(255,255,255)
	dw_2.Object.embq_codigo.BackGround.Color	=	RGB(255,255,255)
	dw_2.Object.ccce_numero.BackGround.Color	=	RGB(255,255,255)
	dw_2.Object.ccin_codigo.BackGround.Color	=	RGB(255,255,255)
	dw_2.Object.ccce_agente.BackGround.Color	=	RGB(255,255,255)
	dw_2.Object.ccce_nomrep.BackGround.Color	=	RGB(255,255,255)
	dw_3.Object.espe_codigo.BackGround.Color	=	RGB(255,255,255)
	dw_3.Object.vari_codigo.BackGround.Color	=	RGB(255,255,255)
	dw_3.Object.vaca_calibr.BackGround.Color	=	RGB(255,255,255)
	dw_3.Object.prod_codigo.BackGround.Color	=	RGB(255,255,255)
	dw_3.Object.plde_codpak.BackGround.Color	=	RGB(255,255,255)
	dw_3.Object.emba_codigo.BackGround.Color	=	RGB(255,255,255)
	dw_3.Object.cclo_fecemb.BackGround.Color	=	RGB(255,255,255)

end subroutine

public subroutine habilitaencab (boolean habilita);//IF Habilita THEN
//	dw_2.Object.zona_codigo.Protect				=	0
//	dw_2.Object.ccce_lugari.Protect				=	0
//	dw_2.Object.plde_codigo.Protect				=	0
//	dw_2.Object.embq_codigo.Protect				=	0
//	dw_2.Object.ccce_numero.Protect				=	0
//	dw_2.Object.ccin_codigo.Protect				=	0
//	dw_2.Object.ccce_agente.Protect				=	0
//	dw_2.Object.ccce_nomrep.Protect				=	0
//	dw_3.Object.espe_codigo.Protect				=	0
//	dw_3.Object.vari_codigo.Protect				=	0
//	dw_3.Object.vaca_calibr.Protect				=	0
//	dw_3.Object.prod_codigo.Protect				=	0
//	dw_3.Object.plde_codpak.Protect				=	0
//	dw_3.Object.emba_codigo.Protect				=	0
//	dw_3.Object.cclo_fecemb.Protect				=	0
//	dw_3.Object.cclo_tamlot.Protect				=	0
//	dw_2.Object.zona_codigo.BackGround.Color	=	RGB(255,255,255)
//	dw_2.Object.plde_codigo.BackGround.Color	=	RGB(255,255,255)
//	dw_2.Object.embq_codigo.BackGround.Color	=	RGB(255,255,255)
//	dw_2.Object.ccce_numero.BackGround.Color	=	RGB(255,255,255)
//	dw_2.Object.ccin_codigo.BackGround.Color	=	RGB(255,255,255)
//	dw_2.Object.ccce_agente.BackGround.Color	=	RGB(255,255,255)
//	dw_2.Object.ccce_nomrep.BackGround.Color	=	RGB(255,255,255)
//	dw_3.Object.espe_codigo.BackGround.Color	=	RGB(255,255,255)
//	dw_3.Object.vari_codigo.BackGround.Color	=	RGB(255,255,255)
//	dw_3.Object.vaca_calibr.BackGround.Color	=	RGB(255,255,255)
//	dw_3.Object.prod_codigo.BackGround.Color	=	RGB(255,255,255)
//	dw_3.Object.plde_codpak.BackGround.Color	=	RGB(255,255,255)
//	dw_3.Object.emba_codigo.BackGround.Color	=	RGB(255,255,255)
//	dw_3.Object.cclo_fecemb.BackGround.Color	=	RGB(255,255,255)
//ELSE
//	dw_2.Object.zona_codigo.Protect				=	1
//	dw_2.Object.ccce_lugari.Protect				=	1
//	dw_2.Object.plde_codigo.Protect				=	1
//	dw_2.Object.embq_codigo.Protect				=	1
//	dw_2.Object.ccce_numero.Protect				=	1
//	dw_2.Object.ccin_codigo.Protect				=	1
//	dw_2.Object.ccce_agente.Protect				=	1
//	dw_2.Object.ccce_nomrep.Protect				=	1
//	dw_3.Object.espe_codigo.Protect				=	1
//	dw_3.Object.vari_codigo.Protect				=	1
//	dw_3.Object.vaca_calibr.Protect				=	1
//	dw_3.Object.prod_codigo.Protect				=	1
//	dw_3.Object.plde_codpak.Protect				=	1
//	dw_3.Object.emba_codigo.Protect				=	1
//	dw_3.Object.cclo_fecemb.Protect				=	1
//	dw_3.Object.cclo_tamlot.Protect				=	1
//	dw_2.Object.zona_codigo.BackGround.Color	=	RGB(192,192,192)
//	dw_2.Object.plde_codigo.BackGround.Color	=	RGB(192,192,192)
//	dw_2.Object.embq_codigo.BackGround.Color	=	RGB(192,192,192)
//	dw_2.Object.ccce_numero.BackGround.Color	=	RGB(192,192,192)
//	dw_2.Object.ccin_codigo.BackGround.Color	=	RGB(192,192,192)
//	dw_2.Object.ccce_agente.BackGround.Color	=	RGB(192,192,192)
//	dw_2.Object.ccce_nomrep.BackGround.Color	=	RGB(192,192,192)
//	dw_3.Object.espe_codigo.BackGround.Color	=	RGB(192,192,192)
//	dw_3.Object.vari_codigo.BackGround.Color	=	RGB(192,192,192)
//	dw_3.Object.vaca_calibr.BackGround.Color	=	RGB(192,192,192)
//	dw_3.Object.prod_codigo.BackGround.Color	=	RGB(192,192,192)
//	dw_3.Object.plde_codpak.BackGround.Color	=	RGB(192,192,192)
//	dw_3.Object.emba_codigo.BackGround.Color	=	RGB(192,192,192)
//	dw_3.Object.cclo_fecemb.BackGround.Color	=	RGB(192,192,192)
//END IF
//
end subroutine

public subroutine noexisteplanilla (string as_columna, string as_valor);Integer	li_Existe, li_Lotes, li_Numero, li_Planta, li_Cliente
Long		ll_NumPlanilla

li_Planta		=	dw_2.Object.plde_codigo[1]
ll_NumPlanilla	=	dw_2.Object.ccce_numero[1]
li_Cliente		=	dw_2.Object.clie_codigo[1]

CHOOSE CASE as_Columna
	CASE "clie_codigo"
		li_Cliente		=	Integer(as_Valor)
		
	CASE "plde_codigo"
		li_Planta		=	Integer(as_Valor)
		
	CASE "ccce_numero"
		ll_NumPlanilla	=	Long(as_Valor)
		
END CHOOSE

SELECT	Count(cclo_numero)
	INTO	:li_Lotes
	FROM	dba.ctlcalplacualinspuvaenc
	WHERE	ccce_numero =	:ll_NumPlanilla
	AND	clie_codigo	=	:li_Cliente
	AND   plde_codigo =  :li_Planta;

	istr_busq.Argum[1]	=	String(ll_NumPlanilla)
	istr_busq.Argum[2]	=	String(li_Cliente)
	istr_busq.Argum[3]	=	String(li_Planta)

	
IF SqlCa.SQLCode = -1 THEN
	F_errorbasedatos(SqlCa,"Lectura tabla Planilla de Cualitativa de Insp. Uvas")	
ELSEIF li_Lotes >	1	THEN
	OpenWithParm(w_busc_lotes_cualitativa, istr_busq)
	
	istr_busq	=	Message.PowerObjectParm
	
	dw_1.Retrieve(li_Cliente, li_Planta, Integer(istr_busq.Argum[11]), ll_NumPlanilla)
	dw_2.Retrieve(li_Cliente, li_Planta, Integer(istr_busq.Argum[11]), ll_NumPlanilla)
	dw_3.Retrieve(li_Cliente, Integer(istr_Busq.Argum[3]), Integer(istr_Busq.Argum[4]),&
					  Integer(istr_Busq.Argum[5]), istr_Busq.Argum[6],&
					  istr_Busq.Argum[7], Integer(istr_Busq.Argum[8]),&
					  Date(istr_Busq.Argum[9]))
					  
					  
	IF dw_2.Object.embq_codigo[1]	=	''	THEN				  
		istr_mant.Argumento[15]		=	dw_2.Object.embq_codigo[1]
	END IF
	
	dw_2.Object.ccce_numero.Protect				=	1
	dw_2.Object.ccce_numero.BackGround.Color	=	RGB(192,192,192)
	
ELSEIF li_Lotes =	1 THEN
	SELECT	cclo_numero
		INTO	:li_Numero
		FROM	dba.ctlcalplacualinspuvaenc
		WHERE	ccce_numero	=	:ll_NumPlanilla
		AND	clie_codigo	=	:li_Cliente
		AND	plde_codigo	=	:li_Planta;
	
	IF sqlca.sqlcode=-1 THEN
		F_ErrorBaseDatos(sqlca,"No se pude leer el encabezado~r " +& 
										"de la planilla cualitativa")
										
	ELSEIF sqlca.sqlcode=0 THEN
		dw_2.Retrieve(li_Cliente, li_Planta, li_Numero, ll_NumPlanilla)
		dw_1.Retrieve(li_Cliente, li_Planta, li_Numero, ll_NumPlanilla)
		
		istr_mant.Argumento[15]	=	String(dw_2.Object.embq_codigo[1])
		istr_mant.Argumento[16]	=	String(dw_2.Object.ccin_codigo[1])
		istr_mant.Argumento[17]	=	dw_2.Object.ccce_nomrep[1]
		
		dw_2.Object.ccce_numero.Protect				=	1
		dw_2.Object.ccce_numero.BackGround.Color	=	RGB(192,192,192)
		
		IF ExisteNumeroLote(as_Columna, as_Valor) THEN
			dw_3.InsertRow(0)
			dw_3.SetITem(1, "espe_codigo", gstr_Lotes.Especie)
			dw_3.SetITem(1, "vari_codigo", gstr_Lotes.Variedad)
			dw_3.SetITem(1, "vaca_calibr", gstr_Lotes.Calibre)
			dw_3.SetITem(1, "prod_codigo", gstr_Lotes.Productor)
			dw_3.SetITem(1, "plde_codpak", gstr_Lotes.Packing)
			dw_3.SetITem(1, "emba_codigo", gstr_Lotes.Embalaje)
			dw_3.SetITem(1, "cclo_fecemb", gstr_Lotes.FechaEmb)
			dw_3.SetITem(1, "cclo_tamlot", gstr_Lotes.Tamaño)
			dw_3.SetITem(1, "cclo_numero", gstr_Lotes.Lote)
		END IF
	END IF				
ELSE
	
IF istr_mant.argumento[4] <> '' THEN
	dw_2.SetItem(1, "ccce_numero", Date(istr_mant.argumento[4]))
END IF

dw_2.SetFocus()
dw_2.SetColumn("ccce_numero")
	
istr_mant.Argumento[17]	=	dw_2.Object.ccce_nomrep[1]			  
istr_mant.Argumento[16]	=	String(dw_2.Object.ccin_codigo[1])	

END IF
end subroutine

public subroutine habilitaingreso (string columna);Boolean	lb_Estado = True
Date		ld_Fecha


IF Columna <> "plde_codigo" AND &
	(dw_2.Object.plde_codigo[1] = 0 OR IsNull(dw_2.Object.plde_codigo[1])) THEN
	lb_Estado = False
END IF

IF Columna <> "ccce_numero" AND &
	(dw_2.Object.ccce_numero[1] = 0 OR IsNull(dw_2.Object.ccce_numero[1])) THEN
	lb_Estado = False
END IF

IF Columna <> "ccin_codigo" AND &
	(dw_2.Object.ccin_codigo[1] = 0 OR IsNull(dw_2.Object.ccin_codigo[1])) THEN
	lb_Estado = False
END IF

IF Columna <> "espe_codigo" AND &
	(dw_3.Object.espe_codigo[1] = 0 OR IsNull(dw_3.Object.espe_codigo[1])) THEN
	lb_Estado = False
END IF

IF Columna <> "vari_codigo" AND &
	(dw_3.Object.vari_codigo[1] = 0 OR IsNull(dw_3.Object.vari_codigo[1])) THEN
	lb_Estado = False
END IF

IF Columna <> "vaca_calibr" AND &
	(dw_3.Object.vaca_calibr[1] = "" OR IsNull(dw_3.Object.vaca_calibr[1])) THEN
	lb_Estado = False
END IF

IF Columna <> "prod_codigo" AND &
	(dw_3.Object.prod_codigo[1] = 0 OR IsNull(dw_3.Object.prod_codigo[1])) THEN
	lb_Estado = False
END IF

IF Columna <> "plde_codpak" AND &
	(dw_3.Object.plde_codpak[1] = 0 OR IsNull(dw_3.Object.plde_codpak[1])) THEN
	lb_Estado = False
END IF

IF Columna <> "emba_codigo" AND &
	(dw_3.Object.emba_codigo[1] = "" OR IsNull(dw_3.Object.emba_codigo[1])) THEN
	lb_Estado = False
	dw_3.Object.cclo_fecemb.SetFocus()
END IF

IF	Columna <> "cclo_fecemb" AND &
	NOT ISDate(String(dw_3.Object.cclo_fecemb[1])) THEN
	lb_Estado = False
	
END IF

IF	Columna <> "cclo_tamlot" AND &
	(dw_3.Object.cclo_tamlot[1] = 0 OR IsNull(dw_3.Object.cclo_tamlot[1])) THEN
	lb_Estado = False
END IF

IF	Columna <> "cclo_numero" AND &
	(dw_3.Object.cclo_numero[1] = 0 OR IsNull(dw_3.Object.cclo_numero[1])) THEN
	lb_Estado = False
END IF


pb_ins_det.Enabled = lb_estado
end subroutine

public function boolean existenumerolote (string as_columna, string as_valor);Integer	li_Existe, li_Cliente, li_Planta, li_Especie, li_Variedad, &
			li_Packing, li_TamLote, li_Planta1, li_Estado,li_NumeroLote
String	ls_Calibre, ls_Embalaje, ls_Null
Date		ld_FechaEmb
Long		ll_NumeroFolio,ll_Productor

SetNull(ls_Null)

li_Cliente		=	dw_2.Object.clie_codigo[1]
li_Planta		=	dw_2.Object.plde_codigo[1]
ll_NumeroFolio	=	dw_2.Object.ccce_numero[1]
li_Especie		=	dw_3.Object.espe_codigo[1]
li_Variedad		=	dw_3.Object.vari_codigo[1]
ls_Calibre		=	dw_3.Object.vaca_calibr[1]
ll_Productor	=	dw_3.Object.prod_codigo[1]
li_Packing		=	dw_3.Object.plde_codpak[1]
ls_Embalaje		=	dw_3.Object.emba_codigo[1]
ld_FechaEmb		=	dw_3.Object.cclo_fecemb[1]

CHOOSE CASE as_Columna
	CASE "espe_codigo"
		li_Especie		=	Integer(as_valor)

	CASE "vari_codigo"
		li_Variedad		=	Integer(as_valor)

	CASE "vaca_calibr"
		ls_Calibre 		=	as_valor

	CASE "prod_codigo"
		ll_Productor 	=	Long(as_valor)

	CASE "plde_codpak"
		li_Packing 		=	Integer(as_valor)

	CASE "emba_codigo"
		ls_Embalaje		=	as_valor

	CASE "cclo_fecemb"
		ld_FechaEmb		=	Date(as_valor)

	CASE "cclo_tamlot"
		li_TamLote		=	Integer(as_valor)

	CASE "ccce_numero"
		ll_NumeroFolio	=	Long(as_valor)

END CHOOSE

IF NOT IsNull(li_Cliente) AND NOT IsNull(li_Especie) AND NOT IsNull(li_Variedad) &
	AND NOT IsNull(ls_Calibre) AND NOT IsNull(ll_Productor) AND NOT IsNull(li_Packing) &
	AND NOT IsNull(ls_Embalaje) AND ld_FechaEmb <> Date('01/01/1900') THEN

	SELECT	plde_codigo, cclo_numero, cclo_tamlot, cclo_estado
		INTO	:li_Planta1, :li_NumeroLote, :li_TamLote, :li_Estado
		FROM	dba.ctlcallotes
		WHERE	clie_codigo	=	:li_Cliente
		AND	plde_codigo =	:li_Planta
		AND   prod_codigo =  :ll_Productor
		AND   espe_codigo =  :li_Especie
		AND   vari_codigo =  :li_Variedad
		AND   emba_codigo =  :ls_Embalaje
		AND   vaca_calibr =  :ls_Calibre
		AND   plde_codpak =  :li_Packing
		AND   cclo_fecemb =  :ld_FechaEmb;

	IF SqlCa.SQLCode = -1 THEN
		F_errorbasedatos(SqlCa,"Lectura tabla Lotes")
		RETURN False
	ELSEIF SqlCa.SqlCode	=	0 THEN
		
		dw_3.SetItem(1, "cclo_numero", li_NumeroLote)
		dw_3.SetItem(1, "cclo_tamlot", li_TamLote)	
		
		dw_2.SetItem(1, "cclo_numero", li_NumeroLote)
		dw_2.SetITem(1, "espe_codigo", li_Especie)
		dw_2.SetItem(1, "vari_codigo", li_Variedad)
		dw_2.SetITem(1, "prod_codigo", ll_Productor)
		
		SELECT	Count(*)
			INTO	:li_Existe
			FROM	dba.ctlcalplacualinspuvaenc
			WHERE	ccce_numero =	:ll_NumeroFolio
			AND	clie_codigo	=	:li_Cliente
			AND   plde_codigo =  :li_Planta
			AND	cclo_numero	=	:li_NumeroLote ;
		
		IF SqlCa.SQLCode = -1 THEN
			F_errorbasedatos(SqlCa,"Lectura tabla Planilla de Cualitativa de Insp. Uvas")
			RETURN False
		ELSEIF li_Existe > 0 THEN
			istr_Mant.Argumento[1]	= String(li_Cliente)
			istr_Mant.Argumento[3]	= String(li_Planta)
			istr_Mant.Argumento[4]	= String(ll_NumeroFolio)
			istr_Mant.Argumento[5]	= String(li_NumeroLote)
			
			This.TriggerEvent("ue_recuperadatos")						
		END IF
		
		RETURN True
	ELSE
		dw_3.SetItem(1, "cclo_numero", Long(ls_Null))
		
		RETURN False
	END IF
ELSE
	RETURN False
END IF
end function

public function boolean existeproductores (integer ai_cliente, long al_productor);String	ls_NombreProductor
Integer	li_Zona

li_Zona	=	dw_2.Object.zona_codigo[1]

SELECT prod_nombre
	INTO	:ls_NombreProductor
	FROM	dba.productores
	WHERE	clie_codigo	=	:ai_Cliente
	AND	zona_codigo	=	:li_Zona
	AND	prod_codigo	=	:al_Productor ;

IF SqlCa.SqlCode = -1 THEN
	F_ErrorBaseDatos(SqlCa,"Lectura de Tabla de Productores")
	RETURN False
ELSEIF SqlCa.SQLCode = 100 THEN
	MessageBox("Atención", "Código de Cliente/Productor (" + String(ai_Cliente, '000') + &
	"/" + String(al_Productor, '00000') + "), no ha sido ingresado en tabla respectiva" + &
	".~r~rIngrese o seleccione otro Productor.")
	RETURN False
END IF

RETURN True
end function

public function boolean existerecibidores (integer ai_cliente, long al_recibidor);
SELECT reci_nombre
	INTO	:is_Recibidor
	FROM	dba.recibidores
	WHERE	clie_codigo	=	:ai_Cliente
	AND	reci_codigo	=	:al_Recibidor ;

IF SqlCa.SqlCode = -1 THEN
	F_ErrorBaseDatos(SqlCa,"Lectura de Tabla de Recibidores")
	RETURN False
ELSEIF SqlCa.SQLCode = 100 THEN
	MessageBox("Atención", "Código de Cliente/Recibidor (" + String(ai_Cliente, '000') + &
	"/" + String(al_Recibidor, '00000') + "), no ha sido ingresado en tabla respectiva" + &
	".~r~rIngrese o seleccione otro Recibidor.")
	RETURN False
END IF

RETURN True
end function

protected subroutine buscalotes (long al_productor, integer ai_especie, integer ai_variedad, string as_embalaje, string as_calibre, integer ai_packing, date ad_fechaemb);Integer li_Existe, li_Cuenta,li_Lote

ii_Planta					=	Integer(istr_mant.Argumento[3])

IF al_Productor<>0 AND ai_Especie<>0 AND ai_Variedad<>0 AND &
	Not IsNull(as_Embalaje)  AND Not IsNull(as_Calibre) AND ai_Packing<>0  AND &
	ad_FechaEmb<>Date('19000101') THEN
	
	SELECT Count(cclo_numero)
	INTO	 :li_Cuenta
	FROM	 dba.ctlcallotes
	WHERE  clie_codigo=:gi_CodExport
	AND    plde_codigo=:ii_Planta
	AND    prod_codigo=:al_Productor
	AND    espe_codigo=:ai_Especie
	AND    vari_codigo=:ai_Variedad
	AND    emba_codigo=:as_Embalaje
	AND    vaca_calibr=:as_Calibre
	AND    plde_codpak=:ai_Packing
	AND    cclo_fecemb=:ad_FechaEmb;
	
	
	IF sqlca.sqlcode=-1 THEN
		F_ErrorBaseDatos(sqlca,"No se pudo leer la Tabla de Lotes")
	END IF
		IF li_Cuenta > 1 THEN
			istr_busq.Argum[1]	=	String(gi_CodExport)
			istr_busq.Argum[2]	=	String(ii_Planta)
			istr_busq.Argum[3]	=	string(al_Productor)
			istr_busq.Argum[4]	=	String(ai_Especie)
			istr_busq.Argum[5]	=	String(ai_Variedad)
			istr_busq.Argum[6]	=	as_Embalaje
			istr_busq.Argum[7]	=	as_Calibre
			istr_busq.Argum[8]	=	String(ai_Packing)
			istr_busq.Argum[9]	=	String(ad_FechaEmb)
		
		
		OpenWithParm(w_busc_lotes,istr_busq)
		
		istr_busq	=	Message.PowerObjectParm
		
			IF istr_busq.Argum[1]	<>	"" THEN								
				dw_3.InsertRow(0)
				dw_2.InsertRow(0)
				dw_3.SetITem(1,"cclo_numero",Integer(istr_busq.Argum[11]))
				dw_3.SetItem(1,"cclo_tamlot",Integer(istr_busq.Argum[10]))
				dw_2.SetItem(1,"espe_codigo",Integer(istr_busq.Argum[4]))
				dw_1.Retrieve(gi_CodExport,ii_Planta,Integer(istr_Busq.Argum[11]),&
								  Long(istr_mant.Argumento[4]))
				pb_ins_det.Enabled	=	True
				pb_eli_det.Enabled	=	True
			END IF
		END IF
	
		IF li_cuenta	=	1	THEN
					
				dw_3.Retrieve(Integer(istr_mant.Argumento[1]), &
								 Long(istr_Mant.Argumento[9]), &
								 Integer(istr_Mant.Argumento[7]), &
								 Integer(istr_Mant.Argumento[8]), &
								 istr_Mant.Argumento[10], &
								 istr_Mant.Argumento[11], &
								 Integer(istr_Mant.Argumento[12]), &
								 Date(istr_Mant.Argumento[13]))
					pb_ins_det.Enabled	=	True
								 
		END IF						 
				//IF dw_3.RowCount() = 0	THEN
		IF li_cuenta = 0 THEN
			pb_grabar.Enabled = FALSE
			MessageBox("Atención", "No Existe Información para la Planilla ~r" +&
											"y el Lote seleccionado")
		ELSEIF	dw_3.RowCount()	=	1 THEN
			li_Lote	=	dw_3.Object.cclo_numero[1]
			dw_2.InsertRow(0)
			dw_2.SetItem(1,"cclo_numero",li_lote)
			dw_2.SetITem(1,"espe_codigo",Integer(ai_Especie))
			pb_ins_det.Enabled	=	TRUE
		END IF
	
END IF

end subroutine

on w_maed_planilla_cualitativa_uva.create
int iCurrent
call super::create
this.dw_3=create dw_3
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_3
end on

on w_maed_planilla_cualitativa_uva.destroy
call super::destroy
destroy(this.dw_3)
end on

event open;/*
****************** Argumentos Mant******************
istr_Mant.Argumento[1]	=> Código de Cliente
istr_Mant.Argumento[2]	=> Código de Zona
istr_Mant.Argumento[3]	=> Código de Planta
istr_Mant.Argumento[4]	=> Número Folio Planilla
istr_Mant.Argumento[5]	=> Número de Lote
istr_Mant.Argumento[6]	=> Fecha de Inspeccion
istr_Mant.Argumento[7]	=> Código de Especie
istr_Mant.Argumento[8]	=> Código de Variedad
istr_Mant.Argumento[9]	=> Código de Productor
istr_Mant.Argumento[10]	=> Código de Embalaje
istr_Mant.Argumento[11]	=> Código de Calibre
istr_Mant.Argumento[12]	=> Código de Packing
istr_Mant.Argumento[13]	=> Fecha de Embalaje
istr_Mant.Argumento[14]	=> Tamaño del Lote
istr_Mant.Argumento[15]	=> Código Embarque
istr_mant.Argumento[16]	=> Código Inspector
istr_mant.Argumento[17]	=> Nombre Responsable
****************** Argumentos Mant******************

****************** Argumentos Busq******************
istr_busq.argum[1]	=	Foilo
istr_busq.argum[2]	=  Cliente
istr_busq.argum[3]	=	Planta
istr_busq.argum[4]	=	Productor
istr_busq.argum[5]	=	Especie
istr_busq.argum[6]	=	Variedad
istr_busq.argum[5]	=	Embalaje
istr_busq.argum[6]	=	Calibre
istr_busq.argum[7]	=	Packing
istr_busq.argum[8]	=	Fecha Embalaje
istr_busq.argum[9]	=	Tamaño Lote
istr_busq.argum[10]	=	Número Lote
****************** Argumentos Mant******************
*/
x	=	0
y	=	0

This.Height	= 2020
im_menu		= m_principal

This.Icon									=	Gstr_apl.Icono
This.ParentWindow().ToolBarVisible	=	True
im_menu.Item[1].Item[6].Enabled		=	True
im_menu.Item[7].Visible					=	True

dw_1.SetTransObject(SQLCA)
dw_2.SetTransObject(SQLCA)
dw_3.SetTransObject(SQLCA)

dw_1.Modify("datawindow.message.title='Error '+ is_titulo")
dw_1.SetRowFocusIndicator(Hand!)
dw_1.Modify("DataWindow.Footer.Height = 110")

istr_Mant.dw						=	dw_1
istr_Mant.dw2						=	dw_2
istr_Mant.UsuarioSoloConsulta	=	OpcionSoloConsulta()

pb_nuevo.PostEvent(Clicked!)

GrabaAccesoAplicacion(True, id_FechaAcceso, it_HoraAcceso, &
							 This.Title, "Acceso a Aplicación", 1)

Buscar	= "Embalaje 1:Ncccd_embal1,Embalaje 2:Ncccd_embal2,Embalaje 3:Ncccd_embal3"
Ordenar	= "Embalaje 1:cccd_embal1,Embalaje 2:cccd_embal2,Embalaje 3:cccd_embal3"

iuo_ctlcalinspectores	=	Create uo_ctlcalinspectores

dw_2.GetChild("zona_codigo", idwc_zonas)
idwc_zonas.SetTransObject(SQLCA)
IF	idwc_zonas.Retrieve()	=	0	THEN
	idwc_zonas.InsertRow(0)
ELSE
	idwc_zonas.SetSort("zona_nombre A")
	idwc_zonas.Sort()
	dw_2.SetItem(1, "zona_codigo", gi_codZona)	
END IF

dw_2.GetChild("prod_codigo", idwc_productores)
idwc_productores.SetTransObject(SQLCA)
IF	idwc_productores.Retrieve(gi_codexport,gi_codZona)	=	0	THEN
	idwc_productores.InsertRow(0)
ELSE
	idwc_productores.SetSort("prod_nombre A")
	idwc_productores.Sort()
	dw_2.SetItem(1,"prod_codigo",gi_CodExport)
END IF

dw_2.GetChild("plde_codigo", idwc_plantas)
idwc_plantas.SetTransObject(SQLCA)
IF idwc_plantas.Retrieve(gi_codexport,1) = 0 THEN	
	idwc_plantas.InsertRow(0)
ELSE
	idwc_plantas.SetSort("plde_nombre A")
	idwc_plantas.Sort()
	dw_2.SetItem(1,"plde_codigo",gi_CodExport)
END IF

dw_2.GetChild("ccin_codigo", idwc_inspectores)
idwc_inspectores.SetTransObject(SQLCA)
IF idwc_inspectores.Retrieve(2) = 0 THEN
	idwc_inspectores.SetSort("ccin_nombre A")
	idwc_inspectores.Sort()
	dw_2.SetITem(1,"ccin_codigo",gi_CodExport)
END IF

dw_2.GetChild("embq_codigo", idwc_embarques)
idwc_embarques.SetTransObject(SQLCA)
IF idwc_embarques.Retrieve(gi_CodExport) = 0 THEN 	
	idwc_embarques.SetSort("embq_nomnav A")
	idwc_embarques.Sort()
	idwc_embarques.InsertRow(0)
	dw_2.SetItem(1,"embq_codigo", gi_CodExport)
END IF

dw_3.GetChild("prod_codigo", idwc_productor)
dw_3.GetChild("espe_codigo", idwc_especie)
dw_3.GetChild("emba_codigo", idwc_embalaje)
dw_3.GetChild("plde_codpak", idwc_packing)
dw_3.Getchild("vari_codigo", idwc_variedad)
dw_3.Getchild("vaca_calibr", idwc_calibre)

idwc_productor.SetTransObject(SQLCA)
idwc_especie.SetTransObject(SQLCA)
idwc_embalaje.SetTransObject(SQLCA)
dw_3.SetTransObject(SQLCA)
idwc_variedad.SetTransObject(SQLCA)
idwc_calibre.SetTransObject(SQLCA)

idwc_productor.Retrieve(gi_codexport,gi_CodZona)
idwc_productores.SetSort("prod_nombre A")
idwc_productores.Sort()
dw_3.SetItem(1, "prod_codigo", gi_codProductor)

idwc_especie.Retrieve(gi_codexport)
idwc_especie.SetSort("espe_nombre A")
idwc_especie.Sort()
dw_3.SetItem(1, "espe_codigo", gi_CodEspecie)
dw_2.SetItem(1, "espe_codigo", gi_CodEspecie)

idwc_embalaje.Retrieve(gi_CodExport)
idwc_embalaje.SetSort("emba_codigo A")
idwc_embalaje.Sort()
dw_3.SetItem(1, "emba_codigo", gs_CodEmbalaje)

dw_3.GetChild("plde_codpak", idwc_packing)
idwc_packing.SetTransObject(sqlca)
idwc_packing.Retrieve(gi_CodExport,2,gi_codZona)
idwc_packing.SetSort("plde_nombre A")
idwc_packing.Sort()

IF idwc_variedad.Retrieve(gi_CodExport,gi_CodEspecie) = 0 THEN
	idwc_variedad.InsertRow(0)
ELSE
	idwc_variedad.SetSort("vari_nombre A")
	idwc_variedad.Sort()
	dw_3.SetItem(1, "vari_codigo", gi_CodVariedad)
END IF
	
IF idwc_calibre.Retrieve(gi_CodExport,gi_CodEspecie,gi_CodVariedad) = 0 THEN
	idwc_calibre.InsertRow(0)
ELSE
	idwc_calibre.SetSort("vaca_calibr A")
	idwc_calibre.Sort()
	dw_3.SetItem(1, "vari_codigo", gi_CodVariedad)
END IF

dw_3.InsertRow(0)
dw_2.SetItem(1, "clie_codigo", gi_CodExport)

istr_mant.argumento[1]	=	String(gi_CodExport)
istr_mant.argumento[2]	=	String(gi_CodZona)
istr_mant.argumento[3]	=	String(gi_CodPlanta)
istr_mant.argumento[6]	=	String(Today()) 
istr_mant.argumento[7]	=	String(gi_CodEspecie)
istr_mant.argumento[8]	=	String(gi_CodVariedad)
istr_mant.argumento[9]	=	String(gi_CodProductor)
istr_mant.argumento[10]	=	gs_CodEmbalaje
istr_mant.argumento[11]	=	''
istr_mant.argumento[12]	=	'0'
istr_mant.argumento[13]	=	String(ToDay())
istr_mant.argumento[14]	=	'0'
istr_mant.argumento[15]	=	''
istr_mant.argumento[16]	=	''
istr_mant.argumento[17]	=	''
end event

event resize;//
end event

event ue_nuevo_detalle();call super::ue_nuevo_detalle;istr_mant.borra			= False
istr_mant.agrega			= True

OpenWithParm(iw_mantencion, istr_mant)

IF dw_1.RowCount() > 0 THEN
	pb_grabar.Enabled		= True
	pb_eli_det.Enabled	= True
END IF

dw_1.SetRow(il_fila)
dw_1.SelectRow(il_fila,True)

end event

event ue_recuperadatos;Long		ll_Fila_e, ll_Fila_d, ll_Fila_f, Respuesta
Integer	li_Grupo
String	ls_Usuario
ls_Usuario	=	Upper(Gstr_Us.Nombre)

DO
	dw_2.SetRedraw(False)
	dw_2.Reset()	
	
	ll_Fila_e	= dw_2.Retrieve(Integer(istr_Mant.Argumento[1]), &
										 Integer(istr_Mant.Argumento[3]), &
										 Integer(istr_Mant.Argumento[5]), &
										 Long(istr_Mant.Argumento[4]))

	IF ll_Fila_e = -1 THEN
		Respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
										Information!, RetryCancel!)
	ELSE
		// Inspectores
		dw_2.GetChild("ccin_codigo", idwc_inspectores)
		idwc_inspectores.SetTransObject(SQLCA)
		idwc_inspectores.Retrieve(istr_mant.Argumento[2])
		// Productores
		dw_3.GetChild("prod_codigo", idwc_productor)
		idwc_productor.SetTransObject(SQLCA)
		idwc_productor.Retrieve(gi_CodExport, Integer(istr_mant.Argumento[2]))
		// Packing
		dw_3.GetChild("plde_codpak", idwc_packing)
		idwc_packing.SetTransObject(SQLCA)
		idwc_packing.Retrieve(gi_CodExport, 2, Integer(istr_mant.Argumento[2]))

		DO
			ll_Fila_d	= dw_1.Retrieve(Integer(istr_Mant.Argumento[1]), &
												 Integer(istr_Mant.Argumento[3]), &
												 Integer(istr_Mant.Argumento[5]), &
												 Long(istr_Mant.Argumento[4]))

			IF ll_Fila_d = -1 THEN
				Respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
												Information!, RetryCancel!)
			ELSE

				DO
					ll_Fila_f	= dw_3.Retrieve(Integer(istr_Mant.Argumento[1]), &
														 Long(istr_Mant.Argumento[9]), &
														 Integer(istr_Mant.Argumento[7]), &
														 Integer(istr_Mant.Argumento[8]), &
														 istr_Mant.Argumento[10], &
														 istr_Mant.Argumento[11], &
														 Integer(istr_Mant.Argumento[12]), &
														 Date(istr_Mant.Argumento[13]))

					IF ll_Fila_f = -1 THEN
						Respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
														Information!, RetryCancel!)
					END IF
				LOOP WHILE Respuesta = 1

				HabilitaEncab(False)

				pb_eliminar.Enabled	= True
				pb_grabar.Enabled		= True
				pb_ins_det.Enabled	= True
			END IF

		IF ll_Fila_d > 0 THEN
			li_Grupo	=	BuscaGrupo(ls_Usuario)
			IF (li_Grupo = 6) OR (li_Grupo = 1)	THEN 
				pb_eli_det.Enabled	= True
				pb_imprimir.Enabled	= True
				dw_1.SetRow(1)
				dw_1.SelectRow(1,True)
				dw_1.SetFocus()				
			ELSE	
				Deshabilit()		
				dw_2.Enabled				=	False
				dw_3.Enabled				=	False
				Pb_eliminar.Enabled		=	False	
				Pb_Grabar.Enabled			=	False	
				Pb_ins_det.Enabled 		=	False
				Pb_eli_det.Enabled		=	False
				Pb_imprimir.Enabled		=	True 				
				istr_mant.Solo_Consulta =	True 									
			END IF 
		ELSE
			pb_ins_det.SetFocus()
			
		END IF

		LOOP WHILE Respuesta = 1

		IF Respuesta = 2 THEN Close(This)
	END IF

	dw_2.SetRedraw(True)

LOOP WHILE Respuesta = 1

IF Respuesta = 2 THEN Close(This)
end event

event ue_seleccion();call super::ue_seleccion;istr_Busq.Argum[1]	=	String(dw_2.Object.clie_codigo[1]) 

OpenWithParm(w_busc_ctlcalplacualinspuvaenc, istr_Busq)

istr_Busq = Message.PowerObjectParm

IF istr_Busq.Argum[2] <> "" THEN
	istr_Mant.Argumento[2]  = istr_Busq.Argum[5]
	istr_Mant.Argumento[3]  = istr_Busq.Argum[2]
	istr_Mant.Argumento[4]  = istr_Busq.Argum[3]
	istr_Mant.Argumento[5]  = istr_Busq.Argum[4]
	istr_Mant.Argumento[7]  = istr_Busq.Argum[7]
	istr_Mant.Argumento[8]  = istr_Busq.Argum[8]
	istr_Mant.Argumento[9]  = istr_Busq.Argum[6]
	istr_Mant.Argumento[10] = istr_Busq.Argum[9]
	istr_Mant.Argumento[11] = istr_Busq.Argum[10]
	istr_Mant.Argumento[12] = istr_Busq.Argum[11]
	istr_Mant.Argumento[13] = istr_Busq.Argum[12]
	istr_Mant.Argumento[14] = istr_Busq.Argum[13]

	This.TriggerEvent("ue_recuperadatos")
ELSE
	dw_2.SetFocus()
	dw_2.SetColumn("zona_codigo")
END IF
end event

event ue_nuevo();Long		ll_modif1, ll_modif2, ll_modif3
	
Habilit()

istr_Busq.Argum[1] = ""

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

dw_2.SetItem(1, "clie_codigo", gi_codexport)
dw_2.SetItem(1, "ccce_lugari", 2)
dw_2.SetItem(1, "plde_codigo", Integer(istr_mant.argumento[3]))
dw_2.SetItem(1, "zona_codigo", Integer(istr_mant.argumento[2]))

dw_2.SetColumn("zona_Codigo")
dw_2.SetFocus()

IF istr_mant.argumento[4] <> '' THEN
	dw_2.SetItem(1, "ccce_numero", Date(istr_mant.argumento[4]))
END IF

IF istr_mant.argumento[6] <> '' THEN
	dw_2.SetItem(1, "ccce_fecins", Date(istr_mant.argumento[6]))
END IF

IF istr_mant.argumento[15] <> '' THEN
	dw_2.SetItem(1, "embq_codigo", istr_mant.argumento[15])
END IF

IF istr_mant.argumento[16] <> '' THEN
	dw_2.SetItem(1, "ccin_codigo", Integer(istr_mant.Argumento[16]))
END IF

IF istr_mant.argumento[17] <> '' THEN
	dw_2.SetItem(1, "ccce_nomrep", istr_mant.Argumento[17])
END IF

dw_2.SetRedraw(True)

dw_3.SetRedraw(False)

dw_3.Reset()
dw_3.InsertRow(0)

dw_3.SetItem(1, "espe_codigo",integer(istr_mant.argumento[7]))
dw_3.SetItem(1, "vari_codigo",integer(istr_mant.argumento[8]))
dw_3.SetItem(1, "cclo_fecemb",date(istr_mant.argumento[13]))
dw_3.SetItem(1, "emba_codigo",istr_mant.argumento[10])

dw_3.SetRedraw(True)

HabilitaEncab(True)
end event

event ue_borrar();call super::ue_borrar;IF dw_3.RowCount() < 1 THEN RETURN

SetPointer(HourGlass!)

ib_borrar = True
w_main.SetMicroHelp("Validando la eliminación...")

Message.DoubleParm = 0

This.TriggerEvent ("ue_validaborrar")

IF Message.DoubleParm = -1 THEN RETURN

IF dw_1.RowCount() > 0 THEN dw_1.RowsMove(1,dw_1.RowCount(),Primary!,dw_1,1,Delete!)

IF dw_2.RowCount() > 0 THEN dw_2.RowsMove(1,1,Primary!,dw_2,1,Delete!)

IF dw_3.DeleteRow(0) = 1 THEN
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

event ue_borra_detalle();IF dw_1.rowcount() < 1 THEN RETURN

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
 IF dw_1.RowCount() = 0 THEN pb_eli_det.Enabled = False
END IF

istr_mant.borra	 = False
end event

event ue_modifica_detalle();call super::ue_modifica_detalle;IF dw_1.RowCount() > 0 THEN
	istr_Mant.Agrega	= False
	istr_Mant.Borra	= False

	OpenWithParm(iw_mantencion, istr_Mant)
END IF
end event

event ue_antesguardar;call super::ue_antesguardar;Long		ll_NumeroFolio, ll_NumeroLote, ll_Productor
Integer	li_Cliente, li_Planta, li_Secuencia, li_Fila
String	ls_resolu

ll_NumeroFolio	=	dw_2.GetItemNumber(1, "ccce_numero")
li_Cliente		=	dw_2.GetItemNumber(1, "clie_codigo")
li_Planta		=	dw_2.GetItemNumber(1, "plde_codigo")
ll_NumeroLote	=	dw_3.GetItemNumber(1, "cclo_numero")
ll_Productor	=	dw_3.GetItemNumber(1, "prod_codigo")

dw_2.SetItem(1, "prod_codigo", ll_Productor)

dw_2.SetItem(1, "ccce_numero", ll_NumeroFolio)
dw_2.SetItem(1, "clie_codigo", li_Cliente)
dw_2.SetItem(1, "plde_codigo", li_Planta)
dw_2.SetItem(1, "cclo_numero", ll_NumeroLote)

ls_resolu		=	dw_2.Object.ccce_resolu[1]
IF ls_resolu = 'A' THEN
	dw_3.SetItem(1, "cclo_estado",1)
ELSE
	dw_3.SetItem(1, "cclo_estado",2)
END IF

SELECT	Max(cccd_secuen)
	INTO	:li_Secuencia
	FROM	dba.ctlcalplacualinspuvadet
	WHERE	ccce_numero	=	:ll_NumeroFolio
	AND 	clie_codigo	=	:li_Cliente
	AND	plde_codigo =	:li_Planta
	AND	cclo_numero	=	:ll_NumeroLote;

IF IsNull(li_Secuencia) THEN li_Secuencia = 0

FOR li_Fila = 1 TO dw_1.RowCount()
	IF Isnull(dw_1.Object.cccd_secuen[li_Fila]) OR  dw_1.Object.cccd_secuen[li_Fila] = 0 THEN
		li_Secuencia ++
		dw_1.Object.ccce_numero[li_Fila]	=	ll_NumeroFolio
		dw_1.Object.clie_codigo[li_Fila] =	li_Cliente
		dw_1.Object.plde_codigo[li_Fila] =	li_Planta
		dw_1.Object.cclo_numero[li_Fila]	=	ll_NumeroLote
		dw_1.Object.cccd_secuen[li_Fila] =	li_Secuencia
	END IF
NEXT

end event

event ue_imprimir;SetPointer(HourGlass!)

Long		Fila

istr_info.titulo	=	"INFORME DE PLANILLAS INSPECCION"
istr_info.copias	=	1

OpenWithParm(vinf,istr_info)

vinf.dw_1.DataObject	=	"dw_info_ctlcalplacualinspuvaenc"

vinf.dw_1.SetTransObject(sqlca)

vinf.dw_1.GetChild("ccin_codigo", idwc_inspectores)
idwc_inspectores.SetTransObject(SQLCA)
idwc_inspectores.Retrieve(Integer(istr_Mant.Argumento[2]))

vinf.dw_1.GetChild("plde_codpak", idwc_packing)
idwc_packing.SetTransObject(SQLCA)
idwc_packing.Retrieve(Integer(istr_Mant.Argumento[1]))

Fila	=	vinf.dw_1.Retrieve(Integer(istr_Mant.Argumento[1]), &
									 Integer(istr_Mant.Argumento[3]), &
									 Integer(istr_Mant.Argumento[4]))

IF Fila	=	-1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF fila	=	0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", &
					StopSign!, Ok!)
ELSE
	F_Membrete(vinf.dw_1)
	IF gs_Ambiente <> 'Windows' THEN
		F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)

	END IF
END IF

SetPointer(Arrow!)
end event

event ue_guardar();istr_Mant.Borra	= False
istr_Mant.Agrega	= False

OpenWithParm(w_mant_ctlcalplacualuvas_resolucionlot, istr_Mant)

call super::ue_guardar
end event

event buscalotes(integer ai_productor, integer ai_especie, integer ai_variedad, string as_embalaje, string as_calibre, integer ai_packing, date ad_fechaemb, integer ai_numplanilla);Integer li_Existe, li_Cuenta


IF ai_Productor<>0 AND ai_Especie<>0 AND ai_Variedad<>0 AND &
	Not IsNull(as_Embalaje)  AND Not IsNull(as_Calibre) AND ai_Packing<>0  AND &
	Not ad_FechaEmb=Date('01/01/1900') THEN
	
	SELECT Count(cclo_numero)
	INTO	 :li_Cuenta
	FROM	 dba.ctlcallotes
	WHERE  clie_codigo=81
	AND    plde_codigo=:ii_Planta
	AND    prod_codigo=:ai_Productor
	AND    espe_codigo=:ai_Especie
	AND    vari_codigo=:ai_Variedad
	AND    emba_codigo=:as_Embalaje
	AND    vaca_calibr=:as_Calibre
	AND    plde_codpak=:ai_Packing
	AND    cclo_fecemb=:ad_FechaEmb;
	
	
	IF sqlca.sqlcode=-1 THEN
		F_ErrorBaseDatos(sqlca,"No se pudo leer la Tabla de Lotes")
	ELSEIF li_Cuenta > 0 THEN
   	istr_busq.Argum[1]	=	String(ii_Planta)
		OpenWithParm(w_busc_lotes_cualitativa,istr_busq)
		
		istr_busq	=	Message.PowerObjectParm
		
		IF istr_busq.Argum[1]	<>	"" THEN
			dw_3.InsertRow(0)
			dw_3.SetITem(1,"clie_codigo",81)
			dw_3.SetITem(1,"plde_codigo",ii_Planta)
			dw_3.SetITem(1,"cclo_numero",istr_busq.Argum[1])
			dw_3.SetITem(1,"prod_codigo",istr_busq.Argum[2])
			dw_3.SetITem(1,"espe_codigo",istr_busq.Argum[3])
			dw_3.SetITem(1,"emba_codigo",istr_busq.Argum[4])
			dw_3.SetITem(1,"vaca_calibr",istr_busq.Argum[5])
			dw_3.SetITem(1,"plde_codpak",istr_busq.Argum[6])
			dw_3.SetITem(1,"cclo_fecemb",istr_busq.Argum[7])
			dw_3.SetITem(1,"cclo_tamlot",istr_busq.Argum[8])
			dw_3.SetITem(1,"cclo_tippla",istr_busq.Argum[9])
			dw_3.SetITem(1,"cclo_pldeap",istr_busq.Argum[10])
			dw_3.SetITem(1,"cclo_numpla",istr_busq.Argum[11])
			dw_3.SetITem(1,"cclo_fecapr",istr_busq.Argum[12])
			dw_3.SetITem(1,"cclo_estado",istr_busq.Argum[13])
			pb_ins_det.Enabled	=	True
			pb_eli_det.Enabled	=	True
			
			SELECT	Count(*)
				INTO	:li_Existe
				FROM	dba.ctlcalplacualinspuvaenc
				WHERE	ccce_numero =	:ai_NumPlanilla
				AND	clie_codigo	=	:gi_CodExport
				AND   plde_codigo =  :ii_Planta
				AND   cclo_numero =  :istr_busq.Argum[1] ;

			IF SqlCa.SQLCode = -1 THEN
				F_errorbasedatos(SqlCa,"Lectura tabla Planilla de Cualitativa de Insp. Uvas")	
			ELSE
				IF li_Existe > 0 THEN
					
					istr_Mant.Argumento[1]	= String(gi_CodExport)
					istr_Mant.Argumento[3]	= String(ii_Planta)
					istr_Mant.Argumento[4]	= String(ai_NumPlanilla)
					istr_Mant.Argumento[5]	= istr_busq.Argum[1]
					
					This.TriggerEvent("ue_recuperadatos")			
					IF dw_1.RowCount() = 0 THEN
						MessageBox("Atención","No existe información para la Planilla y el Lote")						
					END IF
				END IF
			END IF
		END IF			
	ELSE
		MessageBox("","Lote no Existe")
		
	END IF
END IF
end event

type dw_1 from w_mant_encab_deta_csd`dw_1 within w_maed_planilla_cualitativa_uva
string tag = "detalle de plailla cualitativa"
integer y = 948
integer width = 3054
integer height = 816
string title = "Detalle de Planilla Cualitativa de Inspección de Uvas"
string dataobject = "dw_mues_planilla_cualita_uva_deta"
end type

type dw_2 from w_mant_encab_deta_csd`dw_2 within w_maed_planilla_cualitativa_uva
string tag = "Despliega Encabezado de la planilla cualitativa"
integer x = 32
integer y = 64
integer width = 3122
integer height = 396
integer taborder = 10
string dataobject = "dw_maed_planilla_cualitativa_uvas"
end type

event dw_2::itemchanged;String	ls_Columna, ls_Nula
Date		ld_FechaEmb
Integer	li_Nula

SetNull(ls_Nula)
SetNull(li_Nula)

ls_Columna	=	dwo.Name

CHOOSE CASE ls_Columna
	CASE "zona_codigo"
		IF NoExisteZona(Integer(Data)) THEN
			This.SetItem(Row, ls_Columna, Long(ls_Nula))
			RETURN 1
		ELSE
			NoExistePlanilla(ls_Columna, Data)
			
			istr_Mant.Argumento[2]	=	Data

         dw_2.GetChild("cctc_codigo", idwc_Tecnicos)
			idwc_tecnicos.SetTransObject(SqlCa)
			idwc_tecnicos.Retrieve(Integer(istr_Mant.Argumento[2]))
			idwc_tecnicos.SetSort("ccin_nombre A")
			idwc_tecnicos.Sort()
			
			dw_2.GetChild("ccin_codigo", idwc_Inspectores)
			idwc_Inspectores.SetTransObject(SqlCa)
			idwc_Inspectores.Retrieve(Integer(istr_Mant.Argumento[2]))

         dw_3.GetChild("plde_codpak", idwc_Packing)
			idwc_packing.SetTransObject(SqlCa)
			idwc_packing.Retrieve(gi_CodExport,2,Integer(istr_Mant.Argumento[2]))
			idwc_packing.SetSort("plde_nombre A")
			idwc_packing.Sort()

         dw_3.GetChild("prod_codigo", idwc_Productor)
			idwc_Productor.SetTransObject(SqlCa)
			idwc_Productor.Retrieve(gi_CodExport,Integer(istr_Mant.Argumento[2]))
			idwc_Productor.SetSort("prod_nombre A")
			idwc_Productor.Sort()
		END IF

	CASE "plde_codigo"
		IF NoExistePlanta(Integer(Data)) THEN
			This.SetItem(Row, ls_Columna, li_Nula)
			RETURN 1
		ELSE
			NoExistePlanilla(ls_Columna, Data)
			istr_Mant.Argumento[3]	=	Data
		END IF

	CASE "ccce_numero"
		istr_Mant.Argumento[4]	=	Data	
		NoExistePlanilla(ls_Columna, Data)
								  
	CASE "ccin_codigo"
			IF NOT iuo_CtlCalInspectores.Existe(SqlCa,Integer(Data),True) THEN
				This.SetItem(Row, ls_Columna, li_Nula)
				RETURN 1
			ELSE
				istr_mant.Argumento[16]	=	Data
			END IF

	CASE "ccce_fecins"
		IF dw_2.Object.ccce_lugari[1] = 2 THEN			
			istr_Mant.Argumento[6]	=	Data
		END IF

	CASE "embq_codigo"
			IF NOT ExisteEmbarqueProd(Integer(istr_Mant.Argumento[1]),Data) THEN
				This.SetItem(Row, ls_Columna, ls_Nula)
				RETURN 1
			ELSE
				This.SetItem(Row, "plde_codigo", 1)
				istr_mant.Argumento[15]	=	Data
				ii_Planta					=	Integer(istr_mant.Argumento[15])
			END IF
			
	CASE "ccce_nomrep"
		istr_mant.Argumento[17]	=	Data		

END CHOOSE

HabilitaIngreso(ls_Columna)
end event

type pb_nuevo from w_mant_encab_deta_csd`pb_nuevo within w_maed_planilla_cualitativa_uva
integer x = 3287
integer y = 292
integer taborder = 40
end type

type pb_eliminar from w_mant_encab_deta_csd`pb_eliminar within w_maed_planilla_cualitativa_uva
integer x = 3287
integer y = 472
integer taborder = 50
end type

type pb_grabar from w_mant_encab_deta_csd`pb_grabar within w_maed_planilla_cualitativa_uva
integer x = 3287
integer y = 656
integer taborder = 60
end type

type pb_imprimir from w_mant_encab_deta_csd`pb_imprimir within w_maed_planilla_cualitativa_uva
integer x = 3287
integer y = 832
integer taborder = 70
end type

type pb_salir from w_mant_encab_deta_csd`pb_salir within w_maed_planilla_cualitativa_uva
integer x = 3287
integer y = 1012
integer taborder = 80
end type

type pb_ins_det from w_mant_encab_deta_csd`pb_ins_det within w_maed_planilla_cualitativa_uva
integer x = 3287
integer y = 1392
integer taborder = 90
end type

type pb_eli_det from w_mant_encab_deta_csd`pb_eli_det within w_maed_planilla_cualitativa_uva
integer x = 3287
integer y = 1564
integer taborder = 100
end type

type pb_buscar from w_mant_encab_deta_csd`pb_buscar within w_maed_planilla_cualitativa_uva
integer x = 3287
integer y = 112
integer taborder = 30
end type

type dw_3 from uo_dw within w_maed_planilla_cualitativa_uva
integer x = 32
integer y = 496
integer width = 3122
integer height = 400
integer taborder = 20
boolean bringtotop = true
string dataobject = "dw_mues_lotes_planilla_cuantitativa"
boolean vscrollbar = false
end type

event itemchanged;String	ls_Columna, ls_Null
Integer	ll_Fila
Date		ld_FechaInsp

SetNull(ls_Null)
dw_3.AcceptText()
istr_mant.Argumento[13]	=	'01/01/1900'	

ls_Columna	=	dwo.Name

CHOOSE CASE ls_Columna

	CASE "espe_codigo"
		IF NOT ExisteNumeroLote(ls_Columna, Data) AND &
			NOT ExisteEspecie(Integer(istr_Mant.Argumento[1]),Integer(Data)) THEN
			This.SetItem(Row, ls_Columna, Integer(ls_Null))			
			HabilitaIngreso(ls_Columna)
		ELSE
			istr_Mant.Argumento[7]	=	Data
			dw_2.SetItem(1,"espe_codigo",Integer(Data))

         dw_3.GetChild("vari_codigo", idwc_variedad)
			idwc_variedad.SetTransObject(SqlCa)
			idwc_variedad.Retrieve(Integer(istr_Mant.Argumento[1]), &
										  Integer(Data))
			idwc_variedad.SetSort("vari_nombre A")
			idwc_variedad.Sort()
		END IF

	CASE "vari_codigo"
		IF NOT ExisteNumeroLote(ls_Columna, Data) AND &
			NOT ExisteVariedad(Integer(istr_Mant.Argumento[1]), &
									 Integer(istr_Mant.Argumento[7]), &
									 Integer(Data)) THEN
			This.SetItem(Row, ls_Columna, Integer(ls_Null))
			HabilitaIngreso(ls_Columna)
			RETURN 1
		ELSE
			istr_Mant.Argumento[8]	=	Data
			dw_2.Object.vari_codigo[1]	=	Integer(Data)

			dw_3.GetChild("vaca_calibr", idwc_calibre)
			idwc_calibre.SetTransObject(SqlCa)
			idwc_calibre.Retrieve(Integer(istr_Mant.Argumento[1]), &
										 Integer(istr_Mant.Argumento[7]), &
										 Integer(Data))
			dw_3.Object.vaca_calibr[row] = ''
			idwc_calibre.SetSort("vaca_calibr A")
			idwc_calibre.Sort()
			
		END IF

	CASE "prod_codigo"
		IF NOT ExisteNumeroLote(ls_Columna, Data) AND &
			NOT ExisteProductores(Integer(istr_Mant.Argumento[1]),Long(Data)) THEN
			This.SetItem(Row, ls_Columna, Long(ls_Null))
			HabilitaIngreso(ls_Columna)
			RETURN 1
		ELSE
			istr_Mant.Argumento[9]	=	Data
		END IF

	CASE "emba_codigo"
		IF NOT ExisteNumeroLote(ls_Columna, Data) AND &
			NOT ExisteEmbalaje(Integer(istr_Mant.Argumento[1]),Data) THEN
			This.SetItem(Row, ls_Columna, ls_Null)
			HabilitaIngreso(ls_Columna)
			RETURN 1
		ELSE
			istr_Mant.Argumento[10]	=	Data
		END IF

	CASE "vaca_calibr"
		IF NOT ExisteCalibre(Integer(istr_Mant.Argumento[1]),Integer(istr_Mant.Argumento[7]), &
			Integer(istr_Mant.Argumento[8]),Data) AND NOT ExisteNumeroLote(ls_Columna, Data) THEN
			This.SetItem(Row, ls_Columna, ls_Null)
			HabilitaIngreso(ls_Columna)
			RETURN 1
		ELSE
			istr_Mant.Argumento[11]	=	Data
		END IF

	CASE "plde_codpak"
		IF NOT ExistePacking(Integer(Data)) AND NOT ExisteNumeroLote(ls_Columna, Data) THEN
			This.SetItem(Row, ls_Columna, Integer(ls_Null))
			HabilitaIngreso(ls_Columna)
			RETURN 1
		ELSE
			istr_Mant.Argumento[12]	=	Data
		END IF

	CASE "cclo_fecemb"
		
		istr_mant.Argumento[13]	=	Data


END CHOOSE

BuscaLotes(Long(istr_mant.Argumento[9]),Integer(istr_mant.Argumento[7]),&
           Integer(istr_mant.Argumento[8]),istr_mant.Argumento[10],istr_mant.Argumento[11],&
			  Integer(istr_mant.Argumento[12]),Date(istr_mant.Argumento[13]))

HabilitaIngreso(ls_Columna)
end event

event itemerror;call super::itemerror;RETURN 1
end event

event losefocus;call super::losefocus;AcceptText()

RETURN 0
end event

