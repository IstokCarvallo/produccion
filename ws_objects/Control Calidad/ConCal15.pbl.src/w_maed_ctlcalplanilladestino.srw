$PBExportHeader$w_maed_ctlcalplanilladestino.srw
$PBExportComments$Ingresador de antecedentes para Planilla Cuantitativa.
forward
global type w_maed_ctlcalplanilladestino from w_mant_encab_deta_csd
end type
type dw_3 from datawindow within w_maed_ctlcalplanilladestino
end type
type dw_5 from uo_dw within w_maed_ctlcalplanilladestino
end type
end forward

global type w_maed_ctlcalplanilladestino from w_mant_encab_deta_csd
integer width = 3438
integer height = 2032
string title = "PLANILLA DESTINO INSPECCION DE UVAS"
string menuname = ""
boolean minbox = false
boolean maxbox = false
event ue_validaborrar_detalle ( )
dw_3 dw_3
dw_5 dw_5
end type
global w_maed_ctlcalplanilladestino w_maed_ctlcalplanilladestino

type variables
DataWindowChild idwc_clientes,idwc_zonas,idwc_plantas,idwc_productores,idwc_especies, &
idwc_variedades,idwc_tecnicos,idwc_inspectores,idwc_packings,idwc_calibres,idwc_embalajes, &
idwc_agronomos, idwc_etiqueta, idwc_embalaje, idwc_calibre, idwc_inspector, idwc_puerto,&
idwc_recibidor, idwc_transporte

w_mant_deta_ctlcaldesinodet iw_mantencion
end variables

forward prototypes
public function boolean noexistetipoinspec (integer codigo)
public function boolean existenumeroplanilla (string as_columna, string as_valor)
public function long maximoplanilla ()
public function boolean noexisteagronomo (integer agronomo)
public function boolean noexistecalibre (string calibre)
public function integer noexisteembalaje (string as_embalaje)
public function boolean noexisteespecie (integer especie)
public function integer noexisteetiqueta (integer etiqueta)
public function boolean noexistegrupo (string data)
public function boolean noexisteinspector (integer inspector)
public function boolean noexistepacking (integer codigo)
public function boolean noexisteplanta (integer planta)
public function boolean noexistetecnico (integer tecnico)
public function boolean noexistezona (integer zona)
public function long nuevolote (integer cliente, integer planta)
protected function integer wf_modifica ()
protected function boolean wf_actualiza_db (boolean borrando)
public function boolean existeencabezado ()
public function boolean existenumerolote (string as_columna, string as_valor)
public function boolean noexistenumerolote (string as_columna, string as_valor)
public subroutine habilitaingreso (string ls_columna)
public function boolean duplicado (string data)
public function integer noexistetiponave (string tiponave)
public function integer noexistenavecodigo (integer navecodigo)
public function boolean noexistevariedad (integer variedad)
public function integer noexistepuerto (integer puerto)
public function integer noexisteproductor (long productor)
public function integer noexisterecibidor (long reci)
end prototypes

event ue_validaborrar_detalle();IF MessageBox("Borrar Registro","Desea Borrar la Información ?", Question!, YesNo!) = 1 THEN
	Message.DoubleParm = 1
ELSE
	Message.DoubleParm = -1
END IF

RETURN
end event

public function boolean noexistetipoinspec (integer codigo);Integer li_Contador

Select Count(*)
Into :li_Contador
From dba.ctlcaltiposinspeccion
Where ccti_codigo = :codigo;

IF sqlca.sqlcode = -1 THEN
	F_ErrorBaseDatos(sqlca,"No se pudo leer la tabla Tipo Inspección")
	RETURN TRUE
ELSEIF li_Contador = 0 THEN
	messagebox("Atención","Código Tipo Inspección No Existe Para la Zona" + &
					"~n~nSeleccionada, Ingrese Otro Por Favor",Exclamation!)
	RETURN TRUE
ELSE	
	RETURN FALSE	
END IF

end function

public function boolean existenumeroplanilla (string as_columna, string as_valor);Integer	li_lote, li_planta,li_especie, li_variedad, li_packing, li_zona, &
         li_tecnico, li_inspector, li_tipoins, li_agronomo, li_maxlote
Long		ll_numero, ll_noguia, ll_productor
Date		ld_Fecemb, ld_fecins

ll_numero 		=	dw_2.Object.ccpe_numero[1]

CHOOSE CASE as_columna
	
	CASE "ccpe_numero"
		ll_numero 		=	Long(as_valor)

END CHOOSE

SELECT   plde_codigo, ccpe_noguia, zona_codigo, prod_codigo, &
         espe_codigo, vari_codigo, ccag_codigo, ccin_codigo, ccpe_fechin, &
			ccti_codigo, &
			max(cclo_numero)
	INTO	:li_planta, :ll_noguia, :li_zona, :ll_productor, :li_especie, &
			:li_variedad, :li_agronomo, :li_inspector, :ld_fecins, :li_tipoins, &
			:li_maxlote
	FROM	"dba".CTLCALPLACUANINSPUVAENC
	WHERE	ccpe_numero	=	:ll_numero
	GROUP BY plde_codigo, ccpe_noguia, zona_codigo, prod_codigo, &
         espe_codigo, vari_codigo, ccag_codigo, ccin_codigo, ccpe_fechin, &
			ccti_codigo;

IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla CTLCALPLACUANINSPUVAENC")
	RETURN True
ELSE
	IF li_planta > 0 THEN
		istr_mant.argumento[1]	= String(li_lote)
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

      dw_2.GetChild("plde_codigo", idwc_plantas)
		idwc_plantas.SetTransObject(sqlca)
		idwc_plantas.Retrieve(Integer(istr_mant.argumento[7]),1)

      dw_3.GetChild("plde_codpak", idwc_packings)
		idwc_packings.SetTransObject(sqlca)
		idwc_packings.Retrieve(Integer(istr_mant.argumento[7]),2,Integer(istr_mant.argumento[3]))

	   dw_2.GetChild("ccag_codigo", idwc_agronomos)
		idwc_agronomos.SetTransObject(sqlca)
		idwc_agronomos.Retrieve(0,Integer(istr_mant.argumento[3]))

      dw_2.GetChild("ccin_codigo", idwc_inspectores)
		idwc_inspectores.SetTransObject(sqlca)
		idwc_inspectores.Retrieve(Integer(istr_mant.argumento[3]))

      dw_2.GetChild("prod_codigo", idwc_productores)
		idwc_productores.SetTransObject(sqlca)
		idwc_productores.Retrieve(Integer(istr_mant.argumento[7]),Integer(istr_mant.argumento[3]))

		RETURN False
   END IF
	RETURN False
END IF
end function

public function long maximoplanilla ();Long		ll_Cantidad, ll_Inicial, ll_Actual, li_planta, ll_Maximo


SELECT MAX(ccpe_numero)
INTO	:ll_Maximo
FROM   dba.ctlcalplacuaninspuvaenc;

IF sqlca.sqlcode	=	-1	THEN
	F_ErrorBaseDatos(sqlca,"No se pudo leer la tabla de PLanillas de Inspección") 
ELSEIF ll_Maximo	>	0	THEN	
	
	ll_Maximo	=	ll_Maximo + 1	
	RETURN ll_Maximo
END IF

RETURN 0	
end function

public function boolean noexisteagronomo (integer agronomo);Integer li_Contador, li_zona
Long    ll_Productor

li_zona			=	dw_2.Object.zona_codigo[1]
ll_Productor	=	dw_2.Object.prod_codigo[1]

Select Count(*)
Into :li_Contador
From dba.ctlcalagroproduc
Where ccag_codigo = :agronomo
and   zona_codigo = :li_zona
and   prod_codigo	= :ll_Productor
;

IF sqlca.sqlcode = -1 THEN
	F_ErrorBaseDatos(sqlca,"No se pudo leer la tabla Agrónomos")
	RETURN TRUE
ELSEIF li_Contador = 0 THEN
	messagebox("Atención","Código Agrónomo No Existe Para el Productor" + &
					"~n~nSeleccionada, Ingrese Otro Por Favor",Exclamation!)
	RETURN TRUE
ELSE	
	RETURN FALSE	
END IF

end function

public function boolean noexistecalibre (string calibre);Integer li_Contador

Select Count(*)
Into 	:li_Contador
From	dba.variecalibre
Where vaca_calibr	=	:calibre;

IF sqlca.sqlcode = -1 THEN
	F_ErrorBaseDatos(sqlca,"No se pudo leer la tabla VarieCalibre")
	RETURN TRUE
ELSEIF li_Contador = 0 THEN
	messagebox("Atención","Código Calibre no existe" + &
					"~n~n,Ingrese Otro Por Favor",Exclamation!)
	RETURN TRUE
ELSE	
	RETURN FALSE	
END IF

end function

public function integer noexisteembalaje (string as_embalaje);Integer li_Contador 

Select Count(*)
Into :li_Contador
From dba.embalajes
Where emba_codigo = :as_embalaje;


IF sqlca.sqlcode = -1 THEN
	F_ErrorBaseDatos(sqlca,"No se pudo leer la tabla Embalaje")
	RETURN 1
ELSEIF li_Contador = 0 THEN
	messagebox("Atención","Código de Embalaje" + &
					"~n~n, Ingrese Otro Por Favor",Exclamation!)
	RETURN 1
ELSE	
	RETURN 0	
END IF

end function

public function boolean noexisteespecie (integer especie);Integer li_Contador

Select Count(*)
Into :li_Contador
From dba.especies
Where espe_codigo = :especie;

IF sqlca.sqlcode = -1 THEN
	F_ErrorBaseDatos(sqlca,"No se pudo leer la tabla Especies")
	RETURN TRUE
ELSEIF li_Contador = 0 THEN
	messagebox("Atención","Código Especie No Existe Para la Zona" + &
					"~n~nSeleccionada, Ingrese Otro Por Favor",Exclamation!)
	RETURN TRUE
ELSE	
	RETURN FALSE	
END IF

end function

public function integer noexisteetiqueta (integer etiqueta);Integer li_Contador 

Select Count(*)
Into :li_Contador
From dba.etiquetas
Where etiq_codigo = :etiqueta;


IF sqlca.sqlcode = -1 THEN
	F_ErrorBaseDatos(sqlca,"No se pudo leer la tabla Etiqueta")
	RETURN 1
ELSEIF li_Contador = 0 THEN
	messagebox("Atención","Código de Etiqueta" + &
					"~n~n, Ingrese Otro Por Favor",Exclamation!)
	RETURN 1
ELSE	
	RETURN 0	
END IF

end function

public function boolean noexistegrupo (string data);Integer li_Contador, li_Grupo

li_grupo = Integer(Data)
Select Count(*)
Into :li_Contador
From dba.admagrupousuario
Where grpo_codigo = : li_grupo;

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

public function boolean noexisteinspector (integer inspector);Integer li_Contador

Select Count(*)
	Into :li_Contador
	From dba.ctlcalinspectores
	Where ccin_codigo = :inspector ;

IF sqlca.sqlcode = -1 THEN
	F_ErrorBaseDatos(sqlca,"No se pudo leer la tabla Inspectores")
	RETURN TRUE
ELSEIF li_Contador = 0 THEN
	messagebox("Atención","Código Inspectores No Existe Para la Zona" + &
					"~n~nSeleccionada, Ingrese Otro Por Favor",Exclamation!)
	RETURN TRUE
ELSE	
	RETURN FALSE	
END IF

end function

public function boolean noexistepacking (integer codigo); 
Integer li_Contador

Select Count(*)
Into :li_Contador
From dba.plantadesp
Where plde_codigo = :codigo
AND   plde_tipopl = 2;  

IF sqlca.sqlcode = -1 THEN
	F_ErrorBaseDatos(sqlca,"No se pudo leer la tabla Planta Desp")
	RETURN TRUE
ELSEIF li_Contador = 0 THEN
	messagebox("Atención","Código Tipo de Packing no existe " + &
					"~n~n Ingrese Otro Por Favor",Exclamation!)
	RETURN TRUE
ELSE	
	RETURN FALSE	
END IF

 
 RETURN FALSE
end function

public function boolean noexisteplanta (integer planta);Integer li_Contador, li_cliente,li_zona

li_cliente	=	Integer(istr_mant.argumento[7])

Select Count(*)
Into :li_Contador
From dba.plantadesp
Where plde_codigo = :planta
and	plde_tipopl	=	1;

IF sqlca.sqlcode = -1 THEN
	F_ErrorBaseDatos(sqlca,"No se pudo leer la tabla Plantas Despachos")
	RETURN TRUE
ELSEIF li_Contador = 0 THEN
	messagebox("Atención","Código Planta No Existe Para la Zona" + &
					"~nSeleccionada, Ingrese Otro Por Favor",Exclamation!)
	RETURN TRUE
ELSE	
	RETURN FALSE	
END IF
	
end function

public function boolean noexistetecnico (integer tecnico);Integer li_Contador, li_zona

li_zona		=	dw_2.Object.zona_codigo[1]

Select Count(*)
Into :li_Contador
From dba.ctlcaltecnicos
Where cctc_codigo = :tecnico
and   zona_codigo = :li_zona;

IF sqlca.sqlcode = -1 THEN
	F_ErrorBaseDatos(sqlca,"No se pudo leer la tabla Tecnicos")
	RETURN TRUE
ELSEIF li_Contador = 0 THEN
	messagebox("Atención","Código Tecnico No Existe Para la Zona" + &
					"~n~nSeleccionada, Ingrese Otro Por Favor",Exclamation!)
	RETURN TRUE
ELSE	
	RETURN FALSE	
END IF

end function

public function boolean noexistezona (integer zona);Integer li_Contador

Select Count(*)
Into :li_Contador
From "dba".zonas
Where zona_codigo = :zona;

IF sqlca.sqlcode = -1 THEN
	F_ErrorBaseDatos(sqlca,"No se pudo leer la tabla Zonas")
	RETURN TRUE
ELSEIF li_Contador = 0 THEN
	Messagebox("Atención","Código de Zona No Existe, Ingrese Otro Por Favor",Exclamation!)
	RETURN TRUE
ELSE	
	RETURN FALSE	
END IF
	
end function

public function long nuevolote (integer cliente, integer planta);Integer li_Contador

Select IsNull(Max(cclo_numero),0)
Into :li_Contador
From dba.ctlcallotes
Where clie_codigo = :cliente
and   plde_codigo = :planta;

IF sqlca.sqlcode = -1 THEN
	F_ErrorBaseDatos(sqlca,"Problemas en tabla CTLCALLOTES")
	RETURN 1
END IF

return li_Contador
end function

protected function integer wf_modifica ();IF dw_1.AcceptText() = -1 THEN RETURN -1
IF dw_2.AcceptText() = -1 THEN RETURN -1
IF dw_3.AcceptText() = -1 THEN RETURN -1
IF (dw_1.ModifiedCount() + dw_1.DeletedCount()) > 0 THEN RETURN 0
IF dw_2.ModifiedCount() > 0 THEN RETURN 0
IF dw_3.ModifiedCount() > 0 THEN RETURN 0

RETURN 1
end function

protected function boolean wf_actualiza_db (boolean borrando);Boolean	lb_AutoCommit, lb_Retorno

IF Not dw_2.uf_check_required(0) THEN RETURN False

IF Not dw_1.uf_validate(0) THEN RETURN False

lb_AutoCommit		=	sqlca.AutoCommit
sqlca.AutoCommit	=	False

IF Borrando THEN
	IF dw_1.Update(True, False) = 1 THEN
		IF dw_2.Update(True,False) =	1	THEN
//			IF dw_3.Update(True, False) = 1 THEN
				Commit;
				
				IF sqlca.SQLCode <> 0 THEN
					F_ErrorBaseDatos(sqlca, This.Title)
				ELSE
					lb_Retorno	=	True
					
					dw_1.ResetUpdate()
					dw_2.ResetUpdate()
					//dw_3.ResetUpdate()
				END IF
//			ELSE
//				F_ErrorBaseDatos(sqlca, This.Title)
//				
//				RollBack;
//			END IF
		ELSE
			F_ErrorBaseDatos(sqlca, This.Title)
			
			RollBack;
		END IF
	ELSE
		F_ErrorBaseDatos(sqlca, This.Title)
		
		RollBack;
	END IF
ELSE
//	IF dw_3.Update(True, False) = 1 THEN
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
//	ELSE
//		F_ErrorBaseDatos(sqlca, This.Title)
//		
//		RollBack;
//	END IF
END IF

sqlca.AutoCommit	=	lb_AutoCommit

RETURN lb_Retorno

end function

public function boolean existeencabezado (); integer li_cliente, li_conave, li_especie, li_variedad, li_etiq,li_cuenta
 string  ls_tiponave, ls_calibre, ls_emba
 date    ld_fechaemb
 Long    ll_productor
 
 li_cliente   = 81
 ls_tiponave  = istr_mant.argumento[15]
 li_conave    = integer(istr_mant.argumento[1])
 li_especie   = gi_codespecie
 li_variedad  = integer(istr_mant.argumento[11])
 li_etiq      = integer(istr_mant.argumento[16])
 ll_productor = Long(istr_mant.argumento[14])
 ls_calibre   = istr_mant.argumento[23]
 ls_emba      = istr_mant.argumento[17]
 ld_fechaemb  = date (istr_mant.argumento[18])
 
 SELECT count(*)
    INTO :li_cuenta 
    FROM "dba"."ctlcaldestinosenc"  
   WHERE ( "dba"."ctlcaldestinosenc"."clie_codigo" = :li_cliente ) AND  
         ( "dba"."ctlcaldestinosenc"."nave_tipotr" = :ls_tiponave ) AND  
         ( "dba"."ctlcaldestinosenc"."nave_codigo" = :li_conave ) AND  
         ( "dba"."ctlcaldestinosenc"."espe_codigo" = :li_especie ) AND  
         ( "dba"."ctlcaldestinosenc"."vari_codigo" = :li_variedad ) AND  
         ( "dba"."ctlcaldestinosenc"."etiq_codigo" = :li_etiq ) AND  
         ( "dba"."ctlcaldestinosenc"."prod_codigo" = :ll_productor ) AND  
         ( "dba"."ctlcaldestinosenc"."vaca_calibr" = :ls_calibre ) AND  
         ( "dba"."ctlcaldestinosenc"."emba_codigo" = :ls_emba ) AND  
         ( "dba"."ctlcaldestinosenc"."ccde_fecemb" = :ld_fechaemb )   ;

IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla ctlcaldestinosenc")
	return false
ELSEIF sqlca.SQLCode = 100 THEN
	return false
END IF 

IF li_cuenta > 0 THEN
	RETURN TRUE
ELSE
	RETURN FALSE
END IF 	
end function

public function boolean existenumerolote (string as_columna, string as_valor);Integer	li_lote, li_planta,li_especie, li_variedad, li_packing, li_zona
Long		ll_nfolio, ll_numero, ll_planilla, ll_productor
String	ls_Embalaje, ls_calibre
Date		ld_Fecemb
Boolean	lb_Retorno = False

//li_planta		=	dw_2.Object.plde_codigo[1]
//ll_nfolio 		=	dw_2.Object.cclo_numero[1]

ll_numero 		=	dw_2.Object.ccpe_numero[1]
ll_productor	=	dw_2.Object.prod_codigo[1]
li_especie		=	dw_2.Object.espe_codigo[1]
li_variedad		=	dw_2.Object.vari_codigo[1]
ls_embalaje		=	dw_3.Object.emba_codigo[1]
li_packing		=	dw_3.Object.plde_codpak[1]
ls_calibre		=	dw_3.Object.vaca_calibr[1]
ld_fecemb		=	dw_3.Object.cclo_fecemb[1]

CHOOSE CASE as_columna
	
	CASE "plde_codigo"
		li_planta		=	Integer(as_valor)
		
	CASE "cclo_numero"
		ll_nfolio 		=	Long(as_valor)

	CASE "ccpe_numero"
		ll_numero 		=	Long(as_valor)

	CASE "prod_codigo"
		ll_productor	=	Long(as_valor)
		
	CASE "espe_codigo"
		li_especie		=	Integer(as_valor)
		
	CASE "vari_codigo"
		li_variedad		=	Integer(as_valor)
		
	CASE "emba_codigo"
		ls_embalaje		=	as_valor
		
	CASE "plde_codpak"
		li_packing		=	Integer(as_valor)
		
	CASE "vaca_calibr"
		ls_calibre		=	as_valor
		
	CASE "cclo_fecemb"
		ld_fecemb		=	Date(as_valor)
		
END CHOOSE

SELECT	cclo_numero
	INTO	 :li_lote
	FROM	"dba".CTLCALLOTES
	WHERE	plde_codigo	=	:li_planta
	AND   prod_codigo =  :ll_productor
	AND   espe_codigo =  :li_especie
	AND   vari_codigo =  :li_variedad
	AND   emba_codigo =  :ls_embalaje
	AND   vaca_calibr =  :ls_calibre
	AND   plde_codpak =  :li_packing
	AND   cclo_fecemb =  :ld_fecemb;
				
IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla CTLCALLOTES")
ELSEIF sqlca.SQLCode = 0 THEN
	IF li_lote > 0 THEN

		dw_3.SetRedraw(False)
			
		dw_3.Retrieve(gi_CodExport, li_planta, li_lote)
		istr_mant.argumento[19]=String(dw_3.Object.cclo_tamlot[1])
		dw_3.SetRedraw(True)
			
		SELECT ccpe_numero, zona_codigo
			INTO	 :ll_planilla, :li_zona
			FROM	 "dba".CTLCALPLACUANINSPUVAENC
			WHERE	 plde_codigo =	:li_planta
			AND    cclo_numero = :li_lote
			AND    ccpe_numero = :ll_numero;
		IF sqlca.SQLCode = -1 THEN
			F_errorbasedatos(sqlca,"Lectura tabla CTLCALPLACUANINSPUVAENC")
		ELSEIF sqlca.SQLCode = 0 THEN
			IF ll_planilla <> ll_numero THEN
				messagebox("Atención","Nueva Planilla para un Lote Existente," + &
				"",Exclamation!)
				istr_mant.argumento[1]	= String(li_lote)
			ELSE
				istr_mant.argumento[1]	= String(li_lote)
				istr_mant.argumento[2]	= String(ll_planilla)
				istr_mant.argumento[3]	= String(li_zona)							
				istr_mant.argumento[4]	= String(li_planta)
				istr_mant.argumento[7]	= String(gi_CodExport)
				istr_mant.argumento[8]	= String(ll_productor)
				istr_mant.argumento[9]	= String(li_especie)
				istr_mant.argumento[10]	= String(li_variedad)
				istr_mant.argumento[17]	= ls_embalaje
				istr_mant.argumento[18]	= String(ld_fecemb)
				istr_mant.argumento[20]	= String(li_packing)
				istr_mant.argumento[21]	= ls_calibre
				This.TriggerEvent("ue_recuperadatos")
				//dw_1.Enabled	=	True
				//dw_2.Enabled	=	True
				//dw_3.Enabled	=	True							
			END IF

			dw_2.GetChild("plde_codigo", idwc_plantas)
			idwc_plantas.SetTransObject(sqlca)
			idwc_plantas.Retrieve(Integer(istr_mant.argumento[7]),1)
	
			dw_3.GetChild("plde_codpak", idwc_packings)
			idwc_packings.SetTransObject(sqlca)
			idwc_packings.Retrieve(Integer(istr_mant.argumento[7]),2,Integer(istr_mant.argumento[3]))
				
			dw_2.GetChild("ccag_codigo", idwc_agronomos)
			idwc_agronomos.SetTransObject(sqlca)
			idwc_agronomos.Retrieve(0,Integer(istr_mant.argumento[3]))
	
			dw_2.GetChild("ccin_codigo", idwc_inspectores)
			idwc_inspectores.SetTransObject(sqlca)
			idwc_inspectores.Retrieve(Integer(istr_mant.argumento[3]))
				
			dw_2.GetChild("prod_codigo", idwc_productores)
			idwc_productores.SetTransObject(sqlca)
			idwc_productores.Retrieve(Integer(istr_mant.argumento[7]),Integer(istr_mant.argumento[3]))
			

			
			lb_Retorno	=	False
		END IF
	END IF
END IF

RETURN lb_Retorno
end function

public function boolean noexistenumerolote (string as_columna, string as_valor);Integer	li_lote, li_planta, li_especie, li_variedad, li_packing, li_zona
Long		ll_nfolio, ll_numero, ll_planilla, ll_productor
String	ls_Embalaje, ls_calibre
Date		ld_Fecemb
Boolean	lb_Retorno = False

li_planta		=	dw_2.Object.plde_codigo[1]
ll_nfolio 		=	dw_2.Object.cclo_numero[1]

ll_numero 		=	dw_2.Object.ccpe_numero[1]
ll_productor	=	dw_2.Object.prod_codigo[1]
li_especie		=	dw_2.Object.espe_codigo[1]
li_variedad		=	dw_2.Object.vari_codigo[1]
ls_embalaje		=	dw_3.Object.emba_codigo[1]
li_packing		=	dw_3.Object.plde_codpak[1]
ls_calibre		=	dw_3.Object.vaca_calibr[1]
ld_fecemb		=	dw_3.Object.cclo_fecemb[1]

CHOOSE CASE as_columna
	
	CASE "plde_codigo"
		li_planta		=	Integer(as_valor)
		
	CASE "cclo_numero"
		ll_nfolio 		=	Long(as_valor)

	CASE "ccpe_numero"
		ll_numero 		=	Long(as_valor)

	CASE "prod_codigo"
		ll_productor	=	Long(as_valor)
		
	CASE "espe_codigo"
		li_especie		=	Integer(as_valor)
		
	CASE "vari_codigo"
		li_variedad		=	Integer(as_valor)
		
	CASE "emba_codigo"
		ls_embalaje		=	as_valor
		
	CASE "plde_codpak"
		li_packing		=	Integer(as_valor)
		
	CASE "vaca_calibr"
		ls_calibre		=	as_valor
		
	CASE "cclo_fecemb"
		ld_fecemb		=	Date(as_valor)
		
END CHOOSE

SELECT	cclo_numero
	INTO	 :li_lote
	FROM	"dba".CTLCALLOTES
	WHERE	plde_codigo	=	:li_planta
	AND   prod_codigo =  :ll_productor
	AND   espe_codigo =  :li_especie
	AND   vari_codigo =  :li_variedad
	AND   emba_codigo =  :ls_embalaje
	AND   vaca_calibr =  :ls_calibre
	AND   plde_codpak =  :li_packing
	AND   cclo_fecemb =  :ld_fecemb;
				
IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla CTLCALLOTES")
ELSEIF sqlca.SQLCode = 0 THEN
	IF li_lote > 0 THEN

		dw_3.SetRedraw(False)
			
		dw_3.Retrieve(gi_CodExport, li_planta, li_lote)
		istr_mant.argumento[19]=String(dw_3.Object.cclo_tamlot[1])
		dw_3.SetRedraw(True)
			
		SELECT ccpe_numero, zona_codigo
			INTO	 :ll_planilla, :li_zona
			FROM	 "dba".CTLCALPLACUANINSPUVAENC
			WHERE	 plde_codigo =	:li_planta
			AND    cclo_numero = :li_lote
			AND    ccpe_numero = :ll_numero;
		IF sqlca.SQLCode = -1 THEN
			F_errorbasedatos(sqlca,"Lectura tabla CTLCALPLACUANINSPUVAENC")
		ELSEIF sqlca.SQLCode = 0 THEN
			IF ll_planilla <> ll_numero THEN
				messagebox("Atención","Nueva Planilla para un Lote Existente," + &
				"",Exclamation!)
				istr_mant.argumento[1]	= String(li_lote)
			ELSE
				istr_mant.argumento[1]	= String(li_lote)
				istr_mant.argumento[2]	= String(ll_planilla)
				istr_mant.argumento[3]	= String(li_zona)							
				istr_mant.argumento[4]	= String(li_planta)
				istr_mant.argumento[7]	= String(gi_CodExport)
				istr_mant.argumento[8]	= String(ll_productor)
				istr_mant.argumento[9]	= String(li_especie)
				istr_mant.argumento[10]	= String(li_variedad)
				istr_mant.argumento[17]	= ls_embalaje
				istr_mant.argumento[18]	= String(ld_fecemb)
				istr_mant.argumento[20]	= String(li_packing)
				istr_mant.argumento[21]	= ls_calibre
				This.TriggerEvent("ue_recuperadatos")
				//dw_1.Enabled	=	True
				//dw_2.Enabled	=	True
				//dw_3.Enabled	=	True							
			END IF

			dw_2.GetChild("plde_codigo", idwc_plantas)
			idwc_plantas.SetTransObject(sqlca)
			idwc_plantas.Retrieve(Integer(istr_mant.argumento[7]),1)
	
			dw_3.GetChild("plde_codpak", idwc_packings)
			idwc_packings.SetTransObject(sqlca)
			idwc_packings.Retrieve(Integer(istr_mant.argumento[7]),2,Integer(istr_mant.argumento[3]))
				
			dw_2.GetChild("ccag_codigo", idwc_agronomos)
			idwc_agronomos.SetTransObject(sqlca)
			idwc_agronomos.Retrieve(0,Integer(istr_mant.argumento[3]))
	
			dw_2.GetChild("ccin_codigo", idwc_inspectores)
			idwc_inspectores.SetTransObject(sqlca)
			idwc_inspectores.Retrieve(Integer(istr_mant.argumento[3]))
				
			dw_2.GetChild("prod_codigo", idwc_productores)
			idwc_productores.SetTransObject(sqlca)
			idwc_productores.Retrieve(Integer(istr_mant.argumento[7]),Integer(istr_mant.argumento[3]))
			

			
			lb_Retorno	=	False
		END IF
	END IF
END IF

RETURN lb_Retorno
end function

public subroutine habilitaingreso (string ls_columna);Boolean	lb_Estado = True
Date ld_fecha 

dw_2.AcceptText()
dw_3.AcceptText()
//dw_4.AcceptText()

IF ls_Columna <> "nave_tipotr" AND &
	(dw_2.Object.nave_tipotr[1]) = '' OR IsNull(dw_2.Object.nave_tipotr[1]) THEN
	lb_Estado	=	False
END IF

IF ls_Columna <> "nave_codigo" AND &
	(dw_2.Object.nave_codigo[1]) = 0 OR IsNull(dw_2.Object.nave_codigo[1]) THEN
	lb_Estado	=	False
END IF

IF ls_Columna <> "ccde_bodega" AND &
	(dw_2.Object.ccde_bodega[1]) = '' OR IsNull(dw_2.Object.ccde_bodega[1]) THEN
	lb_Estado	=	False
END IF

IF ls_Columna <> "puer_codigo" AND &
	(dw_2.Object.puer_codigo[1]) = 0 OR IsNull(dw_2.Object.puer_codigo[1]) THEN
	lb_Estado	=	False
END IF

IF ls_Columna <> "ccde_lugins" AND &
	(dw_2.Object.ccde_lugins[1]) = '' OR IsNull(dw_2.Object.ccde_lugins[1]) THEN
	lb_Estado	=	False
END IF

IF ls_Columna <> "ccin_codigo" AND &
	(dw_2.Object.ccin_codigo[1]) = 0 OR IsNull(dw_2.Object.ccin_codigo[1]) THEN
	lb_Estado	=	False
END IF

IF ls_Columna <> "reci_codigo" AND &
	(dw_2.Object.reci_codigo[1]) = 0 OR IsNull(dw_2.Object.reci_codigo[1]) THEN
	lb_Estado	=	False
END IF

IF ls_Columna <> "vari_codigo" AND &
	(dw_2.Object.vari_codigo[1]) = 0 OR IsNull(dw_2.Object.vari_codigo[1]) THEN
	lb_Estado	=	False
END IF

IF ls_Columna <> "prod_codigo" AND &
	(dw_3.Object.prod_codigo[1]) = 0 OR IsNull(dw_3.Object.prod_codigo[1]) THEN
	lb_Estado	=	False
END IF

IF ls_Columna <> "vaca_calibr" AND &
	(dw_3.Object.vaca_calibr[1]) = '' OR IsNull(dw_3.Object.vaca_calibr[1]) THEN
	lb_Estado	=	False
END IF

IF ls_Columna <> "etiq_codigo" AND &
	(dw_3.Object.etiq_codigo[1]) = 0 OR IsNull(dw_3.Object.etiq_codigo[1]) THEN
	lb_Estado	=	False
END IF

IF ls_Columna <> "emba_codigo" AND &
	(dw_3.Object.emba_codigo[1]) = '' OR IsNull(dw_3.Object.emba_codigo[1]) THEN
	lb_Estado	=	False
END IF

IF ls_Columna <> "ccde_fecemb" AND &
	(dw_3.Object.ccde_fecemb[1]) = ld_fecha OR IsNull(dw_3.Object.ccde_fecemb[1]) THEN
	lb_Estado	=	False
END IF

pb_ins_det.Enabled	=	lb_Estado





end subroutine

public function boolean duplicado (string data);Integer li_grupo, LI_CONTADOR

li_grupo = Integer(data)		

SELECT	Count(*)
	INTO	:li_Contador
	FROM	dba.ADMAGRUPOUSUARIO
	WHERE	GRPO_CODIGO	=	:li_grupo;

IF sqlca.sqlcode = -1 THEN
	F_ErrorBaseDatos(sqlca,"No se pudo leer la tabla admagruposuario")
ELSEIF li_Contador = 0 THEN
	RETURN FALSE
ELSE
	RETURN TRUE
END IF
end function

public function integer noexistetiponave (string tiponave);Integer li_Contador 


  SELECT count (*)  
    INTO :li_contador
    FROM "dba"."naves"  
   WHERE "dba"."naves"."nave_tipotr" =:tiponave   ;

IF sqlca.sqlcode = -1 THEN
	F_ErrorBaseDatos(sqlca,"No se pudo leer la tabla Productor")
	RETURN 1
ELSEIF li_Contador = 0 THEN
	messagebox("Atención","El tipo de Transporte No Existe " + &
					"~n~nSeleccionada, Ingrese Otro Por Favor",Exclamation!)
	RETURN 1
ELSE	
	RETURN 0
END IF

end function

public function integer noexistenavecodigo (integer navecodigo);Integer li_Contador 

  SELECT count (*)  
    INTO :li_contador
    FROM "dba"."naves"  
   WHERE "dba"."naves"."nave_codigo" =:navecodigo  ;


IF sqlca.sqlcode = -1 THEN
	F_ErrorBaseDatos(sqlca,"No se pudo leer la tabla Productor")
	RETURN 1
ELSEIF li_Contador = 0 THEN
	messagebox("Atención","El tipo de Transporte No Existe " + &
					"~n~nSeleccionada, Ingrese Otro Por Favor",Exclamation!)
	RETURN 1
ELSE	
	RETURN 0
END IF

end function

public function boolean noexistevariedad (integer variedad);Integer li_Contador

Select Count(*)
Into :li_Contador
From dba.variedades
Where vari_codigo = :variedad;

IF sqlca.sqlcode = -1 THEN
	F_ErrorBaseDatos(sqlca,"No se pudo leer la tabla de Variedades")
	RETURN TRUE
ELSEIF li_Contador = 0 THEN
	messagebox("Atención","Código Variedad No Existe" + &
					"~n~n, Ingrese Otro Por Favor",Exclamation!)
	RETURN TRUE
ELSE	
	RETURN FALSE	
END IF

end function

public function integer noexistepuerto (integer puerto);Integer li_Contador 

	SELECT count(*)
    INTO :li_contador
    FROM "dba"."puertos"  
   WHERE "dba"."puertos"."puer_codigo" = :puerto   ;


IF sqlca.sqlcode = -1 THEN
	F_ErrorBaseDatos(sqlca,"No se pudo leer la tabla Productor")
	RETURN 1
ELSEIF li_Contador = 0 THEN
	messagebox("Atención","El tipo de Transporte No Existe " + &
					"~n~nSeleccionada, Ingrese Otro Por Favor",Exclamation!)
	RETURN 1
ELSE	
	RETURN 0
END IF

end function

public function integer noexisteproductor (long productor);Integer li_Contador 

Select Count(*)
Into :li_Contador
From dba.productores
Where prod_codigo = :productor;


IF sqlca.sqlcode = -1 THEN
	F_ErrorBaseDatos(sqlca,"No se pudo leer la tabla Productor")
	RETURN 1
ELSEIF li_Contador = 0 THEN
	messagebox("Atención","Código Productor No Existe Para la Zona" + &
					"~n~nSeleccionada, Ingrese Otro Por Favor",Exclamation!)
	RETURN 1
ELSE	
	RETURN 0	
END IF

end function

public function integer noexisterecibidor (long reci);Integer li_Contador 


  SELECT count(*)
    INTO :li_contador
    FROM "dba"."recibidores"  
   WHERE "dba"."recibidores"."reci_codigo" = :reci   ;



IF sqlca.sqlcode = -1 THEN
	F_ErrorBaseDatos(sqlca,"No se pudo leer la tabla Recibidores")
	RETURN 1
ELSEIF li_Contador = 0 THEN
	messagebox("Atención","El tipo de Transporte No Existe " + &
					"~n~nSeleccionada, Ingrese Otro Por Favor",Exclamation!)
	RETURN 1
ELSE	
	RETURN 0
END IF
end function

on w_maed_ctlcalplanilladestino.create
int iCurrent
call super::create
this.dw_3=create dw_3
this.dw_5=create dw_5
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_3
this.Control[iCurrent+2]=this.dw_5
end on

on w_maed_ctlcalplanilladestino.destroy
call super::destroy
destroy(this.dw_3)
destroy(this.dw_5)
end on

event ue_seleccion();call super::ue_seleccion;String	ls_nula

SetNull(ls_nula)

//istr_busq.argum[1]	=	String(gi_CodExport)
//istr_busq.argum[2]	=	""

OpenWithParm(w_busc_ctlcaldestinosenc, istr_busq)
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
	istr_mant.argumento[11] = istr_busq.argum[11]
	istr_mant.argumento[12] = istr_busq.argum[12]
	istr_mant.argumento[13] = istr_busq.argum[13]
	istr_mant.argumento[14] = istr_busq.argum[14]
	istr_mant.argumento[15]	= istr_busq.argum[15]  
	istr_mant.argumento[16]	= istr_busq.argum[16]		
	istr_mant.argumento[17]	= istr_busq.argum[17]
	istr_mant.argumento[18]	= istr_busq.argum[18]
	istr_mant.argumento[19]	= istr_busq.argum[19]
	istr_mant.argumento[20]	= istr_busq.argum[20]
	istr_mant.argumento[21]	= istr_busq.argum[21]
	istr_mant.argumento[22]	= istr_busq.argum[22]
	istr_mant.argumento[23]	= istr_busq.argum[23]				
	dw_2.object.clie_codigo[1] = gi_codexport
	dw_2.object.espe_codigo[1] = gi_codespecie
	dw_2.object.nave_tipotr[1] = (istr_mant.argumento[15])
	dw_2.object.nave_codigo[1] = Long(istr_mant.argumento[1])
	dw_2.object.ccde_bodega[1] = istr_mant.argumento[2]
	dw_2.object.puer_codigo[1] = Long(istr_mant.argumento[3])
	dw_2.object.ccde_lugins[1] = istr_mant.argumento[4]
	dw_2.object.ccde_fecarr[1] = Date(istr_mant.argumento[5])
	dw_2.object.ccde_fecins[1] = Date(istr_mant.argumento[6])
	dw_2.object.ccde_fecfum[1] = Date(istr_mant.argumento[7])
	dw_2.object.ccde_temmax[1] = Long(istr_mant.argumento[8])
	dw_2.object.ccde_temmin[1] = Long(istr_mant.argumento[9])
	dw_2.object.ccde_temfre[1] = Long(istr_mant.argumento[10])
	dw_2.object.vari_codigo[1] = Long(istr_mant.argumento[11])
	dw_2.object.ccin_codigo[1] = Long(istr_mant.argumento[12])
	dw_2.object.reci_codigo[1] = Long(istr_mant.argumento[13])
	dw_2.object.etiq_codigo[1] = Long(istr_mant.argumento[16])
	dw_2.object.prod_codigo[1] = Long(istr_mant.argumento[14])
	dw_2.object.vaca_calibr[1] = istr_mant.argumento[23]
	dw_2.object.emba_codigo[1] = istr_mant.argumento[17]
	dw_2.object.ccde_fecemb[1] = date(istr_mant.argumento[18])		
	dw_3.object.clie_codigo[1] = gi_codexport
	dw_3.object.espe_codigo[1] = gi_codespecie
	dw_3.object.nave_tipotr[1] = (istr_mant.argumento[15])
	dw_3.object.nave_codigo[1] = Long(istr_mant.argumento[1])
	dw_3.object.prod_codigo[1] = Long(istr_mant.argumento[14])
	dw_3.object.etiq_codigo[1] = Long(istr_mant.argumento[16])
	dw_3.object.ccde_fecemb[1] = Date(istr_mant.argumento[18])
	dw_3.object.emba_codigo[1] = istr_mant.argumento[17]
	dw_3.object.vaca_calibr[1] = istr_mant.argumento[23]
	dw_3.object.ccde_fecemb[1] = date(istr_mant.argumento[18])
	dw_3.object.vari_codigo[1] = Long(istr_mant.argumento[11])			
	This.TriggerEvent("ue_recuperadatos")
ELSE
	pb_buscar.SetFocus()
END IF
end event

event ue_recuperadatos;Long ll_fila_d, ll_fila_f, respuesta
Integer li_grupo, li_especie,li_variedad, especie, variedad, secuencia, li_secue
String ls_Usuario, ls_embarque
ls_Usuario	=	Upper(Gstr_Us.Nombre)

	DO
		ll_fila_f	=	dw_2.Retrieve(gi_CodExport, integer(istr_mant.argumento[1]),istr_mant.argumento[15], &  
					   	gi_codespecie, integer(istr_mant.argumento[11]), integer(istr_mant.argumento[16]), &
							Long(istr_mant.argumento[14]), istr_mant.argumento[23],istr_mant.argumento[17], &
							date(istr_mant.argumento[18]))		
							
		ll_fila_f	=	dw_3.Retrieve(gi_CodExport, integer(istr_mant.argumento[1]),istr_mant.argumento[15], &
							gi_codespecie, integer(istr_mant.argumento[11]), integer(istr_mant.argumento[16]), &
							Long(istr_mant.argumento[14]), istr_mant.argumento[23],istr_mant.argumento[17], &
							date(istr_mant.argumento[18]))		
		
		IF ll_fila_f = -1 THEN
			respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
			Information!, RetryCancel!)
		ELSE
			DO
				ll_fila_d	=	dw_1.Retrieve(gi_CodExport,gi_codespecie ,Integer(istr_mant.argumento[11]) , &
									Integer(istr_mant.argumento[16]),Long(istr_mant.argumento[14]), &
									istr_mant.argumento[23], istr_mant.argumento[17], date(istr_mant.argumento[18]), 1)
									
				IF ll_fila_d = -1 THEN
					respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
					Information!, RetryCancel!)
					pb_ins_det.SetFocus()				
				ELSEIF			ll_fila_d	>	0	THEN
					li_Grupo = BuscaGrupo(ls_Usuario)	
					IF ((li_Grupo =	6) OR (li_grupo = 1)) OR istr_mant.Argumento[22] =	'0' OR dw_1.GetItemStatus(il_Fila, 0, Primary!) = NewModified!  THEN 								
						pb_eli_det.Enabled	=	True
						pb_imprimir.Enabled  =	True
						pb_ins_det.Enabled	=	True
						dw_1.SetRow(1)
						dw_1.SelectRow(1,True)
						dw_1.SetFocus()			
						dw_2.Enabled =	True
						dw_3.Enabled =	True									
					ELSE				
						dw_2.Enabled = False
						dw_3.Enabled = False
						istr_mant.Solo_Consulta =	True 	
						pb_eliminar.Enabled		=	False
						pb_grabar.Enabled			=	False
						pb_imprimir.Enabled		=	True
						pb_ins_det.Enabled		=	False
						pb_eli_det.Enabled		=	False				
					END IF 						
				END IF
			LOOP WHILE respuesta = 1
			IF respuesta = 2 THEN Close(This)
			dw_2.SetRedraw(True)
			dw_2.setitemstatus(1,0,primary!,notmodified!)
			dw_1.setitemstatus(1,0,primary!,notmodified!)
			dw_3.setitemstatus(1,0,primary!,notmodified!)
			pb_grabar.enabled = True
		END IF
	LOOP WHILE respuesta = 1
	IF respuesta = 2 THEN Close(This)
end event

event ue_nuevo();Long		ll_modif1, ll_modif2, ll_modif3, ll_modif4

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
dw_2.settransobject(sqlca)
dw_2.InsertRow(0)
dw_2.SetRedraw(True)

dw_2.SetFocus()
dw_2.SetColumn(0)

dw_3.SetRedraw(False)
dw_3.Reset()
dw_3.InsertRow(0)
dw_3.settransobject(sqlca)
dw_3.SetRedraw(True)
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

event open;/*
	ARGUMENTOS UTILIZADOS

Argumento[1]	=	Nave_Codigo 
Argumento[2]	=	Ccde_Bodega
Argumento[3]	=	Puer_Codigo
Argumento[4]	=	Ccde_Lugins
Argumento[5]	=	Ccde_Fecarr
Argumento[6]	=	Ccde_Fecins
Argumento[7]	=	Ccde_Fecfum
Argumento[8]	=	Ccde_Temax
Argumento[9]	=	Ccde_Temmin
Argumento[10]	=	Ccde_Temfre
Argumento[11]	=	Vari_Codigo
Argumento[12]	=	Ccin_Codigo
Argumento[13]	=	Reci_Codigo
Argumento[14]	=	Prod_Codigo
Argumento[15]	=	Nave_Codigo
Argumento[16]	=	Etiq_Codigo
Argumento[17]	=	Emba_Codigo
Argumento[18]	=	Ccde_Fecemb
Argumento[23]	=	Vaca_Calibr
*/

x				= 0
y				= 0
This.Height	= 2020
im_menu		= m_principal

This.Icon									=	Gstr_apl.Icono
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

istr_mant.UsuarioSoloConsulta	=	OpcionSoloConsulta()

pb_nuevo.PostEvent(Clicked!)

GrabaAccesoAplicacion(True, id_FechaAcceso, it_HoraAcceso, &
							This.Title, "Acceso a Aplicación", 1)

buscar	= "Código:Ncodigo,Descripción:Sconcepto"
ordenar	= "Código:codigo,Descripción:concepto"


//VARIEDAD//
dw_2.GetChild("vari_codigo", idwc_variedades)
idwc_variedades.SetTransObject(sqlca)
idwc_variedades.Retrieve(gi_CodEspecie)
idwc_variedades.SetSort("vari_nombre A")
idwc_variedades.Sort()
dw_2.SetItem(1, "vari_codigo",gi_CodVariedad)

//INSPECTOR//
dw_2.GetChild("ccin_codigo", idwc_inspectores)
idwc_inspectores.SetTransObject(sqlca)
idwc_inspectores.Retrieve()
idwc_inspectores.SetSort("ccin_nombre A")
idwc_inspectores.Sort()

//PRODUCTOR//
dw_3.GetChild("prod_codigo", idwc_productores)
idwc_productores.SetTransObject(sqlca)
idwc_productores.Retrieve(gi_codZona)
idwc_productores.SetSort("prod_nombre A")
idwc_productores.Sort()
dw_3.SetItem(1, "prod_codigo",gi_codProductor)

//ETIQUETA//
dw_3.GetChild("etiq_codigo", idwc_etiqueta)
idwc_etiqueta.SetTransObject(sqlca)
idwc_etiqueta.Retrieve()
idwc_etiqueta.SetSort("etiq_nombre A")
idwc_etiqueta.Sort()

//EMBALAJE//
dw_3.GetChild("emba_codigo", idwc_embalaje)
idwc_embalaje.SetTransObject(sqlca)
idwc_embalaje.Retrieve()
idwc_embalaje.SetSort("emba_nombre A")
idwc_embalaje.Sort()

//CALIBRE//
dw_3.GetChild("vaca_calibr", idwc_calibre)
idwc_calibre.SetTransObject(sqlca)
idwc_calibre.Retrieve(gi_CodEspecie,gi_codvariedad)
idwc_calibre.SetSort("vari_codigo A")
idwc_calibre.Sort()
 
//PUERTO//
dw_2.GetChild("puer_codigo", idwc_puerto)
idwc_puerto.SetTransObject(sqlca)
idwc_puerto.Retrieve(1)

//RECIBIDORES//
dw_2.GetChild("reci_codigo", idwc_recibidor)
idwc_recibidor.SetTransObject(sqlca)
idwc_recibidor.Retrieve()

//TRANSPORTE//
dw_2.GetChild("nave_tipotr", idwc_transporte)
idwc_transporte.SetTransObject(sqlca)
idwc_transporte.Retrieve()


istr_mant.argumento[1]	=	""
istr_mant.argumento[2]	=	""	
istr_mant.argumento[3]	=	""
istr_mant.argumento[4]	=	""	
istr_mant.argumento[5]	=	""
istr_mant.argumento[6]	=	""
istr_mant.argumento[7]	=	""
istr_mant.argumento[8]	=	""
istr_mant.argumento[9]	=	""
istr_mant.argumento[10]	=	""
istr_mant.argumento[11]	=	string (gi_codvariedad)
istr_mant.argumento[12]	=	""
istr_mant.argumento[13]	=	""
istr_mant.argumento[14]	=	""
istr_mant.argumento[15]	=	""
istr_mant.argumento[16]	=	""
istr_mant.argumento[17]	=	""
istr_mant.argumento[18]	=	""
istr_mant.argumento[19] =	""
istr_mant.argumento[20]	=	""
istr_mant.argumento[21]	=	string(gi_codespecie)
istr_mant.argumento[22]	=	""
istr_mant.argumento[23]	=	""
istr_mant.argumento[24]	=	""
istr_mant.argumento[25]	=	""
istr_mant.argumento[26]	=	""

end event

event ue_modifica_detalle();IF dw_1.RowCount()>0 THEN
	istr_mant.Agrega = False
	istr_mant.Borra  = False	
	OpenWithParm(iw_mantencion,istr_mant)
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

event ue_imprimir;SetPointer(HourGlass!)

Long		fila

istr_info.titulo	=	"INFORME DE PLANILLAS DESTINOS"
istr_info.copias	=	1

OpenWithParm(vinf,istr_info)

vinf.dw_1.DataObject	=	"dw_info_planilladestino"

vinf.dw_1.SetTransObject(sqlca)

fila	=	vinf.dw_1.Retrieve(gi_CodExport, istr_mant.argumento[15], Integer(istr_mant.argumento[1]))

IF fila	=	-1 THEN
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

event resize;//Integer	maximo, li_posic_x, li_posic_y, li_visible = 0
//
//maximo	= dw_1.width
//
//IF dw_2.width > maximo THEN maximo = dw_2.width
//
//dw_2.x					= 37 + Round((maximo - dw_2.width) / 2, 0)
//dw_2.y					= 37
//
////dw_1.x					= 37 + Round((maximo - dw_1.width) / 2, 0)
////dw_1.y					= 64 + dw_2.Height
////dw_1.height				= This.WorkSpaceHeight() - dw_1.y - 41
//
//gb_1.x 					= This.WorkSpaceWidth() - 310
//gb_1.y 					= 5
//gb_1.width				= 275
//
//li_posic_x				= This.WorkSpaceWidth() - 250
//li_posic_y				= gb_1.y + 88
//
//IF pb_buscar.Visible THEN
//	pb_buscar.x				= li_posic_x
//	pb_buscar.y				= li_posic_y
//	pb_buscar.width		= 156
//	pb_buscar.height		= 133
//	li_visible ++
//	li_posic_y += 180
//END IF
//
//IF pb_nuevo.Visible THEN
//	pb_nuevo.x				= li_posic_x
//	pb_nuevo.y				= li_posic_y
//	pb_nuevo.width			= 156
//	pb_nuevo.height		= 133
//	li_visible ++
//	li_posic_y += 180
//END IF
//
//IF	pb_eliminar.Visible THEN
//	pb_eliminar.x			= li_posic_x
//	pb_eliminar.y			= li_posic_y
//	pb_eliminar.width		= 156
//	pb_eliminar.height	= 133
//	li_visible ++
//	li_posic_y += 180
//END IF
//
//IF pb_grabar.Visible THEN
//	pb_grabar.x				= li_posic_x
//	pb_grabar.y				= li_posic_y
//	pb_grabar.width		= 156
//	pb_grabar.height		= 133
//	li_visible ++
//	li_posic_y += 180
//END IF
//
//IF pb_imprimir.Visible THEN
//	pb_imprimir.x			= li_posic_x
//	pb_imprimir.y			= li_posic_y
//	pb_imprimir.width		= 156
//	pb_imprimir.height	= 133
//	li_visible ++
//	li_posic_y += 180
//END IF
//
//IF pb_salir.Visible THEN
//	pb_salir.x				= li_posic_x
//	pb_salir.y				= li_posic_y
//	pb_salir.width			= 156
//	pb_salir.height		= 133
//	li_visible ++
//	li_posic_y += 180
//END IF
//
//gb_1.height				= 180 * li_visible + 97 /*  (Según Botones Visibles)  */
//gb_2.x 					= gb_1.x
//gb_2.y 					= 1293
//gb_2.width				= 275
//gb_2.height				= 180 * 2 + 97 /*  (2 Botones)  */
//
//pb_ins_det.x			= li_posic_x
//pb_ins_det.y			= gb_2.y + 93
//pb_ins_det.width		= 156
//pb_ins_det.height		= 133
//
//pb_eli_det.x			= li_posic_x
//pb_eli_det.y			= pb_ins_det.y + 180
//pb_eli_det.width		= 156
//pb_eli_det.height		= 133
end event

event ue_antesguardar;Integer  ll_varcodigo, li_secuencia, i 

ll_varcodigo	=	dw_2.object.vari_codigo[1]

dw_2.setitem(1,"clie_codigo",	gi_CodExport)
dw_2.setitem(1,"espe_codigo", gi_codespecie)
dw_2.setitem(1,"prod_codigo", dw_3.object.prod_codigo[1])
dw_2.setitem(1,"etiq_codigo", dw_3.object.etiq_codigo[1])
dw_2.setitem(1,"vaca_calibr", dw_3.object.vaca_calibr[1])
dw_2.setitem(1,"ccde_fecemb", dw_3.object.ccde_fecemb[1])
dw_2.setitem(1,"emba_codigo", dw_3.object.emba_codigo[1])


//SELECT	IsNull(Max(ccdd_secuen), 0)
//	INTO	:li_Secuencia
//	FROM	dba.ctlcaldestinosdet
//	WHERE	clie_codigo	=	:gi_CodExport
//	AND	espe_codigo =	:gi_codespecie
//	AND	vari_codigo	=	:ll_varcodigo;

FOR  i=1 to dw_1.rowcount()
	li_secuencia ++
	dw_1.setitem(i,"ccdd_secuen", li_secuencia)
	dw_1.setitem(i,"clie_codigo", gi_codexport)
	dw_1.setitem(i,"espe_codigo", gi_codespecie)
	dw_1.setitem(i,"prod_codigo", dw_3.object.prod_codigo[1])
	dw_1.setitem(i,"etiq_codigo", dw_3.object.etiq_codigo[1])
	dw_1.setitem(i,"vaca_calibr", dw_3.object.vaca_calibr[1])
	dw_1.setitem(i,"ccde_fecemb", dw_3.object.ccde_fecemb[1])
	dw_1.setitem(i,"nave_tipotr", dw_2.object.nave_tipotr[1])
	dw_1.setitem(i,"nave_codigo", dw_2.object.nave_codigo[1])
	dw_1.setitem(i,"emba_codigo", dw_2.object.emba_codigo[1])
	dw_1.setitem(i,"vari_codigo", dw_2.object.vari_codigo[1])	
NEXT
end event

event ue_guardar();String	ls_Usuario
Integer	li_Grupo
ls_Usuario	=	Upper(Gstr_Us.Nombre)

	istr_mant.borra	= 	False
	istr_mant.agrega	= 	False
	li_Grupo = BuscaGrupo(ls_Usuario)
	IF ((li_Grupo =	6) OR (li_grupo = 1)) OR dw_1.GetItemStatus(il_Fila, 0, Primary!) = NewModified!  THEN 		
		istr_mant.Solo_Consulta = False
	END IF

	OpenWithParm(w_mant_ctlcaldestinosenc_resolucionlot, istr_mant)

call super::ue_guardar

end event

type dw_1 from w_mant_encab_deta_csd`dw_1 within w_maed_ctlcalplanilladestino
integer x = 27
integer y = 644
integer width = 2912
integer height = 1232
string title = "Detalle Planilla Cuantitativa de Inspección de Destino"
string dataobject = "dw_mues_ctlcaldestinodet"
boolean minbox = true
boolean hsplitscroll = true
end type

type dw_2 from w_mant_encab_deta_csd`dw_2 within w_maed_ctlcalplanilladestino
integer x = 119
integer y = 0
integer width = 2734
integer height = 436
string dataobject = "dw_maed_ctlcalplanilladestino"
end type

event dw_2::itemchanged;  String	ls_Columna, ls_Nula, ls_NroGuia

SetNull(ls_Nula)

ls_Columna	=	dwo.Name

CHOOSE CASE ls_Columna
		
	CASE "nave_tipotr"
		IF (data = "M") or (data = "m") or (data = "A") or(data = "a") or (data = "T") or (data = "t")THEN 
			istr_mant.argumento[15]=data
		ELSE	
	 	IF NoExisteTipoNave(data) = 1 THEN
			  This.SetItem(1,ls_columna , ls_nula)
			  RETURN 1
		 ELSE
 		  istr_mant.argumento[15]=data				
      END IF		
		END IF 	
		
	CASE "nave_codigo"
		IF noexistenavecodigo(integer(data)) = 1 THEN
			  This.SetItem(1,ls_columna ,integer( ls_nula))
			  RETURN 1
		ELSE
				istr_mant.argumento[1]=data
		END IF
		
	CASE "ccde_bodega"
		istr_mant.argumento[2]=data

	CASE "puer_codigo"
		IF noexistepuerto(integer(data)) = 1 THEN
			  This.SetItem(1,ls_columna ,integer( ls_nula))
			  RETURN 1
		ELSE
		istr_mant.argumento[3]=data
	END IF 
	
	CASE "ccde_lugins"
		istr_mant.argumento[4]=data

	CASE "ccde_fecarr"
		istr_mant.argumento[5]=data			

	CASE "ccde_fecins"
		istr_mant.argumento[6]=data	

	CASE "ccde_fecfum"	
		istr_mant.argumento[7]=data

	CASE "ccde_temmax"
		istr_mant.argumento[8]=data
		
	CASE "ccde_temmin"
		istr_mant.argumento[9]=data	

	CASE "ccde_temfre"
		istr_mant.argumento[10]=data
		
CASE "vari_codigo"
		  IF NoExisteVariedad(Integer(data)) THEN
			  This.SetItem(1, ls_Columna, Long(ls_nula))
			  RETURN 1
		  ELSE
			  istr_mant.argumento[11]=data
			  dw_3.GetChild("vaca_calibr", idwc_calibre)
			  idwc_calibre.SetTransObject(sqlca)
			  idwc_calibre.Retrieve(11,integer(data))									   
		     dw_3.SetItem(1, "vaca_calibr", ls_Nula)		
		  END IF							  				  
		 
	CASE "ccin_codigo"
			istr_mant.argumento[12]=data
	
	CASE "reci_codigo"	
		IF noexisterecibidor(Long(data)) = 1 THEN
			This.SetItem(1,ls_columna ,Long( ls_nula))
			RETURN 1
		ELSE
			istr_mant.argumento[13]=data
		END IF 
END CHOOSE

habilitaingreso(ls_Columna)
end event

type pb_nuevo from w_mant_encab_deta_csd`pb_nuevo within w_maed_ctlcalplanilladestino
integer x = 3072
integer y = 368
end type

type pb_eliminar from w_mant_encab_deta_csd`pb_eliminar within w_maed_ctlcalplanilladestino
integer x = 3072
integer y = 548
end type

type pb_grabar from w_mant_encab_deta_csd`pb_grabar within w_maed_ctlcalplanilladestino
integer x = 3072
integer y = 732
end type

type pb_imprimir from w_mant_encab_deta_csd`pb_imprimir within w_maed_ctlcalplanilladestino
integer x = 3072
integer y = 908
end type

type pb_salir from w_mant_encab_deta_csd`pb_salir within w_maed_ctlcalplanilladestino
integer x = 3072
integer y = 1088
end type

type pb_ins_det from w_mant_encab_deta_csd`pb_ins_det within w_maed_ctlcalplanilladestino
integer x = 3072
integer y = 1408
integer taborder = 90
end type

type pb_eli_det from w_mant_encab_deta_csd`pb_eli_det within w_maed_ctlcalplanilladestino
integer x = 3072
integer y = 1580
integer taborder = 100
end type

type pb_buscar from w_mant_encab_deta_csd`pb_buscar within w_maed_ctlcalplanilladestino
integer x = 3072
integer y = 188
end type

type dw_3 from datawindow within w_maed_ctlcalplanilladestino
integer x = 119
integer y = 420
integer width = 2734
integer height = 208
integer taborder = 80
boolean bringtotop = true
string dataobject = "dw_mues_ctlcaldestino"
borderstyle borderstyle = stylelowered!
end type

event itemchanged;String	ls_Columna, ls_Nula
Integer	li_cont,li_variedad
SetNull(ls_Nula)
ls_Columna	=	dwo.Name
li_variedad = dw_2.object.vari_codigo[1]

CHOOSE CASE ls_Columna	
	CASE "prod_codigo"
		IF noexisteproductor(Long(data))= 1 THEN
			This.SetItem(1, ls_Columna, Long(ls_Nula))
			RETURN 1
		ELSE
			istr_mant.argumento[14]=data						
		END IF 		
		
	CASE "etiq_codigo"
		IF noexisteetiqueta(Integer(data))= 1 THEN
			This.SetItem(1, ls_Columna, Integer(ls_Nula))
			RETURN 1
		ELSE
			istr_mant.argumento[16]=data						
		END IF 	
		
	CASE "emba_codigo"
		IF noexisteembalaje(data)= 1 THEN
			This.SetItem(1, ls_Columna, ls_Nula)
			RETURN 1
		ELSE
			istr_mant.argumento[17]=data						
		END IF 		
		
		CASE "vaca_calibr"
		IF noexistecalibre(data) THEN
			This.SetItem(1, ls_Columna, ls_Nula)
			RETURN 1
		ELSE
			istr_mant.argumento[23]=data
		END IF 			
	
	CASE "ccde_fecemb"							
		   istr_mant.argumento[18]=data
END CHOOSE

habilitaingreso(ls_Columna)

IF istr_mant.argumento[1] <> "" and istr_mant.argumento[11] <> ""  and istr_mant.argumento[15] <> "" and istr_mant.argumento[14] <> "" and istr_mant.argumento[17] <> "" and istr_mant.argumento[16] <> "" and istr_mant.argumento[18] <> "" THEN
	IF existeencabezado() then
		Parent.TriggerEvent("ue_recuperadatos")
	END IF 	
END IF 	
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

type dw_5 from uo_dw within w_maed_ctlcalplanilladestino
integer x = 101
integer y = 1944
integer width = 2912
integer height = 1232
integer taborder = 40
boolean bringtotop = true
boolean titlebar = true
string title = "Detalle Planilla Cuantitativa de Inspección de Destino"
string dataobject = "dw_mues_ctlcaldestinodet"
boolean minbox = true
boolean hscrollbar = true
boolean hsplitscroll = true
boolean livescroll = true
end type

