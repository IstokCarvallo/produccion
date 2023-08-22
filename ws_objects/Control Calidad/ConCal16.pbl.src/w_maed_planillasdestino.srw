$PBExportHeader$w_maed_planillasdestino.srw
$PBExportComments$Encabezado de Ingreso de Inspección en Destino USA
forward
global type w_maed_planillasdestino from w_mant_encab_deta_csd
end type
end forward

global type w_maed_planillasdestino from w_mant_encab_deta_csd
integer width = 3927
integer height = 2948
string title = "PLANILLA  DE INSPECCION EN DESTINO"
string menuname = ""
boolean controlmenu = false
boolean minbox = false
end type
global w_maed_planillasdestino w_maed_planillasdestino

type variables
DataWindowChild idwc_clientes,idwc_nave,idwc_recibidor,idwc_puerto,idwc_inspector

w_mant_deta_planillasdestinodet	iw_mantencion
end variables

forward prototypes
protected function integer wf_modifica ()
public function long maximoplanilla ()
public function boolean noexistecalibre (string as_calibre)
public function boolean noexisteespecie (integer especie)
public function boolean noexistegrupo (string data)
public function boolean noexisteinspector (integer inspector)
public function boolean noexistetecnico (integer tecnico)
public function boolean noexistevariedad (integer variedad)
public function boolean noexisteembalaje (string embalaje)
public subroutine existelote ()
public subroutine ingresodedatos ()
public subroutine habilitaencab (boolean habilita)
public subroutine habilitaingreso (string ls_columna)
protected function boolean wf_actualiza_db (boolean borrando)
public function boolean duplicado (string data)
public function boolean existenumerolote (string as_columna, string as_valor)
public subroutine limpia_data ()
public subroutine trae_planilla (long al_planilla, integer ai_cliente)
public function boolean noexistenave (integer ai_nave, string as_tiponave)
public function boolean noexisterecibidor (integer ai_cliente, integer ai_mercado, integer ai_recibidor)
public function boolean noexistepuerto (integer ai_puerto)
end prototypes

protected function integer wf_modifica ();IF dw_1.AcceptText() = -1 THEN RETURN -1
IF dw_2.AcceptText() = -1 THEN RETURN -1
IF (dw_1.ModifiedCount() + dw_1.DeletedCount()) > 0 THEN RETURN 0
IF dw_2.ModifiedCount() > 0 THEN RETURN 0


RETURN 1
end function

public function long maximoplanilla ();//Long		ll_Maximo
//
//SELECT	MAX(ccpe_numero)
//	INTO	:ll_Maximo
//	FROM	dba.ctlcalplacuaninspuvaenc;
//
//IF sqlca.sqlcode	=	-1	THEN
//	F_ErrorBaseDatos(sqlca,"No se pudo leer la tabla de PLanillas de Inspección") 
//ELSEIF ll_Maximo	>	0	THEN	
//	
//	ll_Maximo	=	ll_Maximo + 1	
//	RETURN ll_Maximo
//END IF

RETURN 0	
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

public function boolean noexisteespecie (integer especie);//Integer li_Contador
//
//SELECT	Count(*)
//	INTO	:li_Contador
//	FROM	dba.especies
//	WHERE espe_codigo = :especie;
//
//IF sqlca.sqlcode = -1 THEN
//	F_ErrorBaseDatos(sqlca,"No se pudo leer la tabla Especies")
//	RETURN TRUE
//ELSEIF li_Contador = 0 THEN
//	messagebox("Atención","Código Especie No Existe Para la Zona" + &
//					"~n~nSeleccionada, Ingrese Otro Por Favor",Exclamation!)
//	RETURN TRUE
//ELSE	
	RETURN FALSE	
//END IF
//
end function

public function boolean noexistegrupo (string data);//Integer li_Contador, li_Grupo
//
//li_Grupo = Integer(Data)
//
//SELECT	Count(*)
//	INTO	:li_Contador
//	FROM	dba.admagrupousuario
//	WHERE	grpo_codigo = : li_grupo;
//
//IF sqlca.sqlcode = -1 THEN
//	F_ErrorBaseDatos(sqlca,"No se pudo leer la tabla Grupousuario")
//	RETURN TRUE
//ELSEIF li_Contador = 0 THEN
//	RETURN TRUE
//ELSE	
//	istr_mant.argumento[1] = Data
//	TriggerEvent("ue_recuperadatos")	
RETURN FALSE	
//END IF
	

	

end function

public function boolean noexisteinspector (integer inspector);Integer li_Contador

SELECT	Count(*)
	INTO	:li_Contador
	FROM  dba.ctlcalinspectores
	WHERE ccin_codigo = :inspector ;

IF sqlca.sqlcode = -1 THEN
	F_ErrorBaseDatos(sqlca,"No se pudo leer la tabla Inspectores")
	RETURN TRUE
ELSEIF li_Contador = 0 THEN
	messagebox("Atención","Código Inspectores No Existe, " + &
					"~n~nIngrese Otro Por Favor",Exclamation!)
	RETURN TRUE
ELSE	
	RETURN FALSE	
END IF

end function

public function boolean noexistetecnico (integer tecnico);//Integer li_Contador, li_zona
//
//li_zona		=	dw_2.Object.zona_codigo[1]
//
//SELECT	Count(*)
//	INTO  :li_Contador
//	FROM  dba.ctlcaltecnicos
//	WHERE cctc_codigo = :tecnico
//	AND   zona_codigo = :li_zona;
//
//IF sqlca.sqlcode = -1 THEN
//	F_ErrorBaseDatos(sqlca,"No se pudo leer la tabla Tecnicos")
//	RETURN TRUE
//ELSEIF li_Contador = 0 THEN
//	messagebox("Atención","Código Tecnico No Existe Para la Zona" + &
//					"~n~nSeleccionada, Ingrese Otro Por Favor",Exclamation!)
//	RETURN TRUE
//ELSE	
	RETURN FALSE	
//END IF
//
end function

public function boolean noexistevariedad (integer variedad);//Integer li_Contador
//
//SELECT	Count(*)
//	INTO	:li_Contador
//	FROM	dba.variedades
//	WHERE vari_codigo = :variedad;
//
//IF sqlca.sqlcode = -1 THEN
//	F_ErrorBaseDatos(sqlca,"No se pudo leer la tabla Variedades")
//	RETURN TRUE
//ELSEIF li_Contador = 0 THEN
//	messagebox("Atención","Código Variedad No Existe Para la Zona" + &
//					"~n~nSeleccionada, Ingrese Otro Por Favor",Exclamation!)
//	RETURN TRUE
//ELSE	
	RETURN FALSE	
//END IF
//
end function

public function boolean noexisteembalaje (string embalaje);//Integer li_contador
//
//SELECT	Count(*)
//	INTO	:li_Contador
//	FROM	dba.embalajes
//	WHERE	emba_codigo = :embalaje;
//
//IF sqlca.sqlcode = -1 THEN
//	F_ErrorBaseDatos(sqlca,"No se pudo leer la tabla Embalajes")
//	RETURN TRUE
//ELSEIF li_Contador = 0 THEN
//	RETURN TRUE
//ELSE	
	RETURN FALSE	
//END IF
//	
//
end function

public subroutine existelote ();
end subroutine

public subroutine ingresodedatos ();//String	ls_Mensaje, ls_Columna[]
//Integer	li_Contador
//
//IF Isnull(dw_2.Object.plde_codigo[il_Fila]) OR dw_2.Object.plde_codigo[il_Fila] = 0 THEN
//	li_Contador	++
//	ls_Mensaje 			= ls_Mensaje + "~nNúmero de Planta"
//	ls_Columna[li_Contador]	= "plde_codigo"
//END IF
//
//IF Isnull(dw_2.Object.ccpe_numero[il_Fila]) OR dw_2.Object.ccpe_numero[il_Fila] = 0 THEN
//	li_Contador	++
//	ls_Mensaje 			= ls_Mensaje + "~nnúmero de Planilla"
//	ls_Columna[li_Contador]	= "ccpe_numero"
//END IF
//
//IF li_Contador > 0 THEN
//	MessageBox("Error de Consistencia", "Falta el Ingreso de :" + ls_Mensaje + ".", StopSign!, Ok!)
//	dw_2.SetColumn(ls_Columna[1])
//	dw_2.SetFocus()
//	Message.DoubleParm = -1
//END IF
end subroutine

public subroutine habilitaencab (boolean habilita);IF Habilita THEN
	
   dw_2.Object.cpde_numero.protect = 0            
   dw_2.Object.cpde_numero.BackGround.Color	=	RGB(255,255,255)
	dw_2.Object.clie_codigo.protect = 0             
	dw_2.Object.clie_codigo.BackGround.Color	=	RGB(255,255,255)
END IF

end subroutine

public subroutine habilitaingreso (string ls_columna);Boolean	lb_Estado = True

dw_2.AcceptText()
	
IF ls_Columna <> "clie_codigo" AND &
	(dw_2.Object.clie_codigo[1]) = 0 OR IsNull(dw_2.Object.clie_codigo[1]) THEN
	lb_Estado	=	False
END IF

IF ls_Columna <> "cpde_numero" AND &
	(dw_2.Object.cpde_numero[1]) = 0 OR IsNull(dw_2.Object.cpde_numero[1]) THEN
	lb_Estado	=	False
END IF

IF ls_Columna <> "nave_tipotr" AND &
	(dw_2.Object.nave_tipotr[1]) = '' OR IsNull(dw_2.Object.nave_tipotr[1]) THEN
	lb_Estado	=	False
END IF

IF ls_Columna <> "nave_codigo" AND &
	(dw_2.Object.nave_codigo[1]) = 0 OR IsNull(dw_2.Object.nave_codigo[1]) THEN
	lb_Estado	=	False
END IF

IF ls_Columna <> "merc_codigo" AND &
	(dw_2.Object.merc_codigo[1]) = 0 OR IsNull(dw_2.Object.merc_codigo[1]) THEN
	lb_Estado	=	False
END IF

IF ls_Columna <> "cpde_fecarr" AND &
	IsNull(dw_2.Object.cpde_fecarr[1]) THEN
	lb_Estado	=	False
END IF

IF ls_Columna <> "reci_codigo" AND &
	(dw_2.Object.reci_codigo[1]) = 0 OR IsNull(dw_2.Object.reci_codigo[1]) THEN
	lb_Estado	=	False
END IF

IF ls_Columna <> "puer_codigo" AND &
	(dw_2.Object.puer_codigo[1]) = 0 OR IsNull(dw_2.Object.puer_codigo[1]) THEN
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
			
				Commit;
				
				IF sqlca.SQLCode <> 0 THEN
					F_ErrorBaseDatos(sqlca, This.Title)
				ELSE
					lb_Retorno	=	True
					
					dw_1.ResetUpdate()
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
  
	
		IF dw_2.Update(True,False) =	1	THEN
			IF dw_1.Update(True, False) = 1 THEN
				Commit;
				
				IF sqlca.SQLCode <> 0 THEN
					F_ErrorBaseDatos(sqlca, This.Title)
				ELSE
					lb_Retorno	=	True
					
					dw_1.ResetUpdate()
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
		
	
END IF

sqlca.AutoCommit	=	lb_AutoCommit

RETURN lb_Retorno
RETURN true
end function

public function boolean duplicado (string data);//Integer li_grupo, Li_contador
//
//li_grupo = Integer(data)		
//
//SELECT	Count(*)
//	INTO	:li_Contador
//	FROM	dba.admagrupousuario
//	WHERE	grpo_codigo	=	:li_grupo;
//
//IF sqlca.sqlcode = -1 THEN
//	F_ErrorBaseDatos(sqlca,"No se pudo leer la tabla admagruposuario")
//ELSEIF li_Contador = 0 THEN
//	RETURN FALSE
//ELSE
	RETURN TRUE
//END IF
end function

public function boolean existenumerolote (string as_columna, string as_valor);//Integer	li_lote, li_planta, li_especie, li_variedad, li_packing, li_zona,&
//			li_NeoLote, li_TamLot, li_Contador, li_tipins, respuesta
//Long		ll_nfolio, ll_numero, ll_Fila, ll_noguia, ll_productor
//String	ls_Embalaje, ls_calibre, ls_Null
//Date		ld_Fecemb
//Boolean	lb_Retorno = False
//
//SetNull(ls_Null)
//
//li_Zona			=	dw_2.Object.zona_codigo[1]
//li_planta		=	dw_2.Object.plde_codigo[1]
//li_lote 			=	dw_2.Object.cclo_numero[1]
//ll_numero 		=	dw_2.Object.ccpe_numero[1]
//ll_productor	=	dw_2.Object.prod_codigo[1]
//li_especie		=	dw_2.Object.espe_codigo[1]
//li_variedad		=	dw_2.Object.vari_codigo[1]
//ls_embalaje		=	dw_3.Object.emba_codigo[1]
//li_packing		=	dw_3.Object.plde_codpak[1]
//ls_calibre		=	dw_3.Object.vaca_calibr[1]
//ld_fecemb		=	dw_3.Object.cclo_fecemb[1]
//ll_noguia		=	dw_2.Object.ccpe_noguia[1]
//li_tipins		=	dw_2.Object.ccti_codigo[1]
//
//CHOOSE CASE as_columna
//	
//	CASE "plde_codigo"
//		li_planta		=	Integer(as_valor)
//		
//	CASE "cclo_numero"
//		ll_nfolio 		=	Long(as_valor)
//
//	CASE "ccpe_numero"
//		ll_numero 		=	Long(as_valor)
//
//	CASE "prod_codigo"
//		ll_productor	=	Long(as_valor)
//		
//	CASE "espe_codigo"
//		li_especie		=	Integer(as_valor)
//		
//	CASE "vari_codigo"
//		li_variedad		=	Integer(as_valor)
//		
//	CASE "emba_codigo"
//		ls_embalaje		=	as_valor
//		
//	CASE "plde_codpak"
//		li_packing		=	Integer(as_valor)
//		
//	CASE "vaca_calibr"
//		ls_calibre		=	as_valor
//		
//	CASE "cclo_fecemb"
//		ld_fecemb		=	Date(as_valor)
//		
//			
//END CHOOSE
//
//IF NOT IsNull(li_Especie) AND NOT IsNull(li_Variedad) &
//	AND NOT IsNull(ls_Calibre) AND NOT IsNull(ll_Productor) &
//	AND NOT IsNull(li_Packing) AND NOT IsNull(ls_Embalaje) AND &
//	ld_FecEmb <> Date('01/01/1900') THEN
//
//
//	istr_mant.argumento[2]	=	String(ll_Numero)
//	istr_mant.argumento[3]	=	String(li_Zona)							
//	istr_mant.argumento[4]	=	String(li_Planta)
//	istr_mant.argumento[7]	=	String(gi_CodExport)
//	istr_mant.argumento[8]	=	String(ll_Productor)
//	istr_mant.argumento[9]	=	String(li_Especie)
//	istr_mant.argumento[10]	=	String(li_Variedad)
//	istr_mant.argumento[17]	=	ls_embalaje
//	istr_mant.argumento[18]	=	String(ld_FecEmb)	
//	istr_mant.argumento[20]	=	String(li_Packing)
//	istr_mant.argumento[21]	=	ls_Calibre
//	/* 
//	Gener Lote: 1	====>	SI
//	*/
//	IF gstr_parlote.codgen = 1 THEN	
//		IF li_tipins	=	1	THEN
//		
//			SELECT	cclo_numero
//				INTO	:li_lote
//				FROM	dba.ctlcallotes
//				WHERE	plde_codigo	=	:li_Planta
//				AND   prod_codigo =  :ll_Productor
//				AND   espe_codigo =  :li_Especie
//				AND   vari_codigo =  :li_Variedad
//				AND   emba_codigo =  :ls_Embalaje
//				AND   vaca_calibr =  :ls_Calibre
//				AND   plde_codpak =  :li_Packing
//				AND   cclo_fecemb =  :ld_FecEmb
//				AND   cclo_numpla	=  :ll_numero
//				And 	cclo_noguia =  :ll_noguia;
//		ELSE
//			SELECT	cclo_numero
//				INTO	:li_lote
//				FROM	dba.ctlcallotes
//				WHERE	plde_codigo	=	:li_Planta
//				AND   prod_codigo =  :ll_Productor
//				AND   espe_codigo =  :li_Especie
//				AND   vari_codigo =  :li_Variedad
//				AND   emba_codigo =  :ls_Embalaje
//				AND   vaca_calibr =  :ls_Calibre
//				AND   plde_codpak =  :li_Packing
//				AND   cclo_fecemb =  :ld_FecEmb
//				AND   cclo_numpla	=  :ll_numero;
//		END IF
//			
//		
//		IF Sqlca.SQLCode = -1 THEN	
//			F_errorbasedatos(sqlca,"Lectura tabla CTLCALLOTES")
//		ELSEIF li_Lote > 0 THEN		
//			
//			SELECT	Count(*)
//				INTO	:li_Contador
//				FROM	dba.ctlcalplacuaninspuvaenc
//				WHERE	plde_codigo =	:li_Planta
//				AND   cclo_numero = :li_Lote
//				AND   ccpe_numero = :ll_Numero;
//			
//			IF sqlca.SQLCode = -1 THEN
//				F_errorbasedatos(sqlca,"Lectura tabla CTLCALPLACUANINSPUVAENC")
//			ELSEIF li_Contador	>	0	THEN						
//				istr_mant.argumento[1]	=	String(li_Lote)
//				
//				This.TriggerEvent("ue_recuperadatos")			
//				//HabilitaEncab(False)			
//				lb_Retorno	=	True				
//			ELSE
//				IF dw_2.Object.ccti_codigo[1]	<>	3	THEN				
//	//				li_NeoLote		=	NuevoLote(gi_CodExport,li_Planta)
//	//				dw_2.SetItem(1,"cclo_numero",li_NeoLote)
//					dw_3.SetITem(1,"clie_codigo",gi_CodExport)
//					dw_3.SetItem(1,"plde_codigo",li_Planta)
//	//				dw_3.SetItem(1,"cclo_numero",li_NeoLote)
//					dw_3.SetITem(1,"prod_codigo",ll_Productor)
//					dw_3.SetITem(1,"espe_codigo",li_Especie)
//					dw_3.SetITem(1,"vari_codigo",li_Variedad)
//					dw_3.SetITem(1,"emba_codigo",ls_Embalaje)
//					dw_3.SetITem(1,"vaca_calibr",ls_Calibre)
//					dw_3.SetITem(1,"cclo_fecemb",ld_FecEmb)					
//					
//					istr_mant.argumento[1]	=	String(li_NeoLote)
//					
//					lb_Retorno	=	False
//				END IF			
//			END IF
//		ELSE
//	//		li_NeoLote	=	NuevoLote(gi_CodExport,li_Planta)				
//	//		istr_mant.Argumento[1]	=	String(li_NeoLote)
//			
//	//		dw_2.SetItem(1,"cclo_numero",li_NeoLote)
//			dw_3.SetITem(1,"clie_codigo",gi_CodExport)
//			dw_3.SetITem(1,"plde_codigo",li_Planta)
//	//		dw_3.SetItem(1,"cclo_numero",li_NeoLote)
//			dw_3.SetItem(1,"prod_codigo",ll_Productor)
//			dw_3.SetItem(1,"espe_codigo",li_Especie)
//			dw_3.SetItem(1,"vari_codigo",li_Variedad)
//			dw_3.SetItem(1,"emba_codigo",ls_Embalaje)
//			dw_3.SetItem(1,"vaca_calibr",ls_Calibre)
//			dw_3.SetItem(1,"cclo_fecemb",ld_FecEmb)
//			lb_Retorno	=	False
//		END IF
//	ELSE
//		SELECT	cclo_numero, cclo_tamlot
//			INTO	:li_lote, :li_Tamlot
//			FROM	dba.ctlcallotes
//			WHERE	plde_codigo	=	:li_Planta
//			AND   prod_codigo =  :ll_Productor
//			AND   espe_codigo =  :li_Especie
//			AND   vari_codigo =  :li_Variedad
//			AND   emba_codigo =  :ls_Embalaje
//			AND   vaca_calibr =  :ls_Calibre
//			AND   plde_codpak =  :li_Packing
//			AND   cclo_fecemb =  :ld_FecEmb;
//			
//			IF Sqlca.SQLCode = -1 THEN	
//				F_errorbasedatos(sqlca,"Lectura tabla CTLCALLOTES")
//			ELSEIF li_Lote > 0 THEN	
//				SELECT	Count(*)
//					INTO	:li_Contador
//					FROM	dba.ctlcalplacuaninspuvaenc
//					WHERE	plde_codigo =	:li_Planta
//					AND   cclo_numero = :li_Lote
//					AND   ccpe_numero = :ll_Numero;
//			
//				IF sqlca.SQLCode = -1 THEN
//					F_errorbasedatos(sqlca,"Lectura tabla CTLCALPLACUANINSPUVAENC")
//				ELSEIF li_Contador	>	0	THEN						
//					istr_mant.argumento[1]	=	String(li_Lote)
//					
//					This.TriggerEvent("ue_recuperadatos")			
//							
//					lb_Retorno	=	True
//				ELSE
//					ll_fila	= dw_3.Retrieve(gi_CodExport,li_Planta,li_Lote)
//		
//					IF ll_fila = -1 THEN
//						respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
//														Information!, RetryCancel!)
//						lb_Retorno	=	False
//					ELSE
//						dw_1.Reset()
//						lb_Retorno	=	True
//					END IF
//				END IF
//			END IF
//		END IF
//END IF
//RETURN lb_Retorno	
RETURN true
end function

public subroutine limpia_data ();//String ls_Nula
//Setnull(ls_Nula)
//
//dw_3.SetItem(1, "emba_codigo", ls_Nula)
//dw_3.SetItem(1, "plde_codpak", Integer (ls_Nula))
//dw_3.SetItem(1, "cclo_fecemb", Integer (ls_Nula))
//dw_3.SetItem(1, "vaca_calibr", ls_Nula)
//dw_3.SetItem(1, "cclo_tamlot", Integer (ls_Nula))
end subroutine

public subroutine trae_planilla (long al_planilla, integer ai_cliente);Long 		ll_planilla

SELECT cpde_numero
  INTO :ll_planilla
  FROM dba.ctlcalplandestinosenc
 WHERE cpde_numero =	:al_planilla
   AND clie_codigo = :ai_cliente; 

IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla CTLCALPLANDESTINOSENC")
ELSEIF sqlca.sqlcode	=	0 THEN
		
	dw_2.retrieve(ai_cliente,ll_planilla)
	
   dw_2.GetChild("nave_codigo", idwc_nave)
   idwc_nave.SetTransObject(sqlca)
   idwc_nave.Retrieve(dw_2.Object.nave_tipotr[1]) 
	idwc_nave.InsertRow(0)
   
   dw_2.GetChild("reci_codigo", idwc_recibidor)
   idwc_recibidor.SetTransObject(sqlca)
   idwc_recibidor.Retrieve(dw_2.Object.reci_codigo[1])
	idwc_recibidor.InsertRow(0)
   
   dw_2.GetChild("puer_codigo", idwc_puerto)
   idwc_puerto.SetTransObject(sqlca)
   idwc_puerto.Retrieve()
   idwc_puerto.InsertRow(0)
  
   dw_2.GetChild("cpde_inspec", idwc_inspector)
   idwc_inspector.SetTransObject(sqlca)
   idwc_inspector.Retrieve()
	idwc_inspector.InsertRow(0)
	
	dw_1.Retrieve(ai_cliente,ll_planilla)
	
   pb_ins_det.Enabled	=	TRUE
   pb_eli_det.Enabled	=	TRUE
	pb_grabar.Enabled    =  TRUE
	pb_eliminar.Enabled    =  TRUE
	
END IF
end subroutine

public function boolean noexistenave (integer ai_nave, string as_tiponave);Integer li_Contador 

SELECT	Count(*)
	INTO 	:li_Contador
	FROM 	dba.naves
	WHERE nave_codigo = :ai_nave
	AND   nave_tipotr = :as_tiponave;

IF sqlca.sqlcode = -1 THEN
	F_ErrorBaseDatos(sqlca,"No se pudo leer la tabla Naves")
	RETURN TRUE
ELSEIF li_Contador = 0 THEN
	messagebox("Atención","Código Nave No Existe Para Tipo de Nave" + &
					"~n~nSeleccionada, Ingrese Otro Por Favor",Exclamation!)
	RETURN TRUE
ELSE	
	RETURN FALSE	
END IF

end function

public function boolean noexisterecibidor (integer ai_cliente, integer ai_mercado, integer ai_recibidor);Integer li_Contador 

SELECT	Count(*)
	INTO 	:li_Contador
	FROM 	dba.recibidores
	WHERE merc_codigo = :ai_mercado
	AND   reci_codigo = :ai_recibidor;

IF sqlca.sqlcode = -1 THEN
	F_ErrorBaseDatos(sqlca,"No se pudo leer la tabla Recibidores")
	RETURN TRUE
ELSEIF li_Contador = 0 THEN
	messagebox("Atención","Código Recibidor No Existe Para El Mercado y Cliente" + &
					"~n~nSeleccionados, Ingrese Otro Por Favor",Exclamation!)
	RETURN TRUE
ELSE	
	RETURN FALSE	
END IF

end function

public function boolean noexistepuerto (integer ai_puerto);Integer li_Contador

SELECT	Count(*)
	INTO	:li_Contador
	FROM  dba.puertos
	WHERE puer_codigo = :ai_puerto ;

IF sqlca.sqlcode = -1 THEN
	F_ErrorBaseDatos(sqlca,"No se pudo leer la tabla Puertos")
	RETURN TRUE
ELSEIF li_Contador = 0 THEN
	messagebox("Atención","Código Puerto No Existe, " + &
					"~n~nIngrese Otro Por Favor",Exclamation!)
	RETURN TRUE
ELSE	
	RETURN FALSE	
END IF

end function

on w_maed_planillasdestino.create
call super::create
end on

on w_maed_planillasdestino.destroy
call super::destroy
end on

event ue_seleccion;call super::ue_seleccion;String	ls_nula

SetNull(ls_nula)

istr_busq.argum[1]	=	String(gi_CodExport)

OpenWithParm(w_busc_ctlcalplanilladestino, istr_busq)
istr_busq = Message.PowerObjectParm

IF istr_busq.argum[1] <> '' THEN

istr_mant.argumento[6] 	= istr_busq.argum[1]
istr_mant.argumento[1] 	= istr_busq.argum[2]
		
This.TriggerEvent("ue_recuperadatos")

ELSE
	pb_buscar.SetFocus()
END IF
	

	
	
end event

event ue_recuperadatos;long ll_fila_e, ll_fila_d, ll_fila_f, respuesta


DO
	dw_2.SetRedraw(False)
	dw_2.Reset()
	
	ll_fila_e	= dw_2.Retrieve(Integer(istr_mant.argumento[6]),Integer(Istr_mant.argumento[1]))

								
	dw_2.Object.cpde_numero.protect = 1            
   dw_2.Object.cpde_numero.BackGround.Color	=	RGB(192,192,192)
	dw_2.Object.clie_codigo.protect = 1             
	dw_2.Object.clie_codigo.BackGround.Color	=	RGB(192,192,192)

	
	
	IF ll_fila_e = -1 THEN
		respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
										Information!, RetryCancel!)
	ELSE
		DO
		
			ll_fila_d	= dw_1.Retrieve(Integer(istr_mant.argumento[6]),Integer(Istr_mant.argumento[1]))

			IF ll_fila_d = -1 THEN
				respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
												Information!, RetryCancel!)
												
			ELSE
								
			   pb_eliminar.Enabled	= TRUE
				pb_grabar.Enabled		= TRUE
				pb_imprimir.Enabled	= TRUE
				pb_ins_det.Enabled	= TRUE
				
				IF ll_fila_d > 0 THEN
				   pb_eli_det.Enabled	= True
					dw_1.SetRow(1)
					dw_1.SelectRow(1,True)
					dw_1.SetFocus()
					HabilitaEncab(False)
				ELSE
					pb_ins_det.SetFocus()
				END IF
			END IF
		LOOP WHILE respuesta = 1

		IF respuesta = 2 THEN Close(This)
	END IF
	dw_2.SetRedraw(True)
LOOP WHILE respuesta = 1

IF respuesta = 2 THEN Close(This)

//dw_2.Enabled	=	False
end event

event ue_nuevo;Long		ll_modif1, ll_modif2

ib_ok	= True

istr_mant.Solo_Consulta	=	istr_mant.UsuarioSoloConsulta

IF Not istr_mant.Solo_Consulta THEN
	CHOOSE CASE wf_modifica()
		CASE -1
			ib_ok = False
		CASE 0
			ll_modif1	=	dw_1.GetNextModified(0, Primary!)
			ll_modif2	=	dw_2.GetNextModified(0, Primary!)
			
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

dw_2.SetRedraw(False)
dw_2.Reset()
dw_2.InsertRow(0)
dw_2.SetRedraw(True)
dw_2.Setitem(1,"clie_codigo",81)
dw_2.SetFocus()
dw_2.SetColumn("cpde_numero")

HabilitaEncab(True)

end event

event ue_nuevo_detalle;call super::ue_nuevo_detalle;istr_mant.borra			= False
istr_mant.agrega			= True

OpenWithParm(iw_mantencion, istr_mant)

IF dw_1.RowCount() > 0 THEN
	pb_grabar.Enabled		= True
END IF
pb_eli_det.Enabled	= True

dw_1.SetRow(il_fila)
dw_1.SelectRow(il_fila,True)




end event

event open;/*Argumentos
istr_mant.argumento[1] = planilla
istr_mant.argumento[2] = nave
istr_mant.argumento[3] = recibidor
istr_mant.argumento[4] = inspector
istr_mant.argumento[5] = puerto
istr_mant.argumento[6] = cliente
istr_mant.argumento[7] = dw_1.Retrieve > 0 => 1 ; 0
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

dw_1.Modify("datawindow.message.title='Error '+ is_titulo")
dw_1.SetRowFocusIndicator(Hand!)

istr_mant.dw						=	dw_1
istr_Mant.dw2						=	dw_2

istr_mant.UsuarioSoloConsulta	=	OpcionSoloConsulta()

//pb_nuevo.PostEvent(Clicked!)

GrabaAccesoAplicacion(True, id_FechaAcceso, it_HoraAcceso, &
							This.Title, "Acceso a Aplicación", 1)

buscar	= "Código:Ncodigo,Descripción:Sconcepto"
ordenar	= "Código:codigo,Descripción:concepto"

dw_2.GetChild("clie_codigo", idwc_clientes)
idwc_clientes.SetTransObject(sqlca)
IF idwc_clientes.Retrieve() = 0 THEN
	idwc_clientes.InsertRow(0)
END IF
dw_2.SetItem(1,"clie_codigo",gi_CodExport)

dw_2.GetChild("nave_codigo", idwc_nave)
idwc_nave.SetTransObject(sqlca)
IF idwc_nave.Retrieve('*') = 0 THEN
	idwc_nave.InsertRow(0)
END IF

dw_2.GetChild("reci_codigo", idwc_recibidor)
idwc_recibidor.SetTransObject(sqlca)
IF idwc_recibidor.Retrieve(0)= 0 THEN
	idwc_recibidor.InsertRow(0)
END IF

dw_2.GetChild("puer_codigo", idwc_puerto)
idwc_puerto.SetTransObject(sqlca)
IF idwc_puerto.Retrieve()= 0 THEN
	idwc_puerto.InsertRow(0)
END IF

dw_2.GetChild("cpde_inspec", idwc_inspector)
idwc_inspector.SetTransObject(sqlca)
IF idwc_inspector.Retrieve()= 0 THEN
	idwc_inspector.InsertRow(0)
END IF

istr_mant.argumento[1] = '0'
istr_mant.argumento[2] = ''
istr_mant.argumento[3] = '0'
istr_mant.argumento[4] = '0'
istr_mant.argumento[5] = '0'
istr_mant.argumento[6] = String(gi_codexport)
istr_mant.argumento[7]	=	'0'
end event

event ue_modifica_detalle;IF dw_1.RowCount()>0 THEN
	istr_mant.Agrega = False
	istr_mant.Borra  = False	
	OpenWithParm(iw_mantencion,istr_mant)
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
 IF dw_1.RowCount() = 0 THEN pb_eli_det.Enabled = False
END IF

istr_mant.borra	 = False
end event

event ue_imprimir;SetPointer(HourGlass!)

Long		fila
DataWindowChild  dwc_packing

istr_info.titulo	=	"INFORME DE PLANILLAS DESTINO"
istr_info.copias	=	1

OpenWithParm(vinf,istr_info)

vinf.dw_1.DataObject	=	"dw_info_planilladestino"

vinf.dw_1.SetTransObject(sqlca)


fila	=	vinf.dw_1.Retrieve(Integer(istr_mant.argumento[6]), Integer(istr_mant.argumento[1]))

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

event ue_antesguardar;Integer  li_secuencia, li_cliente
Long		ll_fila, ll_numero

ll_numero		=	dw_2.Object.cpde_numero[1]
li_cliente		=	dw_2.Object.clie_codigo[1]


SELECT	IsNull(Max(cpdd_secuen), 0)
	INTO	:li_Secuencia
	FROM	dba.ctlcalplandestinosdet
	WHERE	cpde_numero	=	:ll_Numero 
	AND	clie_codigo =	:li_cliente;
	

FOR ll_Fila = 1 TO dw_1.RowCount()
	IF dw_1.GetItemStatus(ll_Fila, 0, Primary!) = NewModified! THEN
		li_Secuencia ++
		dw_1.Object.cpde_numero[ll_Fila] =	ll_Numero
		dw_1.Object.clie_codigo[ll_Fila] =	li_cliente
		dw_1.Object.cpdd_secuen[ll_Fila] =	li_Secuencia
	END IF
NEXT
end event

type dw_1 from w_mant_encab_deta_csd`dw_1 within w_maed_planillasdestino
integer x = 32
integer y = 728
integer width = 3154
integer height = 1172
string title = "Detalle Planilla  de Inspección en Destino"
string dataobject = "dw_mant_mues_planillasdestinodet"
boolean hscrollbar = false
boolean hsplitscroll = true
end type

type dw_2 from w_mant_encab_deta_csd`dw_2 within w_maed_planillasdestino
integer x = 46
integer y = 36
integer width = 2830
integer height = 592
string dataobject = "dw_mant_mues_planillasdestinoenc"
end type

event dw_2::itemchanged;String	ls_Columna, ls_Nula, ls_NroGuia


SetNull(ls_Nula)



ls_Columna	=	dwo.Name

CHOOSE CASE ls_Columna
		
	CASE "clie_codigo"
		   istr_mant.argumento[6] = data
	
	CASE "cpde_numero"
			Trae_Planilla(Long(data),dw_2.Object.clie_codigo[1])
			istr_mant.argumento[1] = data
	CASE "nave_tipotr"
		   dw_2.GetChild("nave_codigo", idwc_nave)
         idwc_nave.SetTransObject(sqlca)
         idwc_nave.Retrieve(data)
			dw_2.SetItem(1, "nave_codigo", Integer(ls_nula))
								
	CASE "nave_codigo"
		  IF IsNull(dw_2.Object.nave_tipotr[1]) OR dw_2.Object.nave_tipotr[1] = '' THEN
			  Messagebox("Atención","Debe Seleccionar Tipo de Nave",exclamation!)
			  This.SetItem(1, ls_Columna, Integer(ls_nula))
			  return 1
		  ELSEIF NoExisteNave(Integer(data),dw_2.Object.nave_tipotr[1]) THEN
			  This.SetItem(1, ls_Columna, Integer(ls_nula))
			  RETURN 1
		  ELSE
			  istr_mant.argumento[2] = data
		  END IF
		  
	CASE "merc_codigo"
		   dw_2.GetChild("reci_codigo", idwc_recibidor)
         idwc_recibidor.SetTransObject(sqlca)
         idwc_recibidor.Retrieve(dw_2.Object.Clie_codigo[1],Integer(data))

	CASE "reci_codigo"
		  IF IsNull(dw_2.Object.merc_codigo[1]) OR dw_2.Object.merc_codigo[1] = 0 THEN
			  Messagebox("Atención","Debe Seleccionar Mercado",exclamation!)
			  This.SetItem(1, ls_Columna, Integer(ls_nula))
			  return 1
		  ELSEIF NoExisteRecibidor(dw_2.Object.Clie_codigo[1],dw_2.Object.Merc_codigo[1],Integer(data)) THEN
			  This.SetItem(1, ls_Columna, Integer(ls_nula))
			  RETURN 1
		  ELSE
		     istr_mant.argumento[3] = data 
	     END IF
		  
	CASE "cpde_inspec"
		  IF NoExisteInspector(Integer(data)) THEN
			  This.SetItem(1, ls_Columna, Long(ls_nula))
			  RETURN 1
		  ELSE
		     istr_mant.argumento[4] = data
			  dw_2.GetChild("cpde_inspec", idwc_inspector)
           idwc_inspector.SetTransObject(sqlca)
           idwc_inspector.Retrieve(0)
	     END IF							  

	CASE "puer_codigo"
		  IF NoExistePuerto(Integer(data)) THEN
			  This.SetItem(1, ls_Columna, Long(ls_nula))
			  RETURN 1
		  ELSE
			  istr_mant.argumento[5] = data
			  dw_2.GetChild("puer_codigo", idwc_puerto)
           idwc_puerto.SetTransObject(sqlca)
           idwc_puerto.Retrieve(0)
	     END IF
	
END CHOOSE

habilitaingreso(ls_Columna)
end event

type pb_nuevo from w_mant_encab_deta_csd`pb_nuevo within w_maed_planillasdestino
integer x = 3310
integer y = 412
end type

type pb_eliminar from w_mant_encab_deta_csd`pb_eliminar within w_maed_planillasdestino
integer x = 3310
integer y = 592
end type

type pb_grabar from w_mant_encab_deta_csd`pb_grabar within w_maed_planillasdestino
integer x = 3310
integer y = 776
end type

type pb_imprimir from w_mant_encab_deta_csd`pb_imprimir within w_maed_planillasdestino
integer x = 3310
integer y = 952
end type

type pb_salir from w_mant_encab_deta_csd`pb_salir within w_maed_planillasdestino
integer x = 3310
integer y = 1132
end type

type pb_ins_det from w_mant_encab_deta_csd`pb_ins_det within w_maed_planillasdestino
integer x = 3314
integer y = 1424
integer taborder = 100
end type

type pb_eli_det from w_mant_encab_deta_csd`pb_eli_det within w_maed_planillasdestino
integer x = 3314
integer y = 1596
integer taborder = 110
end type

type pb_buscar from w_mant_encab_deta_csd`pb_buscar within w_maed_planillasdestino
integer x = 3310
integer y = 232
end type

