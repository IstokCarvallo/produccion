$PBExportHeader$w_maed_ctlcaltercerainspepecenc.srw
$PBExportComments$Encabezado de Ingreso de Inspección de Lotes Objetados
forward
global type w_maed_ctlcaltercerainspepecenc from w_mant_encab_deta_csd
end type
type dw_3 from uo_dw within w_maed_ctlcaltercerainspepecenc
end type
end forward

global type w_maed_ctlcaltercerainspepecenc from w_mant_encab_deta_csd
integer width = 4306
integer height = 2016
string title = "Lotes Objetados"
string menuname = ""
boolean minbox = false
dw_3 dw_3
end type
global w_maed_ctlcaltercerainspepecenc w_maed_ctlcaltercerainspepecenc

type variables
DataWindowChild		idwc_clientes, idwc_zonas,   idwc_plantas,  idwc_agronomos, idwc_inspectores, &
                		idwc_calibres, idwc_especie, idwc_embalaje, idwc_especies,  idwc_destino1, &
							idwc_destino2, idwc_productor

uo_ctlcaldanoespecie	iuo_ctlcaldanoespecie
uo_ctrolusuario 		iuo_ctrlusuario
uo_DestinoRechazado	iuo_Destino1
uo_DestinoRechazado	iuo_Destino2
uo_especie     		iuo_especie

Integer 					ii_var
end variables

forward prototypes
protected function integer wf_modifica ()
public function integer tipoins (integer ai_tipo)
public function boolean noexistezona (integer zona)
public function boolean noexisteplanta (integer planta)
public function boolean noexisteembalaje (string embalaje)
public function boolean noexistenumeroplanilla (string as_columna, string as_valor)
public function boolean duplicado (string as_columna, string as_valor)
public function boolean existefolio (string as_columna, string as_valor)
public function boolean existeinspec (integer ai_valor)
public subroutine habilitaingreso (string columna)
public function boolean noexisteagronomo (integer ai_agronomo)
public function boolean noexistegrupo (string data)
public subroutine califresol (string as_calemb, string as_calcal, string as_calcon)
public function boolean existecalibre (string as_valor)
public function boolean noexistenumerolote (string as_columna, string as_valor)
public function long buscahistorialote (integer ai_planta, integer ai_lote, integer ai_tipoinsp, long al_folio)
public subroutine habilitaencab (boolean habilita)
public subroutine captura_usuario ()
public subroutine elimina_columna (string as_columna, string as_valor)
public subroutine borrafila ()
public function boolean resolucion ()
public subroutine avisa_fecha ()
public subroutine inserta ()
public function integer validaestadolote (long al_lote, integer ai_planta, long al_planilla)
public function boolean valida_reso (string as_columna, string as_valor, string as_valcol)
public function boolean existeproductor (long ai_codigo)
end prototypes

protected function integer wf_modifica ();IF dw_1.AcceptText() = -1 THEN RETURN -1
IF dw_2.AcceptText() = -1 THEN RETURN -1
IF (dw_1.ModifiedCount() + dw_1.DeletedCount()) > 0 THEN RETURN 0
IF dw_2.ModifiedCount() > 0 THEN RETURN 0
RETURN 1
end function

public function integer tipoins (integer ai_tipo);IF (ai_tipo <> 11) OR (ai_tipo <> 12) OR (ai_tipo <> 15) THEN 
	RETURN 1 
END IF 	








end function

public function boolean noexistezona (integer zona);Integer li_Contador

SELECT	Count(*)
	INTO 	:li_Contador
	FROM 	dbo.zonas
	WHERE	zona_codigo = :zona;

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

public function boolean noexisteplanta (integer planta);Integer li_Contador, li_cliente,li_planta

SELECT 	Count(*)
	INTO 	:li_Contador
	FROM 	dbo.plantadesp
	WHERE 	plde_codigo = :planta
	AND		plde_tipopl	=	1;

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

public function boolean noexisteembalaje (string embalaje);Integer li_Contador, li_cliente

li_cliente	=	gi_CodExport

SELECT	Count(*)
	INTO 	:li_Contador
	FROM 	dbo.embalajesprod
	WHERE clie_codigo = :li_cliente
	AND	emba_codigo = :embalaje;

IF sqlca.sqlcode = -1 THEN
	F_ErrorBaseDatos(sqlca,"No se pudo leer la tabla Embalajes")
	RETURN TRUE
ELSEIF li_Contador = 0 THEN
	RETURN TRUE
ELSE	
	RETURN FALSE	
END IF
	

end function

public function boolean noexistenumeroplanilla (string as_columna, string as_valor);Long		ll_Folio, ll_lote
Integer	li_TipoInsp, li_Zona, li_Planta, li_Existe
Boolean	lb_Retorno

li_TipoInsp	=	dw_1.Object.cctd_tipins[il_Fila]
ll_Folio		=	dw_1.Object.cctd_folpla[il_Fila]
li_Zona		=	dw_2.Object.zona_codigo[1]
li_Planta	=	dw_2.Object.plde_codigo[1]

CHOOSE CASE as_Columna
	CASE "cctd_tipins"
		li_TipoInsp	=	Integer(as_Valor)
		
	CASE "cctd_folpla"
		ll_Folio		=	Long(as_Valor)
		
END CHOOSE

IF Not IsNull(li_TipoInsp) AND Not IsNull(ll_Folio) THEN
	SELECT	Count(*),cclo_numero
		INTO	:li_Existe,:ll_lote 
		FROM	dbo.ctlcalhistobjetados
		WHERE	clie_codigo	=	:gi_codexport
		AND	plde_codigo	=	:li_Planta
		AND	ccho_tipins	=	:li_TipoInsp
		AND	ccho_folpla	=	:ll_Folio 
		GROUP BY cclo_numero;
	
	IF sqlca.sqlcode	=	-1	THEN
		F_ErrorBaseDatos(sqlca, "No se pudo leer la tabla de verificación")
	ELSEIF li_Existe = 0 THEN	
		MessageBox("Atención", "Planilla no se encuentra en Lotes Objetados Pendientes." + &
						"~n~nIngrese o seleccione otra Planilla.")
		
		lb_Retorno	=	True
	ELSE
		dw_1.SetItem(il_fila,"cclo_numero",ll_lote)
		lb_Retorno	=	False
	END IF
END IF

RETURN lb_Retorno			
end function

public function boolean duplicado (string as_columna, string as_valor);Long		ll_Fila,li_lote

CHOOSE CASE as_columna		
	CASE "cclo_numero"		
			li_lote = Integer(as_valor)
			
END CHOOSE	

ll_Fila	= dw_1.Find("cclo_numero =" + as_valor , 1, dw_1.RowCount())

IF ll_Fila > 0 AND ll_Fila <> il_Fila THEN
	MessageBox("Error","Número de Lote ya fue ingresado anteriormente",Information!, Ok!)
	RETURN True
ELSE
	RETURN False
END IF
end function

public function boolean existefolio (string as_columna, string as_valor);Integer	li_planta, li_existe, li_cliente
Long		ll_nfolio, ll_numero,ll_lote

li_planta	=	dw_2.Object.plde_codigo[1]
ll_nfolio 	=	dw_2.Object.ccte_numero[1]

CHOOSE CASE as_columna
	CASE "ccte_numero"
		ll_nfolio 	=	Long(as_valor)		
		
END CHOOSE

SELECT	Count(*)
	INTO 	:ll_lote
	FROM  dbo.ctlcalotesobjetadosenc 
	WHERE ccte_numero = :ll_nfolio
	AND	plde_codigo = :li_planta ;

IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla CTLCALOTESOBJETADOSENC")
	RETURN True
ELSEIF ll_lote > 0 THEN
	istr_mant.argumento[1]	=	String(ll_nfolio)
	istr_mant.argumento[3]	=	String(li_planta)	
	This.TriggerEvent("ue_recuperadatos")				
	RETURN False
ELSE
	MessageBox("Atención","Nº Planilla No Existe, Ingrese Otro",exclamation!)
	
	istr_mant.argumento[3]	=	String(li_planta)
	
	RETURN True
END IF

end function

public function boolean existeinspec (integer ai_valor);Integer embala1, embala2, embala3, var1


embala1	=	dw_1.Object.cctd_embal1[il_fila]
embala2	=	dw_1.Object.cctd_embal2[il_fila]
embala3	=	dw_1.Object.cctd_embal3[il_fila]

SELECT	Count(*)
	INTO  : var1
	FROM 	dbo.ctlcalotesobjetadosdet
	WHERE	(embala1 < ai_valor) 
	OR (embala2 < ai_valor) 
	OR (embala3 < ai_valor);
	
	
RETURN False
end function

public subroutine habilitaingreso (string columna);Boolean	lb_estado = True

IF columna <> "espe_codigo" AND (dw_2.Object.espe_codigo[1] = 0 OR IsNull(dw_2.Object.espe_codigo[1])) THEN
	lb_estado = False
END IF

IF columna <> "zona_codigo" AND (dw_2.Object.zona_codigo[1] = 0 OR IsNull(dw_2.Object.zona_codigo[1])) THEN
	lb_estado = False
END IF

IF columna <> "plde_codigo" AND (dw_2.Object.plde_codigo[1] = 0 OR IsNull(dw_2.Object.plde_codigo[1])) THEN
	lb_estado = False
END IF

//IF columna <> "ccag_codigo" AND (dw_2.Object.ccag_codigo[1] = 0 OR IsNull(dw_2.Object.ccag_codigo[1])) THEN
//	lb_estado = False
//END IF

IF lb_estado AND dw_1.RowCount() > 0 THEN
	pb_ins_det.Enabled = lb_estado
	pb_eli_det.Enabled = lb_estado
END IF
end subroutine

public function boolean noexisteagronomo (integer ai_agronomo);Integer li_Contador, li_Zona

li_Zona		=	dw_2.Object.zona_codigo[1]

SELECT	Count(*)
	INTO	:li_Contador
	FROM	dbo.ctlcalagronomos
	WHERE	zona_codigo =	:li_Zona
	AND	ccag_codigo =	:ai_Agronomo ;

IF sqlca.sqlcode = -1 THEN
	F_ErrorBaseDatos(sqlca,"No se pudo leer la tabla Tecnicos")
	RETURN True
ELSEIF li_Contador = 0 THEN
	MessageBox("Atención","Código de Agronomo No Existe Para la Zona" + &
					"~nSeleccionada.~n~nIngrese o seleccione otro.",Exclamation!)
	RETURN True
ELSE
	istr_Mant.Argumento[5]	=	String(ai_Agronomo)
	RETURN False
END IF

end function

public function boolean noexistegrupo (string data);Integer li_Contador, li_Grupo

li_grupo = Integer(Data)

SELECT	Count(*)
	INTO 	:li_Contador
	FROM 	dbo.admagrupousuario
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

public subroutine califresol (string as_calemb, string as_calcal, string as_calcon);IF (as_calemb = "B") AND (as_calcal = "B") AND (as_calcon = "B") THEN 
	dw_1.Setitem(il_fila,"cctd_otrare","A")	
END IF 		

IF (as_calemb = "M") OR (as_calcal = "M") OR (as_calcon = "M") THEN 
	dw_1.Setitem(il_fila,"cctd_otrare", "O")
END IF 	


end subroutine

public function boolean existecalibre (string as_valor);Long		 ll_numero
Integer	li_existe,li_especie, li_Estado,li_variedad, li_Planta
String 	ls_calibre

ls_calibre 	=	as_valor + fill(' ',3 - len(as_valor))
ll_Numero	=	dw_1.Object.cclo_numero[il_Fila]
li_Planta	=	dw_2.Object.plde_codigo[1]

	SELECT	espe_codigo,vari_codigo
		INTO	:li_especie, :li_variedad
		FROM	dbo.ctlcallotes
		WHERE	cclo_numero	=	:ll_numero
		AND   plde_codigo 		= 	:li_Planta;
				
IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla CTLCALLOTES")
	RETURN True
ELSEIF SqlCa.SqlCode	=	0	THEN
	
	SELECT	count(*)
		INTO	:li_Existe
		FROM	dbo.variecalibre
		WHERE	espe_codigo	=	:li_especie
		AND   vari_codigo = 	:li_variedad
		AND   vaca_calibr =  :ls_calibre;
					
	IF sqlca.SQLCode = -1 THEN
		F_errorbasedatos(sqlca,"Lectura tabla VARIECALIBRE")
		RETURN True
	ELSEIF li_Existe > 0 THEN
		RETURN False
	ELSE
		MessageBox("Atención","Calibre para esta Variedad no Existe ," + &
		   		"~n~nIngrese Otro,Por Favor",Exclamation!) 
		RETURN True
	END IF
	RETURN True
END IF


end function

public function boolean noexistenumerolote (string as_columna, string as_valor);Long		ll_Folio,ll_Respuesta,ll_NroLote
Integer	li_TipoInsp, li_Causal, li_Planta, &
			li_Estado, li_Especie, li_Variedad, li_null, li_Existe
Boolean	lb_Retorno

Setnull(li_null)

li_Planta	=	dw_2.Object.plde_codigo[1]
ll_NroLote 	=	dw_1.Object.cclo_numero[il_Fila]
li_TipoInsp	=	dw_1.Object.cctd_tipins[il_Fila]
ll_Folio		=	dw_1.Object.cctd_folpla[il_Fila]

Choose Case as_Columna
	Case "cclo_numero"
		ll_NroLote 	=	Long(as_Valor)	
		
	Case "cctd_tipins"
		li_TipoInsp =	Integer(as_Valor)
		
	Case "cctd_folpla"
		ll_Folio 	=	Long(as_Valor)

End Choose

If Not IsNull(ll_NroLote) And Not IsNull(li_TipoInsp) And Not IsNull(ll_Folio) Then
	
	SELECT 	espe_codigo, vari_codigo
	  INTO	:li_Especie, :li_Variedad
	  FROM	dbo.ctlcallotes
	 WHERE	cclo_numero	=	:ll_NroLote
		And   plde_codigo = 	:li_Planta;
		
		If sqlca.SQLCode = -1 Then
			F_ErrorBaseDatos(sqlca, "Lectura tabla CTLCALLOTES")
			lb_Retorno	=	True
			
		ElseIf sqlca.SQLCode =	100 Then
				MessageBox("Atención","Lote no Existe en Planta")
				lb_Retorno = True
				
		ElseIf sqlca.SQLCode =	0 Then
			ll_Respuesta		=	ValidaEstadoLote(ll_NroLote,li_Planta, ll_Folio)		
			If ll_Respuesta	<>	0	Then
				MessageBox("Atención","Lote ya fue Aprobado ~r " + &
			              "para la Planilla Nº" + + String(ll_Folio))
				lb_Retorno = True		
		   Else				
				SELECT	ccda_secuen 		
					INTO	:li_Causal
					FROM	dbo.ctlcalhistobjetados
					WHERE	clie_codigo	=	:gi_CodExport
					And   plde_codigo 	=  :li_Planta
					And	cclo_numero	=	:ll_NroLote
					And	ccho_tipins		=	:li_TipoInsp
					And   ccho_folpla		=	:ll_Folio ;
				
				If sqlca.SQLCode = -1 Then
					F_errorbasedatos(sqlca,"Lectura tabla CTLCALHISTOBJETADOS")
					lb_Retorno	=	True
					
				ElseIf sqlca.Sqlcode	=	0	Then
					dw_1.SetItem(il_Fila, "plde_codigo", li_Planta)
					dw_1.SetItem(il_Fila, "ccda_secuen", li_Causal)				
					istr_mant.Argumento[9]		=	String(li_Especie)
					istr_Mant.Argumento[10]	=	String(li_Variedad)
				ElseIf sqlca.SQLCode =	100 Then
					 MessageBox("Atención","Lote no Corresponde al Nº Folio")
					 lb_Retorno = True	
				End If
		End If	
	End If
	istr_mant.Argumento[9]		=	String(li_Especie)
	istr_Mant.Argumento[10]	=	String(li_Variedad)
End If

Return lb_Retorno
end function

public function long buscahistorialote (integer ai_planta, integer ai_lote, integer ai_tipoinsp, long al_folio);Long	ll_contador

SELECT	Count(*)
	INTO	:ll_Contador
	FROM	dbo.ctlcalhistobjetados
	WHERE clie_codigo=:gi_CodExport
	AND   plde_codigo=:ai_Planta
	AND   cclo_numero=:ai_Lote
	AND 	ccho_folpla=:al_Folio
	AND   ccho_tipins=:ai_TipoInsp;

IF sqlca.sqlcode	=	-1	THEN
	F_errorBaseDatos(sqlca,"No se pudo leer el Histórico de Lotes")
END IF
RETURN ll_Contador

end function

public subroutine habilitaencab (boolean habilita);IF Habilita THEN
	dw_2.Object.espe_codigo.Protect           		=  0
	dw_2.Object.ccte_numero.Protect				=	0
//	dw_2.Object.ccag_codigo.Protect					=	0
	dw_2.Object.plde_codigo.Protect					=	0
	dw_2.Object.ccte_fecins.Protect					=	0
	dw_2.Object.zona_codigo.Protect				=	0
	dw_2.Object.espe_codigo.BackGround.Color  	=  Rgb(255,255,255)
	dw_2.Object.ccte_numero.BackGround.Color	=	Rgb(255,255,255)
//	dw_2.Object.ccag_codigo.BackGround.Color	=	Rgb(255,255,255)
	dw_2.Object.plde_codigo.BackGround.Color	=	Rgb(255,255,255)
	dw_2.Object.ccte_fecins.BackGround.Color	=	Rgb(255,255,255)
	dw_2.Object.zona_codigo.BackGround.Color	=	Rgb(255,255,255)
ELSE
	dw_2.Object.espe_codigo.Protect           		=  1
	dw_2.Object.ccte_numero.Protect				=	1
//	dw_2.Object.ccag_codigo.Protect					=	1
	dw_2.Object.plde_codigo.Protect					=	1
	dw_2.Object.ccte_fecins.Protect					=	1
	dw_2.Object.zona_codigo.Protect				=	1
	dw_2.Object.espe_codigo.BackGround.Color  	=  553648127
	dw_2.Object.ccte_numero.BackGround.Color	=	553648127
//	dw_2.Object.ccag_codigo.BackGround.Color	=	553648127
	dw_2.Object.plde_codigo.BackGround.Color	=	553648127
	dw_2.Object.ccte_fecins.BackGround.Color	=	553648127
	dw_2.Object.zona_codigo.BackGround.Color	=	553648127
END IF

end subroutine

public subroutine captura_usuario ();dw_2.setitem(1,"usua_codigo",gstr_us.nombre)
dw_2.Setitem(1,"apac_fechaa",today())
dw_2.Setitem(1,"apac_horaac",now())
dw_2.Setitem(1,"comp_nombre",gstr_us.computador)
end subroutine

public subroutine elimina_columna (string as_columna, string as_valor);Integer	li_null, li_v3, li_v6, li_v7, li_v8, li_v9, li_v10, li_v11
String ls_v1, ls_v2, ls_v4, ls_v5
SetNull(li_null)

ls_v1		=	dw_1.Object.cctd_reclas[il_fila]
ls_v2		=	dw_1.Object.cctd_reclae[il_fila]
li_v3		=	dw_1.Object.cctd_devpro[il_fila]
ls_v4		=	dw_1.Object.cctd_otrare[il_fila]
ls_v5		=	dw_1.Object.cctd_resolu[il_fila]
li_v6		=	dw_1.Object.cctd_embal1[il_fila]
li_v7		=	dw_1.Object.cctd_embal2[il_fila]
li_v8		=	dw_1.Object.cctd_calid1[il_fila]
li_v9		=	dw_1.Object.cctd_calid2[il_fila]
li_v10		=	dw_1.Object.cctd_condi1[il_fila]
li_v11		=	dw_1.Object.cctd_condi2[il_fila]

Choose Case as_Columna
	Case "cctd_reclas"
		If ls_v1 = "" And ls_v2 = "" And li_v3 = 0 And ls_v4 ="" And ls_v5 = "" And li_v6 = 0 &
		And li_v7 = 0 And li_v8 = 0 And li_v9 = 0 And li_v10 = 0 And li_v11 = 0 Then
		ls_v1	=	as_valor
		//If li_v1 = 0 Then
			dw_1.SetItem(il_fila,"ccho_tipins",li_null)
			dw_1.SetItem(il_fila,"ccho_folpla",li_null)		
			dw_1.SetItem(il_fila,"cclo_numero",li_null)		
			dw_1.SetItem(il_fila,"ccda_secuen",li_null)		
		End If

	Case "cctd_reclae"
		If ls_v1 = "" And ls_v2 = "" And li_v3 = 0 And ls_v4 ="" And ls_v5 = "" And li_v6 = 0 &
		And li_v7 = 0 And li_v8 = 0 And li_v9 = 0 And li_v10 = 0 And li_v11 = 0 Then
		ls_v2	=	as_valor
		//If li_v2 = 0 Then
			dw_1.SetItem(il_fila,"ccho_tipins",li_null)
			dw_1.SetItem(il_fila,"ccho_folpla",li_null)		
			dw_1.SetItem(il_fila,"cclo_numero",li_null)		
			dw_1.SetItem(il_fila,"ccda_secuen",li_null)		
		End If

	Case "cctd_devpro"
		If ls_v1 = "" And ls_v2 = "" And li_v3 = 0 And ls_v4 ="" And ls_v5 = "" And li_v6 = 0 &
		And li_v7 = 0 And li_v8 = 0 And li_v9 = 0 And li_v10 = 0 And li_v11 = 0 Then
		li_v3	=	Integer(as_valor)
		//If li_v3 = 0 Then
			dw_1.SetItem(il_fila,"ccho_tipins",li_null)
			dw_1.SetItem(il_fila,"ccho_folpla",li_null)		
			dw_1.SetItem(il_fila,"cclo_numero",li_null)		
			dw_1.SetItem(il_fila,"ccda_secuen",li_null)			
		End If

	Case "cctd_otrare"
		If ls_v1 = "" And ls_v2 = "" And li_v3 = 0 And ls_v4 ="" And ls_v5 = "" And li_v6 = 0 &
		And li_v7 = 0 And li_v8 = 0 And li_v9 = 0 And li_v10 = 0 And li_v11 = 0 Then
		ls_v4 =	as_valor
		//If li_v4 = 0 Then
			dw_1.SetItem(il_fila,"ccho_tipins",li_null)
			dw_1.SetItem(il_fila,"ccho_folpla",li_null)		
			dw_1.SetItem(il_fila,"cclo_numero",li_null)		
			dw_1.SetItem(il_fila,"ccda_secuen",li_null)			
		End If
		
		Case "cctd_resolu"
			If ls_v1 = "" And ls_v2 = "" And li_v3 = 0 And ls_v4 ="" And ls_v5 = "" And li_v6 = 0 &
		And li_v7 = 0 And li_v8 = 0 And li_v9 = 0 And li_v10 = 0 And li_v11 = 0 Then
		ls_v5 =	as_valor
		//If li_v5 = 0 Then
			dw_1.SetItem(il_fila,"ccho_tipins",li_null)
			dw_1.SetItem(il_fila,"ccho_folpla",li_null)		
			dw_1.SetItem(il_fila,"cclo_numero",li_null)		
			dw_1.SetItem(il_fila,"ccda_secuen",li_null)			
		End If
		
		Case "cctd_embal1"
			If ls_v1 = "" And ls_v2 = "" And li_v3 = 0 And ls_v4 ="" And ls_v5 = "" And li_v6 = 0 &
		And li_v7 = 0 And li_v8 = 0 And li_v9 = 0 And li_v10 = 0 And li_v11 = 0 Then
		li_v6 =	Integer(as_valor)
		//If li_v6 = 0 Then
			dw_1.SetItem(il_fila,"ccho_tipins",li_null)
			dw_1.SetItem(il_fila,"ccho_folpla",li_null)		
			dw_1.SetItem(il_fila,"cclo_numero",li_null)		
			dw_1.SetItem(il_fila,"ccda_secuen",li_null)			
		End If
		
		Case "cctd_embal2"
			If ls_v1 = "" And ls_v2 = "" And li_v3 = 0 And ls_v4 ="" And ls_v5 = "" And li_v6 = 0 &
		And li_v7 = 0 And li_v8 = 0 And li_v9 = 0 And li_v10 = 0 And li_v11 = 0 Then
		li_v7 =	Integer(as_valor)
		//If li_v7 = 0 Then
			dw_1.SetItem(il_fila,"ccho_tipins",li_null)
			dw_1.SetItem(il_fila,"ccho_folpla",li_null)		
			dw_1.SetItem(il_fila,"cclo_numero",li_null)		
			dw_1.SetItem(il_fila,"ccda_secuen",li_null)			
		End If
		
		Case "cctd_calid1"
			If ls_v1 = "" And ls_v2 = "" And li_v3 = 0 And ls_v4 ="" And ls_v5 = "" And li_v6 = 0 &
		And li_v7 = 0 And li_v8 = 0 And li_v9 = 0 And li_v10 = 0 And li_v11 = 0 Then
		li_v8 =	Integer(as_valor)
		//If li_v8 = 0 Then
			dw_1.SetItem(il_fila,"ccho_tipins",li_null)
			dw_1.SetItem(il_fila,"ccho_folpla",li_null)		
			dw_1.SetItem(il_fila,"cclo_numero",li_null)		
			dw_1.SetItem(il_fila,"ccda_secuen",li_null)			
		End If
		
		Case "cctd_calid2"
			If ls_v1 = "" And ls_v2 = "" And li_v3 = 0 And ls_v4 ="" And ls_v5 = "" And li_v6 = 0 &
		And li_v7 = 0 And li_v8 = 0 And li_v9 = 0 And li_v10 = 0 And li_v11 = 0 Then
		li_v9 =	Integer(as_valor)
		//If li_v9 = 0 Then
			dw_1.SetItem(il_fila,"ccho_tipins",li_null)
			dw_1.SetItem(il_fila,"ccho_folpla",li_null)		
			dw_1.SetItem(il_fila,"cclo_numero",li_null)		
			dw_1.SetItem(il_fila,"ccda_secuen",li_null)			
		End If
		
		Case "cctd_condi1"
			If ls_v1 = "" And ls_v2 = "" And li_v3 = 0 And ls_v4 ="" And ls_v5 = "" And li_v6 = 0 &
		And li_v7 = 0 And li_v8 = 0 And li_v9 = 0 And li_v10 = 0 And li_v11 = 0 Then
		li_v10 =	Integer(as_valor)
		//If li_v10 = 0 Then
			dw_1.SetItem(il_fila,"ccho_tipins",li_null)
			dw_1.SetItem(il_fila,"ccho_folpla",li_null)		
			dw_1.SetItem(il_fila,"cclo_numero",li_null)		
			dw_1.SetItem(il_fila,"ccda_secuen",li_null)			
		End If
		
		Case "cctd_condi2"
			If ls_v1 = "" And ls_v2 = "" And li_v3 = 0 And ls_v4 ="" And ls_v5 = "" And li_v6 = 0 &
		And li_v7 = 0 And li_v8 = 0 And li_v9 = 0 And li_v10 = 0 And li_v11 = 0 Then
		li_v11 =	Integer(as_valor)
		//If li_v11 = 0 Then
			dw_1.SetItem(il_fila,"ccho_tipins",li_null)
			dw_1.SetItem(il_fila,"ccho_folpla",li_null)		
			dw_1.SetItem(il_fila,"cclo_numero",li_null)		
			dw_1.SetItem(il_fila,"ccda_secuen",li_null)			
		End If
		
End Choose
end subroutine

public subroutine borrafila ();Long	ll_fila = 1

DO WHILE ll_fila <= dw_1.RowCount()
	
	IF dw_1.GetItemStatus(ll_fila, 0, Primary!) = NewModified! AND &
		IsNull(dw_1.Object.cctd_resolu[ll_fila]) THEN
		dw_1.DeleteRow(ll_fila)
	ELSE
		ll_fila ++
	END IF
LOOP
end subroutine

public function boolean resolucion ();String reclas, reclae, otrare
Integer repesa, devpro

dw_1.Object.cctd_reclas[il_fila]	= reclas
dw_1.Object.cctd_reclae[il_fila]	= reclae
dw_1.Object.cctd_repesa[il_fila]	= repesa
dw_1.Object.cctd_devpro[il_fila]	= devpro
dw_1.Object.cctd_otrare[il_fila]	= otrare

IF reclas = "" AND reclae = "" AND (Isnull(repesa) OR repesa = 0) AND &
(Isnull(devpro) OR devpro = 0) AND otrare = "" THEN
	Messagebox("Atención","Debe ingresar algún destino del producto rechazado")
	dw_1.SetColumn("cctd_reclas")
	Return False
ELSE
	Return True
END IF

Return True





end function

public subroutine avisa_fecha ();dw_3.SetFilter("ccho_fecins < Date('" + String(relativedate(today(), -4)) + "')")
dw_3.filter()

IF dw_3.RowCount() > 0 THEN 
	Messagebox("ALERTA!!!","Tiene " + String(dw_3.RowCount()) + "  lotes sin levantar~r" + & 
	            "con mas de 4 dias de antiguedad")
END IF
dw_3.SetFilter("")
dw_3.filter()

end subroutine

public subroutine inserta ();Long ll_fila, ll_NroFila, ll_productor
Integer li_zona, li_planta,li_dia,li_especie
dwItemStatus l_status

li_especie  	= dw_2.Object.espe_codigo[1]
li_zona 		= dw_2.Object.zona_codigo[1]
li_planta		= dw_2.Object.plde_codigo[1]

IF dw_2.Object.prod_ptodos[1] = 1 THEN
	ll_productor = -1
ELSE	
	ll_productor = dw_2.Object.prod_codigo[1]
END IF

dw_3.retrieve(li_planta,li_zona,li_especie,ll_productor)

IF dw_2.GetItemStatus(1, 0, Primary!) = NewModified!  THEN
	FOR ll_NroFila	=	1 TO dw_3.RowCount()
		ll_Fila		=	dw_1.InsertRow(0)
		dw_1.SetItem(ll_Fila, "cctd_tipins", dw_3.Object.ccho_tipins[ll_NroFila])
		dw_1.SetItem(ll_Fila, "cctd_folpla", dw_3.Object.ccho_folpla[ll_NroFila])
		dw_1.SetItem(ll_Fila, "cclo_numero", dw_3.Object.cclo_numero[ll_NroFila])
		dw_1.SetItem(ll_Fila, "ccda_secuen", dw_3.Object.ccda_secuen[ll_NroFila])	
		
		dw_1.SetItem(ll_Fila, "paen_numero", dw_3.Object.cclo_npalle[ll_NroFila])	
		dw_1.SetItem(ll_Fila, "cctd_embal1", dw_3.Object.cclo_causal1[ll_NroFila])	
		dw_1.SetItem(ll_Fila, "cctd_calid1", dw_3.Object.cclo_causal2[ll_NroFila])	
		dw_1.SetItem(ll_Fila, "cctd_condi1", dw_3.Object.cclo_causal3[ll_NroFila])	
		dw_1.SetItem(ll_Fila, "prod_codigo", dw_3.Object.prod_codigo[ll_NroFila])		/*LRBB 07.mar.2014 agrega codigo de productor*/
		dw_1.SetItem(ll_Fila, "espe_codigo", dw_3.Object.espe_codigo[ll_NroFila])		/*10.sep.2014 agrega codigo de especie*/
		
		
      	dw_1.SetSort("cclo_numero asc")
		dw_1.Sort()
	   l_status	= dw_1.GetItemStatus(ll_fila, 0, Primary!)
	NEXT
END IF

end subroutine

public function integer validaestadolote (long al_lote, integer ai_planta, long al_planilla);Long	ll_Contador

SELECT	Count(*)
	INTO	:ll_Contador
	FROM	dbo.ctlcallotes
	WHERE	cclo_numero=:al_Lote
	AND   plde_codigo=:ai_planta
	AND   cclo_numpla=:al_planilla
	AND   cclo_estado=1;

If sqlca.sqlcode =	-1	Then F_ErrorBaseDatos(sqlca,"No se pudo leer la tabla Lotes en función ValidaEstadoLote")

Return ll_Contador
end function

public function boolean valida_reso (string as_columna, string as_valor, string as_valcol);Long		ll_Fila
String	ls_resolucion

ls_resolucion	=	String(dw_1.Object.cctd_resolu[il_Fila])

CHOOSE CASE as_Columna
	
	CASE "cctd_resolu"
		ls_resolucion		=	as_Valor
	
END CHOOSE

ll_fila	= dw_1.Find("cctd_resolu = '" + ls_resolucion + "'", 1 , dw_1.RowCount())

IF isNull(as_valcol) THEN ii_var ++

IF ll_fila = 0 AND ii_var > 1 THEN
	MessageBox("Atención","Resolución debe ser la misma para todos los lotes.",Information!, Ok!)
	ii_var --
	RETURN True
ELSE
	RETURN False
END IF
end function

public function boolean existeproductor (long ai_codigo);Integer li_Contador


SELECT	Count(*)
	INTO 	:li_Contador
	FROM 	dbo.productores
	WHERE	prod_codigo = : ai_codigo;

IF sqlca.sqlcode = -1 THEN
	F_ErrorBaseDatos(sqlca,"No se pudo leer la tabla productores")
	RETURN TRUE
ELSEIF li_Contador = 0 THEN
		MessageBox("Atención","Productor No Existe, Ingrese Otro",exclamation!)
	RETURN TRUE
ELSE	
	
	RETURN FALSE	
END IF
	

	

end function

on w_maed_ctlcaltercerainspepecenc.create
int iCurrent
call super::create
this.dw_3=create dw_3
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_3
end on

on w_maed_ctlcaltercerainspepecenc.destroy
call super::destroy
destroy(this.dw_3)
end on

event ue_seleccion;istr_busq.argum[1]	=	istr_mant.Argumento[4]//cliente
istr_busq.argum[2]	=	String(dw_2.Object.plde_codigo[1])//istr_Mant.Argumento[3]//planta
istr_busq.argum[3]	=	''
istr_busq.argum[7]   =  istr_Mant.Argumento[7]//Especie

/*se agrega zona planta y fecha inspec. como filtro 
para nuevo requerimiento*/
//istr_busq.argum[6]   =  istr_mant.Argumento[2]//zona
//istr_busq.argum[7]   =  istr_mant.Argumento[6]//fecha
/**/

OpenWithParm(w_busc_ctlcaltercerinspeccion, istr_busq)

istr_busq = Message.PowerObjectParm

IF istr_busq.argum[3] <> "" THEN
	istr_mant.argumento[1] = istr_busq.argum[1]
	istr_mant.argumento[2] = istr_busq.argum[2]
	istr_mant.argumento[3] = istr_busq.argum[3]
	This.TriggerEvent("ue_recuperadatos")
ELSE
	pb_buscar.SetFocus()
END IF
end event

event ue_recuperadatos;Long 		ll_fila_e, ll_fila_d, ll_fila_f, respuesta

DO
	dw_2.SetRedraw(False)
	dw_2.Reset()
	
	ll_fila_e	=	dw_2.Retrieve(Integer(istr_mant.argumento[1]), Integer(istr_mant.argumento[3]))
	
	IF ll_fila_e = -1 THEN
		respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", Information!, RetryCancel!)
	ELSE
		
		idwc_agronomos.SetTransObject(sqlca)			
	  	idwc_agronomos.Retrieve(dw_2.Object.zona_codigo[1])	
		DO			
   		ll_fila_d	= dw_1.Retrieve(Integer(istr_mant.argumento[1]), Integer(istr_mant.argumento[3]))

			IF ll_fila_d = -1 THEN				
				respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", Information!, RetryCancel!)
			ElseIf ll_fila_d > 0 Then
				pb_grabar.Enabled	= True
				pb_ins_det.Enabled	= True
				pb_eli_det.Enabled	= True
				pb_imprimir.Enabled	= True
				pb_eliminar.Enabled	= True
				dw_1.SetRow(1)
				dw_1.SelectRow(1,False)
				dw_1.SetFocus()
				iuo_Especie.Codigo = dw_2.Object.espe_codigo[1]
				dw_2.object.prod_ptodos.protect = 1		/*LRBB 07.mar.2014 desbloqueo de todos los productores*/
				dw_2.object.prod_codigo.protect = 1		/*LRBB 07.mar.2014 desbloqueo de los productores*/
			Else
				dw_2.Enabled 				=	False
				dw_1.Enabled				=	False				
				istr_mant.Solo_Consulta =	True 						
			END IF			
		LOOP WHILE respuesta = 1
		
		HabilitaEncab(False)

		IF respuesta = 2 THEN Close(This)
	END IF
	dw_2.SetRedraw(True)
LOOP WHILE respuesta = 1

IF respuesta = 2 THEN Close(This)		
end event

event ue_nuevo;Long		ll_modif1, ll_modif2, ll_modif3
Integer	li_Grupo

ib_ok	= True

istr_busq.argum[1]		=	""
istr_busq.argum[2]		=	""
istr_Mant.Argumento[3]	=	""
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

HabilitaEncab(True)

pb_eliminar.Enabled	=	False
pb_eli_det.Enabled	=	False
pb_ins_det.Enabled	=	False
pb_grabar.Enabled		=	False
pb_imprimir.Enabled	=	False
dw_2.Enabled			=	True
dw_2.object.prod_ptodos.protect = 0				//LRBB 07.mar.2014 bloqueo de todos los productores cuando trae informacion*/
dw_2.object.prod_codigo.protect = 0				//LRBB 07.mar.2014 bloqueo de los productores cuando trae informacion*/
dw_2.Object.prod_codigo.BackGround.Color  =  (553648127)

dw_2.SetRedraw(False)
dw_2.Reset()
dw_2.InsertRow(0)
dw_2.SetRedraw(True)

li_Grupo = BuscaGrupo(Upper(Gstr_Us.Nombre))

If (li_Grupo > 2) Then
	pb_eliminar.Visible	=	False
	TriggerEvent('resize')
End If

dw_2.Object.prod_ptodos[1] = 1
dw_2.Object.prod_codigo.Protect				=	1
dw_2.Object.prod_codigo.BackGround.Color  =  (553648127)
//dw_2.Object.espe_codigo.Protect				=	1
//dw_2.Object.espe_codigo.BackGround.Color  =  (553648127)

iuo_Especie.Codigo = gi_CodEspecie
dw_2.SetColumn("espe_codigo")
//dw_2.SetItem(1, "espe_codigo",gi_CodEspecie)
dw_2.SetItem(1, "clie_codigo",gi_codexport)
dw_2.SetItem(1, "zona_codigo",gi_CodZona)
dw_2.SetItem(1, "plde_codigo",gi_CodPlanta)
dw_1.SetRedraw(False)
//dw_1.Reset()
Inserta()
avisa_fecha()
dw_1.SetRedraw(True)
end event

event ue_nuevo_detalle;call super::ue_nuevo_detalle;String	ls_Calificacion, ls_CalCalidad, ls_CalCondicion, ls_CalEmbalaje, &
			ls_Mensaje,ls_Colu[]
Integer	li_Cont

IF il_fila > 0 THEN
	pb_eliminar.Enabled	= True
	pb_grabar.Enabled		= True
END IF

IF dw_1.RowCount()	>	0	THEN	
	
	IF dw_1.Object.cctd_resolu[il_fila] = 'R' THEN 
		
		IF IsNull(dw_1.Object.ccda_secuen[il_fila]) OR &
			dw_1.Object.ccda_secuen[il_fila] = 0 THEN 
			MessageBox("Atención","Falta Ingresar Causal de Objeción",Exclamation!)
						dw_1.SetColumn("ccda_secuen")
		END IF
		
		IF IsNull(dw_1.Object.cctd_embal1[il_fila]) OR &
			dw_1.Object.cctd_embal1[il_fila] = 0 THEN 	
	
			IF IsNull(dw_1.Object.cctd_calid1[il_fila]) &
				OR dw_1.Object.cctd_calid1[il_fila] = 0 THEN 
	
				IF IsNull(dw_1.Object.cctd_condi1[il_fila]) OR &
					dw_1.Object.cctd_condi1[il_fila] = 0 THEN 
						MessageBox("Atención","Falta Ingresar Causal de Resolución",Exclamation!)
						dw_1.SetColumn("cctd_embal1")
				END IF
			END IF
		END IF
	ELSEIF dw_1.Object.cctd_resolu[il_fila] = 'A' THEN 
		
		IF IsNull(dw_1.Object.ccda_secuen[il_fila]) OR &
			dw_1.Object.ccda_secuen[il_fila] = 0 THEN 
			MessageBox("Atención","Falta Ingresar Causal de Objeción",Exclamation!)
						dw_1.SetColumn("ccda_secuen")
		END IF
		
		IF IsNull(dw_1.Object.cctd_embal2[il_fila]) OR &
			dw_1.Object.cctd_embal1[il_fila] = 0 THEN 	
	
			IF IsNull(dw_1.Object.cctd_calid2[il_fila]) &
				OR dw_1.Object.cctd_calid1[il_fila] = 0 THEN 
	
				IF IsNull(dw_1.Object.cctd_condi2[il_fila]) OR &
					dw_1.Object.cctd_condi1[il_fila] = 0 THEN 
						MessageBox("Atención","Falta Ingresar Causal de Resolución",Exclamation!)
						dw_1.SetColumn("cctd_embal1")
				END IF
			END IF
		END IF
	END IF
	
	IF IsNull(dw_1.Object.cctd_folpla[il_fila]) OR &
			dw_1.Object.cctd_folpla[il_fila] = 0 THEN 
				MessageBox("Atención","Falta Ingresar Folio Planilla",Exclamation!)
				dw_1.SetColumn("cctd_folpla")		
	ELSE	
		il_fila = dw_1.InsertRow(0)
		dw_1.Setfocus()
		dw_1.ScrollToRow(il_fila)
		dw_1.SetRow(il_fila)
		dw_1.SetColumn("cctd_folpla")
		dw_1.SetItem(il_fila,"cctd_tipins",11)
		
		IF dw_1.RowCount() > 0 THEN
			dw_1.Object.cctd_secuen[il_fila] = dw_1.RowCount()
		ELSE
			dw_1.Object.cctd_secuen[il_fila] = 1
		END IF	
	END IF
ELSE
	IF dw_1.RowCount()	=	0	THEN il_fila = dw_1.InsertRow(0)	
	
	dw_1.Setfocus()
	dw_1.ScrollToRow(il_fila)
	dw_1.SetRow(il_fila)
	dw_1.SetColumn("cctd_folpla")
	dw_1.SetItem(il_fila,"cctd_tipins",11)
	
//	IF dw_1.RowCount() > 0 THEN
//		dw_1.Object.cctd_secuen[il_fila] = dw_1.RowCount()
//	ELSE
//		dw_1.Object.cctd_secuen[il_fila] = 1
//	END IF	
		
END IF
dw_2.SetItem(1, "zona_codigo",Integer(istr_mant.argumento[2]))
end event

event open;Long	ll_null

SetNull(ll_null)
/*Argumentos Utilizados 

Argumento[1]	=	Folio
Argumento[2]	=	Zona
Argumento[3]	=	Planta
Argumento[4]	=  Cliente
Argumento[5]	=	Agrónomo
Argumento[6]	=	Fecha de Inspección
Argumento[7]   =  Especie
*/

x				= 0
y				= 0

This.Height	= 2470
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
dw_1.Modify("DataWindow.Footer.Height = 110")

istr_mant.dw						=	dw_1
istr_mant.UsuarioSoloConsulta	=	OpcionSoloConsulta()

pb_nuevo.PostEvent(Clicked!)

GrabaAccesoAplicacion(True, id_FechaAcceso, it_HoraAcceso, This.Title, "Acceso a Aplicación", 1)

dw_2.GetChild("espe_codigo", idwc_especies)
idwc_especies.SetTransObject(sqlca)
idwc_especies.Retrieve()
idwc_especies.SetFilter("espe_codigo <> 11")
idwc_especies.Filter( )
idwc_especies.SetSort("espe_nombre A")
idwc_especies.Sort()
dw_2.SetItem(1, "espe_codigo",ll_null)

dw_2.GetChild("plde_codigo", idwc_plantas)
idwc_plantas.SetTransObject(sqlca)
idwc_plantas.Retrieve(1)
idwc_plantas.SetSort("plde_nombre A")
idwc_plantas.Sort()
dw_2.SetItem(1, "plde_codigo",gi_codPlanta)

dw_2.GetChild("zona_codigo", idwc_zonas)
idwc_zonas.SetTransObject(sqlca)
idwc_zonas.Retrieve()
idwc_zonas.SetSort("zona_nombre A")
idwc_zonas.Sort()
dw_2.SetItem(1, "zona_codigo",gi_codZona)

dw_2.GetChild("prod_codigo", idwc_productor)
idwc_productor.SetTransObject(sqlca)
idwc_productor.Retrieve(-1)
idwc_productor.Sort()
dw_2.SetItem(1, "prod_codigo",ll_null)

dw_2.Object.prod_ptodos[1] = 1
dw_2.Object.prod_codigo.Protect				=	0
dw_2.Object.prod_codigo.BackGround.Color  	=  Rgb(255,255,255)

dw_1.GetChild("cctd_reclae", idwc_embalaje)
idwc_embalaje.SetTransObject(SQLCA)
idwc_embalaje.Retrieve()
idwc_embalaje.SetSort("emba_codigo A")
idwc_embalaje.Sort()
dw_1.SetItem(1, "cctd_reclae", gs_CodEmbalaje)

dw_2.GetChild("ccag_codigo", idwc_agronomos)
idwc_agronomos.SetTransObject(sqlca)
IF idwc_agronomos.Retrieve(gi_CodZona) = 0 THEN
	MessageBox("Atención","La Zona: " + String(gi_CodZona) + &
					" No tiene Agrónomos",Exclamation!)
	idwc_agronomos.InsertRow(0)
ELSE	
   idwc_agronomos.SetSort("ccagc_nombre A")
   idwc_agronomos.Sort()
END IF

dw_1.GetChild("cctd_reclas", idwc_calibres)
idwc_calibres.SetTransObject(sqlca)
idwc_calibres.Retrieve(gi_CodEspecie,gi_CodVariedad)

dw_1.GetChild("cctd_repesa", idwc_destino1)
idwc_destino1.SetTransObject(sqlca)
idwc_destino1.Retrieve(gi_CodEspecie)

dw_1.GetChild("cctd_cammer", idwc_destino2)
idwc_destino2.SetTransObject(sqlca)
idwc_destino2.Retrieve(gi_CodEspecie)

istr_mant.argumento[2]	=	String(gi_CodZona)
istr_mant.argumento[3]	=	String(gi_CodPlanta)
istr_mant.argumento[4]	=	String(gi_CodExport)
istr_mant.argumento[6]	=	String(today())
istr_mant.argumento[7]	=	String(gi_CodEspecie)
istr_mant.argumento[8]	=	"2"
istr_mant.argumento[12]	=	""
//istr_mant.argumento[6]	=	String(dw_2.Object.ccte_fecins[1])/*modific.por requerimiento*/
istr_mant.argumento[11]	=	""

iuo_ctlcaldanoespecie	=	Create uo_ctlcaldanoespecie
iuo_especie				=	Create uo_especie
iuo_Destino1			=	Create uo_DestinoRechazado
iuo_Destino2			=	Create uo_DestinoRechazado
end event

event ue_borra_detalle;IF dw_1.rowcount() < 1 THEN RETURN

SetPointer(HourGlass!)

ib_borrar = True
w_main.SetMicroHelp("Validando la eliminación de detalle...")

Message.DoubleParm = 0

This.TriggerEvent ("ue_validaborrar_detalle")

IF Message.DoubleParm = -1 THEN RETURN

 //	IF	dw_1.GetItemStatus(il_Fila,0,Primary!)	=	NewModified!	THEN	
		IF MessageBox("Eliminación de Registro", "Está seguro de Eliminar este Registro", Question!, YesNo!) = 1 THEN
			IF dw_1.DeleteRow(0) = 1 THEN
				ib_borrar = False
				w_main.SetMicroHelp("Borrando Registro...")
				SetPointer(Arrow!)
			ELSE
				ib_borrar = False
				MessageBox(This.Title,"No se puede borrar actual registro.")
			END IF
		
			IF dw_1.RowCount() = 0 THEN
				pb_eliminar.Enabled = False
			ELSE
				il_fila = dw_1.GetRow()
			END IF
		END IF
//	ELSE
//		MessageBox("Atención","No se borrarán registros ya almacenados")
//	END IF
end event

event ue_imprimir;Long		fila
					  
istr_info.titulo	=	"PALLET RECHAZADOS"
istr_info.copias	=	1

OpenWithParm(vinf,istr_info)

vinf.dw_1.DataObject	=	"dw_info_ctlcallotesobjetados_uvas"
vinf.dw_1.SetTransObject(sqlca)
vinf.dw_1.Getchild("espe_codigo", idwc_especie)
idwc_especie.SetTransObject(SQLCA)
idwc_especie.Retrieve(0)

fila	=	vinf.dw_1.Retrieve(dw_2.Object.plde_codigo[1], dw_2.Object.ccte_numero[1])

If fila	=	-1 Then
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ElseIf fila	=	0 Then
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
Else
	F_Membrete(vinf.dw_1)
	vinf.dw_1.Object.DataWindow.Zoom = 70
	If gs_Ambiente <> 'Windows' Then F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
End If
end event

event ue_antesguardar;Long		ll_Numero, ll_secuencia, ll_fila = 1
Integer  li_Planta, li_Cliente, li_Secuen, li_cont, li_Causal, li_cont1, li_lote, li_null
String	ls_Mensaje, ls_colu[]

dw_1.AcceptText()

BorraFila()

For ll_Fila = 1 TO dw_1.RowCount()
	If IsNull(dw_1.Object.cctd_tipins[ll_Fila]) OR dw_1.Object.cctd_tipins[ll_Fila] = 0 Then
		li_cont ++
		ls_mensaje 			= ls_mensaje + "~nTipo de Planilla"
		ls_colu[li_cont]	= "cctd_folpla"
	End If	
	
	If IsNull(dw_1.Object.cctd_folpla[ll_Fila]) OR dw_1.Object.cctd_folpla[ll_Fila] = 0 Then
		li_cont ++
		ls_mensaje 			= ls_mensaje + "~nFolio de Planilla"
		ls_colu[li_cont]	= "cctd_folpla"
	End If	
	
	If IsNull(dw_1.Object.cclo_numero[ll_Fila]) Or dw_1.Object.cclo_numero[ll_Fila] = 0 Then
		li_cont ++
		ls_mensaje 			= ls_mensaje + "~nNúmero Lote"
		ls_colu[li_cont]	= "cclo_numero"
	End If
	
	If dw_1.Object.cctd_resolu[ll_Fila] = 'R' Then 
		//resolucion()		
//		If IsNull(dw_1.Object.ccda_secuen[ll_Fila]) Or dw_1.Object.ccda_secuen[ll_Fila] = 0 Then 
//			MessageBox("Atención","Falta Ingresar Causal de Objeción",Exclamation!)
//			dw_1.SetColumn("ccda_secuen")
//		End If

/* 
20141113 		
		If IsNull(dw_1.Object.cctd_embal1[ll_Fila]) Or dw_1.Object.cctd_embal1[ll_Fila] = 0 Then 	
			If IsNull(dw_1.Object.cctd_calid1[ll_Fila]) Or dw_1.Object.cctd_calid1[ll_Fila] = 0 Then 	
				If IsNull(dw_1.Object.cctd_condi1[ll_Fila]) Or dw_1.Object.cctd_condi1[ll_Fila] = 0 Then 
						li_cont ++
						ls_mensaje 			= ls_mensaje + "~nCausal de Resolución"
						ls_colu[li_cont]	= "cctd_embal1"
				End If
			End If
		End If
fin comentario 20141113
*/		
		
		If	(Isnull(dw_1.Object.cctd_devpro[ll_Fila]) Or dw_1.Object.cctd_devpro[ll_Fila] = 0) And &
			(Isnull(dw_1.Object.cctd_otrare[ll_Fila]) Or dw_1.Object.cctd_otrare[ll_Fila] = '') And & 
			(Isnull(dw_1.Object.cctd_repesa[ll_Fila]) Or dw_1.Object.cctd_repesa[ll_Fila] = 0) And &
			dw_1.Object.cctd_resfin[ll_Fila] = 'R' Then
			li_cont ++
			ls_mensaje 		= ls_mensaje + "~nDestino del producto rechazado"
			ls_colu[li_cont]	= "cctd_reclas"
		End If

	ElseIf dw_1.Object.cctd_resolu[ll_Fila] = 'A' Then 
		
		If IsNull(dw_1.Object.ccda_secuen[ll_Fila]) OR dw_1.Object.ccda_secuen[ll_Fila] = 0 Then 
			MessageBox("Atención","Falta Ingresar Causal de Objeción",Exclamation!)
						dw_1.SetColumn("ccda_secuen")
		End If
		
		If dw_1.Object.cctd_embal1[ll_Fila] = 0 Then 	
			If dw_1.Object.cctd_calid1[ll_Fila] = 0 Then	
				If dw_1.Object.cctd_condi1[ll_Fila] = 0 Then 
						li_cont ++
						ls_mensaje 			= ls_mensaje + "~nCausal de Resolución"
						ls_colu[li_cont]	= "cctd_embal2"
				End If
			End If
		End If
	End If
	
	If IsNull(dw_1.Object.cctd_resfin[ll_Fila]) Or dw_1.Object.cctd_resfin[ll_Fila] = '' Then
		li_cont ++
		ls_mensaje 			= ls_mensaje + "~nResolución Final"
		ls_colu[li_cont]	= "cctd_resfin"
	End If
	
	If IsNull(dw_1.Object.cctd_embal2[ll_Fila]) Or dw_1.Object.cctd_embal2[ll_Fila] = 0 Then
		li_cont ++
		ls_mensaje 			= ls_mensaje + "~nQuien Resuelve"
		ls_colu[li_cont]	= "cctd_embal2"
	End If
Next

If li_cont = 8 And li_cont1 = 12 Then
	dw_1.DeleteRow(ll_fila)
Else
	If li_cont > 0 Then
		MessageBox("Error de Consistencia", "Falta el ingreso de :" + ls_mensaje + ".", StopSign!, Ok!)
		dw_1.SetColumn(ls_colu[1])
		dw_1.SetFocus()
		Message.DoubleParm = -1
	Else
		li_Planta	=	dw_2.Object.plde_codigo[1]
		li_Cliente	=	dw_2.Object.clie_codigo[1]
		ll_Numero	=	dw_2.Object.ccte_numero[1]	
		
		If dw_2.GetItemStatus(1,0,Primary!) = New! OR &
			dw_2.GetItemStatus(1,0,Primary!) = NewModIfied! Then
		
			UPDATE 	dba.ctlcalotesobjetadosenc
				SET 	ccte_numero = 1
				WHERE	ccte_numero = 1
				And	clie_codigo	=	li_Cliente;
		
			SELECT 	IsNull(Max(ccte_numero),0) + 1
				INTO 	:ll_numero
				FROM	dba.ctlcalotesobjetadosenc
				WHERE	clie_codigo	=	:li_Cliente
				And   plde_codigo =  :li_Planta;
		
			istr_mant.Argumento[1] = String(ll_numero)
		
			dw_2.SetItem(1,"ccte_numero",ll_numero)
				
		 End If
		
		If dw_1.GetItemStatus(1,0,Primary!) = New! OR &
			dw_1.GetItemStatus(1,0,Primary!) = NewModIfied! Then
		    SELECT 	IsNull(Max(cctd_secuen),0) + 1
				INTO 	:ll_secuencia
				FROM	dba.ctlcalotesobjetadosdet
				WHERE ccte_numero =  :ll_numero
				And   clie_codigo	=	:li_Cliente
				And   plde_codigo =  :li_Planta
				And   cclo_numero =  :li_lote;
		End If		
				
		For ll_Fila = 1 TO dw_1.RowCount()			
			li_Causal = dw_1.Object.ccda_secuen[ll_Fila]
			If (dw_1.GetItemStatus(ll_Fila, 0, Primary!) = New! OR &
				dw_1.GetItemStatus(ll_Fila, 0, Primary!) = NewModIfied!) And &
				Not IsNull(dw_1.Object.cctd_resolu[ll_Fila]) Then
				dw_1.SetItem(ll_Fila, "ccte_numero", ll_Numero)
				dw_1.SetItem(ll_Fila, "clie_codigo", gi_CodExport)
				dw_1.SetItem(ll_Fila, "cctd_cauobj", li_Causal)
				dw_1.SetItem(ll_Fila, "plde_codigo", li_Planta)
				dw_1.SetItem(ll_Fila, "cctd_secuen", ll_secuencia)
				
				ll_secuencia ++
			End If	
		Next
		HabilitaEncab(FALSE)
	End If
End If
end event

event ue_guardar;Captura_Usuario()

IF dw_1.AcceptText() = -1 THEN RETURN

SetPointer(HourGlass!)

w_main.SetMicroHelp("Grabando información...")

Message.DoubleParm = 0

TriggerEvent("ue_antesguardar")

IF Message.DoubleParm = -1 THEN RETURN

IF dw_1.RowCount() > 0 THEN
	
	IF wf_actualiza_db(False) THEN
		w_main.SetMicroHelp("Información Grabada.")
		//pb_eliminar.Enabled	= True
		pb_imprimir.Enabled	= True
	ELSE
		w_main.SetMicroHelp("No se puede Grabar información.")
		Message.DoubleParm = -1
		RETURN
	END IF
ELSE
	MessageBox("Error","La información no fué guardada al no haber detalle")
	pb_nuevo.Triggerevent(Clicked!)
END IF
end event

type dw_1 from w_mant_encab_deta_csd`dw_1 within w_maed_ctlcaltercerainspepecenc
event nomover pbm_syscommand
integer x = 32
integer y = 456
integer width = 3735
integer height = 1336
integer taborder = 80
string title = "Detalle Resolución de Lotes Objetados Pendientes"
string dataobject = "dw_mues_ctlcaltercerainspe"
boolean hsplitscroll = true
end type

event dw_1::nomover;
uint wParam, lParam

wParam = message.wordparm

CHOOSE CASE wParam
CASE 61456, 61458
message.processed = TRUE
message.returnValue = 0

END CHOOSE
end event

event type long dw_1::ue_seteafila(unsignedlong wparam, long lparam);il_fila	= This.GetRow()

RETURN 0
end event

event dw_1::rowfocuschanged;ib_datos_ok = True



end event

event dw_1::losefocus;AcceptText()
end event

event dw_1::itemchanged;String		ls_Nula, ls_Columna, li_ReclasIfica, ls_CalEmb, ls_CalCal, ls_CalCon,&
			ls_calIfi, ls_resolucion, ls_re
Integer	li_Planta, li_Lote, li_Causal, li_resolucion, ll_Familia = 80
Long		ll_Folio

This.AcceptText()

SetNull(ls_Nula)
ls_Columna	= dwo.name

Choose Case ls_Columna		
	Case "cctd_tipins"		
		If	NoExisteNumeroPlanilla(ls_Columna, Data) Or NoExisteNumeroLote(ls_Columna, data)	  Then
			dw_1.SetItem(il_Fila, ls_Columna, Integer(ls_Nula))			
			Return 1
		End If		
		
   Case "cctd_folpla"
		If IsNull(dw_1.Object.cctd_tipins[il_fila]) Then 
			Messagebox("Atención","Debe ingresar tipo de inspección",StopSign!)							
			dw_1.SetItem(row,"cctd_folpla",Long(ls_nula))	
			SetColumn("cctd_tipins")						
			Return 1
		ElseIf NoExisteNumeroPlanilla(ls_Columna, Data)  Or NoExisteNumeroLote(ls_Columna, data)		Then
			dw_1.SetItem(il_Fila, ls_Columna, Long(ls_Nula))
			dw_1.SetItem(row,"cclo_numero",Long(ls_nula))	
			Return 1
		End If		
		
	 Case "cclo_numero" 	
		dw_1.ModIfy("cctd_embal1.Protect='0'")	
		If NoExisteNumeroLote(ls_Columna, data) Then
			dw_1.SetColumn("cclo_numero")
			dw_1.SetItem(row, "cclo_numero", Long(ls_Nula))	
			dw_1.SetItem(row, "ccda_secuen", Long(ls_Nula))				
			Return 1	
		Else
			idwc_calibres.SetTransObject(sqlca)
			idwc_calibres.Retrieve(Integer(istr_Mant.Argumento[9]),Integer(istr_Mant.Argumento[10]))
		End If			
	
	Case  "cctd_embal1", "cctd_calid1", "cctd_condi1"
		If This.Object.cctd_resolu[Row] = 'R' Or This.Object.cctd_resolu[Row] = 'C' Or This.Object.cctd_resolu[Row] = 'O' Then ll_Familia = 10			
		If Not iuo_ctlcaldanoespecie.Existe(iuo_Especie.Codigo, Integer(Data), ll_Familia, False, Sqlca) Then
			ll_Familia = 30
			If Not iuo_ctlcaldanoespecie.Existe(iuo_Especie.Codigo, Integer(Data), ll_Familia, False, Sqlca) Then
				ll_Familia = 40
				If Not iuo_ctlcaldanoespecie.Existe(iuo_Especie.Codigo, Integer(Data), ll_Familia, True, Sqlca) Then
					This.SetItem(il_Fila, ls_Columna, Integer(ls_Nula))
					Return 1
				End If
			End If
		End If
		
	Case  "cctd_embal2", "cctd_calid2"
		If Not iuo_ctlcaldanoespecie.Existe(iuo_Especie.Codigo, Integer(Data), 80, True, Sqlca) Then
			This.SetItem(il_Fila, ls_Columna, Integer(ls_Nula))
			Return 1
		End If
				  
	Case "cctd_reclas"		
		If IsNull(dw_1.object.cclo_numero[il_fila]) Then 			
			Messagebox("Atención","Debe Ingresar Nro de Lote Previamente")
			dw_1.SetItem(il_Fila, "cctd_reclas", ls_Nula)
			Return 1
		End If  
		If Existecalibre(data) Then
			dw_1.SetItem(row,"cctd_reclas",ls_Nula)
			Return 1  	
		End If	
		
	Case "cctd_reclae"
		If NOT NoExisteNumeroLote(ls_Columna, Data) And NoExisteEmbalaje(Data) Then
			This.SetItem(Row, ls_Columna, ls_Nula)
			HabilitaIngreso(ls_Columna)
			Return 1
		Else
			istr_Mant.Argumento[10]	=	Data
		End If
	
	Case "cctd_resolu"
		If valida_reso(ls_Columna, data, This.Object.cctd_resolu[Row]) Then
			dw_1.SetItem(il_fila, ls_columna, ls_Nula)
			Return 1
		Else
			If data = 'A'Then  
				This.Object.cctd_resfin[Row] = 'A'
			Else
				This.Object.cctd_resfin[Row]		= ls_Nula
			End If
			This.Object.cctd_embal1[Row] 	= Integer(ls_Nula)
			This.Object.cctd_calid1[Row] 		= Integer(ls_Nula)
			This.Object.cctd_condi1[Row] 		= Integer(ls_Nula)
			This.Object.cctd_embal2[Row] 	= Integer(ls_Nula)
			This.Object.cctd_calid2[Row] 		= Integer(ls_Nula)
			This.Object.cctd_repesa[Row] 	= Integer(ls_Nula)
			This.Object.cctd_cammer[Row]	= Integer(ls_Nula)

		End If
	
	Case "cctd_resfin"
		If data = 'A'Then
			This.Object.cctd_repesa[Row] 	= Integer(ls_Nula)
			This.Object.cctd_cammer[Row]	= Integer(ls_Nula)
		ElseIf data = 'R'Then
			This.Object.cctd_embal2[Row] 	= Integer(ls_Nula)
		End If
		
End Choose

pb_grabar.Enabled	=	True
end event

event dw_1::getfocus;//return 0
end event

event dw_1::doubleclicked;//
end event

event dw_1::clicked;If Row > 0 Then 
	il_fila = Row
	This.SelectRow(0,False)
	This.SetRow(il_fila)
	This.SelectRow(il_fila,False)
End  If


/*
Para que funcione este ordenamiento los títulos deben tener el nombre
de la columna y terminacion "_t", de lo contrario no funcionará
*/
String	ls_old_sort, ls_column, ls_color_old
Char		lc_sort

IF IsNull(dwo) THEN RETURN

If Right(dwo.Name,2) = '_t' Then
	ls_column	= Left (dwo.Name, Len(String(dwo.Name)) - 2)
	ls_old_sort	= This.Describe("Datawindow.Table.sort")
	ls_color_old	=This.Describe(ls_Column + "_t.Color")

	If ls_column = Left(ls_old_sort, Len(ls_old_sort) - 2) Then
		lc_sort = Right(ls_old_sort, 1)
		If lc_sort = 'A' Then
			lc_sort = 'D'
		Else
			lc_sort = 'A'
		End If
		This.SetSort(ls_column+" "+lc_sort)
	
/*string ls_pos
long ll_pos
ll_pos = long(This.Describe(ls_Column + "_t.x"))
ls_pos = "datawindow.p_1.x="+string(ll_pos)
this.Modify(ls_pos) 
//this.object.p_1.visible = true
*/
	Else
		This.SetSort(ls_column+" A")
		This.Modify(Left(ls_old_sort, Len(ls_old_sort) - 2) + "_t.Color = " + ls_color_old)
	End If
	
//	This.Modify(dwo.Name + ".Color = " + String(Rgb(0, 0, 255)))

	
	This.Sort()
End If

end event

event dw_1::itemerror;call super::itemerror;Return 1 
end event

event dw_1::sqlpreview;//
end event

type dw_2 from w_mant_encab_deta_csd`dw_2 within w_maed_ctlcaltercerainspepecenc
integer x = 398
integer y = 36
integer width = 3063
integer height = 412
string dataobject = "dw_maed_tercerainspe_uvas"
end type

event dw_2::itemchanged;String	ls_Columna, ls_Nula

SetNull(ls_Nula)

ls_Columna	=	dwo.Name
dw_2.AcceptText()

Choose Case ls_Columna	
	Case "espe_codigo"
		If Not iuo_especie.existe(Integer(data),True,Sqlca) Then
			This.SetItem(Row, ls_Columna, Long(ls_nula))
			Return 1
		Else
			dw_1.GetChild("cctd_reclas", idwc_calibres)
			idwc_calibres.SetTransObject(sqlca)
			idwc_calibres.Retrieve(Integer(Data),0)
			istr_mant.argumento[7]	=	data
			
			dw_1.GetChild("cctd_repesa", idwc_destino1)
			idwc_destino1.SetTransObject(sqlca)
			idwc_destino1.Retrieve(Integer(Data))
			
			dw_1.GetChild("cctd_cammer", idwc_destino2)
			idwc_destino2.SetTransObject(sqlca)
			idwc_destino2.Retrieve(Integer(Data))
			/*
			Carga Datos
			*/
			dw_1.SetRedraw(False)
			dw_1.Reset()
			dw_1.SetRedraw(True)
			Inserta()
			avisa_fecha()
		End If	
		
	Case "zona_codigo"
		If NoExisteZona(Integer(data)) Then
			This.SetItem(Row, ls_Columna, Long(ls_nula))
			Return 1
		Else
			istr_mant.argumento[2] = Data
			/*
			Carga Datos
			*/
			dw_1.SetRedraw(False)
			dw_1.Reset()
			dw_1.SetRedraw(True)
			Inserta()
			avisa_fecha()

			
			dw_2.GetChild("ccag_codigo", idwc_agronomos)
			idwc_agronomos.SetTransObject(sqlca)
			
			If idwc_agronomos.Retrieve(Integer(istr_mant.argumento[2]))	= 0 Then
				MessageBox("Atención","La Zona Seleccionada, " + &
								"No Tiene Agrónomos Ingresados", Exclamation!)
			Else
				 dw_2.Object.ccag_codigo[1]	=	Long(ls_nula)
			End If								
		End If
		
	Case "plde_codigo"
		If NoExistePlanta(Integer(data)) Then
			This.SetItem(Row, ls_Columna, Long(ls_nula))
			Return 1
		Else
			istr_mant.argumento[3]	=	data
			/*
			Carga Datos
			*/
			dw_1.SetRedraw(False)
			dw_1.Reset()
			dw_1.SetRedraw(True)
			Inserta()
			avisa_fecha()
			
		End If	
			
	Case "ccte_numero"			
		If Existefolio(ls_columna, data)Then					
			dw_2.SetItem(1, "ccte_numero", Long(ls_nula))
			Return 1
		End If	
			
	Case "ccte_fecins"
		istr_mant.argumento[6]	=	data	
		
	Case "prod_ptodos"
		IF Integer(Data) = 1 THEN
			dw_2.Object.prod_ptodos[1] = 1
			dw_2.Object.prod_codigo.Protect				=	1
			dw_2.Object.prod_codigo.BackGround.Color  =  (553648127)
			idwc_productor.Reset()
			idwc_productor.Retrieve()
			idwc_productor.Sort()
			dw_2.SetItem(1, "prod_codigo",Long(ls_nula))

		ELSE	
			dw_2.Object.prod_codigo.Protect				=	0
			dw_2.Object.prod_codigo.BackGround.Color  =  RGB(255,255,255)
		END IF
	
Case "prod_codigo"
	If existeproductor(Long(data)) Then
		This.SetItem(Row, ls_Columna, Long(ls_nula))
		Return 1
	ELSE
		/*
			Carga Datos
			*/
			dw_1.SetRedraw(False)
			dw_1.Reset()
			dw_1.SetRedraw(True)
			Inserta()
			avisa_fecha()
	END IF		
			
//	No Saca Por Solicitud de V. Costa Correo 20121110
//Case "ccag_codigo"
//		
//	  If NoExisteAgronomo(Integer(data)) Then
//		  This.SetItem(1, ls_Columna, Long(ls_nula))
//		  Return 1
//	  Else
//		dw_1.SetRedraw(False)
//		dw_1.Reset()
//		dw_1.SetRedraw(True)
//		Inserta()
//		avisa_fecha()
//	  End If	
	  
End Choose

HabilitaIngreso(ls_Columna) 
	
end event

type pb_nuevo from w_mant_encab_deta_csd`pb_nuevo within w_maed_ctlcaltercerainspepecenc
integer x = 3941
integer y = 408
end type

type pb_eliminar from w_mant_encab_deta_csd`pb_eliminar within w_maed_ctlcaltercerainspepecenc
integer x = 3941
integer y = 588
end type

type pb_grabar from w_mant_encab_deta_csd`pb_grabar within w_maed_ctlcaltercerainspepecenc
integer x = 3941
integer y = 772
end type

type pb_imprimir from w_mant_encab_deta_csd`pb_imprimir within w_maed_ctlcaltercerainspepecenc
integer x = 3941
integer y = 948
end type

type pb_salir from w_mant_encab_deta_csd`pb_salir within w_maed_ctlcaltercerainspepecenc
integer x = 3941
integer y = 1128
end type

type pb_ins_det from w_mant_encab_deta_csd`pb_ins_det within w_maed_ctlcaltercerainspepecenc
integer x = 3954
integer y = 1420
integer taborder = 90
end type

type pb_eli_det from w_mant_encab_deta_csd`pb_eli_det within w_maed_ctlcaltercerainspepecenc
integer x = 3945
integer y = 1592
integer taborder = 100
end type

type pb_buscar from w_mant_encab_deta_csd`pb_buscar within w_maed_ctlcaltercerainspepecenc
integer x = 3941
integer y = 228
end type

type dw_3 from uo_dw within w_maed_ctlcaltercerainspepecenc
boolean visible = false
integer x = 759
integer y = 1116
integer width = 2263
integer height = 584
integer taborder = 11
string dataobject = "dw_mues_ctlcalhistobjetados_uvas"
boolean hscrollbar = true
boolean ib_allow_inserts = false
end type

