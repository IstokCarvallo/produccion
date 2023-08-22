$PBExportHeader$w_maed_ctlcalplanillaespecies_auto.srw
$PBExportComments$Ingresador de antecedentes para Planilla Verificación Cerezas
forward
global type w_maed_ctlcalplanillaespecies_auto from w_mant_encab_deta_csd
end type
type dw_3 from datawindow within w_maed_ctlcalplanillaespecies_auto
end type
type cb_aprobar from commandbutton within w_maed_ctlcalplanillaespecies_auto
end type
type tab_1 from tab within w_maed_ctlcalplanillaespecies_auto
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
type tab_1 from tab within w_maed_ctlcalplanillaespecies_auto
tabpage_1 tabpage_1
tabpage_2 tabpage_2
end type
end forward

global type w_maed_ctlcalplanillaespecies_auto from w_mant_encab_deta_csd
integer width = 5038
integer height = 2256
string menuname = ""
windowstate windowstate = maximized!
event ue_validapassword ( )
dw_3 dw_3
cb_aprobar cb_aprobar
tab_1 tab_1
end type
global w_maed_ctlcalplanillaespecies_auto w_maed_ctlcalplanillaespecies_auto

type variables
DataWindowChild 	idwc_clientes,idwc_zonas,idwc_plantas,idwc_productores,idwc_especies, idwc_variedades, &
						idwc_packings,idwc_calibres,idwc_embalajes,idwc_agronomos,idwc_tipinspec,idwc_inspector,&
						idwc_categorias, idwc_Camaras

Integer  ii_sw

w_mant_deta_ctlcalplanillacerezas	iw_mantencion
w_mant_deta_ctlcalplanillakiwis   		iw_mantencion1
w_mant_deta_ctlcalplanillanaranjas  	iw_mantencion2 
w_mant_deta_ctlcalplanillapaltas    	iw_mantencion3
w_mant_deta_ctlcalplanillagranados  	iw_mantencion4
w_mant_deta_ctlcalplanillaciruelas  	iw_mantencion5
w_mant_deta_ctlcalplanillalimones	iw_mantencion6
w_mant_deta_ctlcalplanillalimas		iw_mantencion7
w_mant_deta_ctlcalplanillaclementinas	iw_mantencion8

uo_zonas             				iuo_zonas
uo_plantadesp        			iuo_plantas
uo_Especie						iuo_Especie
uo_productores       			iuo_productor
uo_loteobjetadopendiente	iuo_Objetados
uo_variedades        			iuo_variedades
uo_ctlcalagronomos 	  		iuo_agronomos
uo_embalajesprod     		iuo_embalajesprod
uo_ctlcaltipoinspec  			iuo_ctlcaltipoinspec
uo_ctlcalinspectores 			iuo_ctlcalinspectores
uo_Cliente						iuo_Clientes
uo_Etiquetas					iuo_Etiquetas

DataStore						ids_orden
end variables

forward prototypes
protected function integer wf_modifica ()
public function boolean noexistecalibre (string as_calibre)
public function boolean noexistegrupo (string data)
public subroutine ingresodedatos ()
public function boolean existedetalleplanilla (string as_columna, string as_valor)
public function boolean existenumeroplanilla (string as_columna, string as_valor)
public subroutine habilitaencab (boolean habilita)
public subroutine habilitaingreso (string ls_columna)
protected function boolean wf_actualiza_db (boolean borrando)
public function boolean duplicado (string data)
public subroutine buscalote ()
public subroutine limpia_data ()
public function boolean existenumerolote (string as_columna, string as_valor)
public function boolean noexisteembalaje (integer cliente, string embalaje)
public function integer nuevolote (integer cliente, integer planta)
public subroutine validalote (integer planta)
public subroutine loterecepcion ()
public function boolean loteduplicado (string columna, string valor)
public function boolean existemovimiento ()
protected function boolean noexistelotes (integer ai_valor, integer ai_tipo)
public function boolean trae_planilla (long al_planilla, integer ai_planta, integer ai_especie)
public subroutine ingresoencab ()
public function boolean orden_proceso (integer ai_cliente, integer ai_planta, integer ai_tipo, long al_numero, integer ai_especie)
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

public function boolean noexistecalibre (string as_calibre);Integer li_contador

Select Count(*)
Into :li_Contador
From dbo.variecalibre
Where vaca_calibr = :as_calibre;

IF sqlca.sqlcode = -1 THEN
	F_ErrorBaseDatos(sqlca,"No se pudo leer la tabla Calibre")
	RETURN TRUE
ELSEIF li_Contador = 0 THEN
	RETURN TRUE
ELSE	
	RETURN FALSE	
END IF
	

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

public subroutine ingresodedatos ();String	ls_Mensaje, ls_Columna[]
Integer	li_Contador

IF Isnull(dw_2.Object.plde_codigo[il_Fila]) OR dw_2.Object.plde_codigo[il_Fila] = 0 THEN
	li_Contador	++
	ls_Mensaje 			= ls_Mensaje + "~nNúmero de Planta"
	ls_Columna[li_Contador]	= "plde_codigo"
END IF

IF Isnull(dw_2.Object.ccpv_numero[il_Fila]) OR dw_2.Object.ccpv_numero[il_Fila] = 0 THEN
	li_Contador	++
	ls_Mensaje 			= ls_Mensaje + "~nnúmero de Planilla"
	ls_Columna[li_Contador]	= "ccpv_numero"
END IF

IF li_Contador > 0 THEN
	MessageBox("Error de Consistencia", "Falta el Ingreso de :" + ls_Mensaje + ".", StopSign!, Ok!)
	dw_2.SetColumn(ls_Columna[1])
	dw_2.SetFocus()
	Message.DoubleParm = -1
END IF
end subroutine

public function boolean existedetalleplanilla (string as_columna, string as_valor);Integer	li_planta, li_especie, li_variedad, li_packing, li_zona, &
         li_tecnico, li_inspector, li_tipoins, li_agronomo, li_estado
Long		ll_numero, ll_noguia, ll_productor, ll_lote
String	ls_Reslot
Date		ld_Fecemb, ld_fecins

ll_numero 		=	dw_2.Object.ccpv_numero[1]
li_Planta		=	dw_2.Object.plde_codigo[1]
// Hacer cambios para mostrar los lotes que existen para el detalle
CHOOSE CASE as_columna
	
	CASE "plde_codpak"
		ll_numero 		=	Long(as_valor)

END CHOOSE

SELECT   zona_codigo, prod_codigo, 
         espe_codigo, vari_codigo, 
			ccag_codigo, ccpv_fecins, 
			ccti_codigo, ccpv_estado
	INTO	:li_zona, :ll_productor, 
	      :li_especie,:li_variedad,
			:li_agronomo, :ld_fecins, 
			:li_tipoins, :li_estado
	FROM	dbo.ctlcalplaniverifienca
	WHERE clie_codigo	=	:gstr_parempresa.empr_codexp
	AND	plde_codigo	=	:li_Planta
	AND	ccpv_numero	=	:ll_numero
	GROUP BY zona_codigo, prod_codigo, &
         espe_codigo, vari_codigo, ccag_codigo, ccpv_fecins, &
			ccti_codigo, ccpv_estado;	
	

IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla ctlcalplaniverifienca")
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
	istr_mant.argumento[14]	= String(li_estado)
	//istr_mant.argumento[15]	= ls_Reslot

	dw_2.SetItem(1, "zona_codigo", li_zona)
	dw_2.SetItem(1, "plde_codigo", li_planta)
	dw_2.SetItem(1, "ccpv_fechin", ld_fecins)		
	dw_2.SetItem(1, "prod_codigo", ll_productor)		
	dw_2.SetItem(1, "vari_codigo", li_variedad)
	dw_2.SetItem(1, "ccag_codigo", li_agronomo)		
	dw_2.SetItem(1, "ccti_codigo", li_tipoins)	
	dw_2.SetItem(1, "ccpv_estado", li_estado)
	//dw_2.SetItem(1, "ccpv_reslot", ls_reslot)	
	

	dw_2.GetChild("plde_codigo", idwc_plantas)
	idwc_plantas.SetTransObject(sqlca)
	idwc_plantas.Retrieve(1,li_zona)

	dw_3.GetChild("plde_codpak", idwc_packings)
	idwc_packings.SetTransObject(sqlca)
	idwc_packings.Retrieve(2,li_zona)

	dw_2.GetChild("ccag_codigo", idwc_agronomos)
	idwc_agronomos.SetTransObject(sqlca)
	idwc_agronomos.Retrieve(0,li_zona)

	dw_2.GetChild("prod_codigo", idwc_productores)
	idwc_productores.SetTransObject(sqlca)
	idwc_productores.Retrieve()
	RETURN True
ELSE	
	RETURN False
END IF



end function

public function boolean existenumeroplanilla (string as_columna, string as_valor);Integer	li_lote, li_planta, li_especie, li_variedad, li_packing, li_zona, &
         li_tecnico, li_inspector, li_tipoins, li_agronomo, li_estado
Long		ll_numero, ll_noguia, ll_productor
String	ls_Reslot
Date		ld_Fecemb, ld_fecins

ll_numero 		=	dw_2.Object.ccpv_numero[1]
li_Planta		=	dw_2.Object.plde_codigo[1]


CHOOSE CASE as_columna
	
	CASE "ccpv_numero"
		ll_numero 		=	Long(as_valor)

	CASE "plde_codigo"
		li_Planta 		=	Integer(as_valor)
		
END CHOOSE

SELECT   zona_codigo, prod_codigo,espe_codigo, 
         vari_codigo, ccag_codigo,
			ccpv_fecins, ccpv_estado
	INTO	:li_zona, :ll_productor, :li_especie, 
			:li_variedad, :li_agronomo, 
			:ld_fecins, :li_tipoins,
			:li_estado
	FROM	dbo.ctlcalplaniverifienca
	WHERE clie_codigo	=	:gstr_parempresa.empr_codexp
	AND	plde_codigo	=	:li_Planta
	AND	ccpv_numero	=	:ll_numero
	GROUP BY zona_codigo, prod_codigo, &
         espe_codigo, vari_codigo,
			ccag_codigo, ccpv_fecins, 
			ccpv_estado;	
	

IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla ctlcalplaniverifienca")
	RETURN True
ELSEIF sqlca.sqlcode	=	0 THEN
	
	Messagebox("Atención","Número de Planilla ya Existe Para Planta Digitada",Exclamation!)

	RETURN True
ELSE	
	RETURN False
END IF


end function

public subroutine habilitaencab (boolean habilita);If Habilita Then
	dw_2.Object.clie_codigo.Protect				=	0
	dw_2.Object.zona_codigo.Protect				=	0
	dw_2.Object.plde_codigo.Protect				=	0
	dw_2.Object.ccpv_numero.Protect				=	0
	dw_2.Object.ccpv_fecins.Protect				=	0
	dw_2.Object.ccti_codigo.Protect				=	0
	dw_2.Object.prod_codigo.Protect				=	0
	dw_2.Object.vari_codigo.Protect				=	0
	dw_2.Object.ccag_codigo.Protect				=	0
	dw_3.Object.emba_codigo.Protect				=	0
	dw_3.Object.cclo_fecemb.Protect				=	0
	dw_3.Object.vaca_calibr.Protect				=	0
	dw_2.Object.zona_codigo.BackGround.Color	=	Rgb(255,255,255)
	dw_2.Object.clie_codigo.BackGround.Color		=	Rgb(255,255,255)
	dw_2.Object.plde_codigo.BackGround.Color		=	Rgb(255,255,255)
	dw_2.Object.ccpv_numero.BackGround.Color	=	Rgb(255,255,255)
	dw_2.Object.ccpv_fecins.BackGround.Color		=	Rgb(255,255,255)
	dw_2.Object.ccti_codigo.BackGround.Color		=	Rgb(255,255,255)
	dw_2.Object.prod_codigo.BackGround.Color		=	Rgb(255,255,255)
	dw_2.Object.vari_codigo.BackGround.Color		=	Rgb(255,255,255)
	dw_2.Object.ccag_codigo.BackGround.Color		=	Rgb(255,255,255)
	dw_3.Object.vaca_calibr.BackGround.Color		=	Rgb(255,255,255)
	dw_3.Object.emba_codigo.BackGround.Color	=	Rgb(255,255,255)
	dw_3.Object.cclo_fecemb.BackGround.Color	=	Rgb(255,255,255)
Else
	dw_2.Object.clie_codigo.Protect				=	1
	dw_2.Object.zona_codigo.Protect				=	1
	dw_2.Object.plde_codigo.Protect				=	1
	dw_2.Object.ccpv_numero.Protect				=	1
	dw_2.Object.ccpv_fecins.Protect				=	1
	dw_2.Object.ccti_codigo.Protect				=	1
	dw_2.Object.prod_codigo.Protect				=	1
	dw_2.Object.vari_codigo.Protect				=	1
	dw_2.Object.ccag_codigo.Protect				=	1
	dw_3.Object.emba_codigo.Protect				=	1
	dw_3.Object.cclo_fecemb.Protect				=	1	
	dw_3.Object.vaca_calibr.Protect				=	1
	dw_2.Object.zona_codigo.BackGround.Color	=	553648127
	dw_2.Object.clie_codigo.BackGround.Color		=	553648127
	dw_2.Object.plde_codigo.BackGround.Color		=	553648127
	dw_2.Object.ccpv_numero.BackGround.Color	=	553648127
	dw_2.Object.ccpv_fecins.BackGround.Color		=	553648127
	dw_2.Object.ccti_codigo.BackGround.Color		=	553648127
	dw_2.Object.prod_codigo.BackGround.Color		=	553648127
	dw_2.Object.vari_codigo.BackGround.Color		=	553648127
	dw_2.Object.ccag_codigo.BackGround.Color		=	553648127
	dw_3.Object.vaca_calibr.BackGround.Color		=	553648127
	dw_3.Object.emba_codigo.BackGround.Color	=	553648127	
	dw_3.Object.cclo_fecemb.BackGround.Color	=	553648127
End If
end subroutine

public subroutine habilitaingreso (string ls_columna);Boolean	lb_Estado = True

dw_2.AcceptText()
dw_3.AcceptText()

If ls_Columna <> "clie_codigo" And &
	(dw_2.Object.clie_codigo[1]) = 0 OR IsNull(dw_2.Object.clie_codigo[1]) Then
	lb_Estado	=	False
End If

If ls_Columna <> "ccti_codigo" And &
	(dw_2.Object.ccti_codigo[1]) = 0 OR IsNull(dw_2.Object.ccti_codigo[1]) Then
	lb_Estado	=	False
End If

If iuo_Especie.Codigo <> 82 Then
	If ls_Columna <> "lote_codigo" And &
		(dw_2.Object.lote_codigo[1]) = 0 OR IsNull(dw_2.Object.lote_codigo[1]) Then
		lb_Estado	=	False
	End If
End If

If ls_Columna <> "prod_codigo" And &
	(dw_2.Object.prod_codigo[1]) = 0 OR IsNull(dw_2.Object.prod_codigo[1]) Then
	lb_Estado	=	False
End If

If ls_Columna <> "ccag_codigo" And &
	(dw_2.Object.ccag_codigo[1]) = 0 OR IsNull(dw_2.Object.ccag_codigo[1]) Then
	lb_Estado	=	False
End If

If ls_Columna <> "ccpv_numero" And &
	(dw_2.Object.ccpv_numero[1]) = 0 OR IsNull(dw_2.Object.ccpv_numero[1]) Then
	lb_Estado	=	False
End If

If ls_Columna <> "emba_codigo" And &
	(dw_3.Object.emba_codigo[1]) = "" OR IsNull(dw_3.Object.emba_codigo[1]) Then
	lb_Estado	=	False
End If

If ls_Columna <> "cclo_fecemb" And &
	Date(istr_mant.argumento[5]) < Date(dw_3.Object.cclo_fecemb[1]) Then
	lb_Estado	= False	
End If 

If ls_Columna <> "plde_codpak" And &
	(dw_3.Object.plde_codpak[1]) = 0 OR IsNull(dw_3.Object.plde_codpak[1]) Then
	lb_Estado	=	False
End If

If ls_Columna <> "vaca_calibr" And &
	(dw_3.Object.vaca_calibr[1]) = '' OR IsNull(dw_3.Object.vaca_calibr[1]) Then
	lb_Estado	=	False
End If

If gstr_parlote.codgen = 1 OR istr_mant.argumento[19] <> '1' Then
	lb_Estado = True
ELSE
	lb_Estado = False
End If

pb_ins_det.Enabled	=	lb_Estado
pb_grabar.Enabled	=	lb_Estado
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

public subroutine buscalote ();Integer	 li_Planta, li_Especie, li_zona, li_cont
Long      ll_fila, ll_lote
String    ls_mensaje, ls_colu[]

If (IsNull(dw_2.Object.clie_codigo[1]) OR dw_2.Object.clie_codigo[1] = 0) Then
	li_cont	++
	ls_mensaje 		= ls_mensaje + "~nCliente"
	ls_colu[li_cont]	= "clie_codigo"
End If

If (IsNull(dw_2.Object.prod_codigo[1]) OR dw_2.Object.prod_codigo[1] = 0) Then
	li_cont	++
	ls_mensaje 		= ls_mensaje + "~nProductor"
	ls_colu[li_cont]	= "prod_codigo"
End If

If (IsNull(dw_2.Object.vari_codigo[1]) OR dw_2.Object.vari_codigo[1] = 0) Then
	li_cont	++
	ls_mensaje 		= ls_mensaje + "~nVariedad"
	ls_colu[li_cont]	= "vari_codigo"
End If

If (IsNull(dw_2.Object.plde_codigo[1]) OR dw_2.Object.plde_codigo[1] = 0) Then
	li_cont	++
	ls_mensaje 		= ls_mensaje + "~nPlanta"
	ls_colu[li_cont]	= "plde_codigo"
End If

If li_cont > 0 Then
	MessageBox("Error de Consistencia", "Falta el Ingreso de :" + ls_mensaje + ".", StopSign!, Ok!)
	dw_2.SetColumn(ls_colu[1])
	dw_2.SetFocus()
	Message.DoubleParm = -1
Else
	
	istr_busq.argum[1] = String(dw_2.object.plde_codigo[1])
	istr_busq.argum[2] = String(dw_2.object.prod_codigo[1])
	istr_busq.argum[4] =	String(dw_2.Object.zona_codigo[1])
	istr_busq.argum[5] = String(dw_2.Object.clie_codigo[1])
	istr_busq.argum[7] =	String(dw_3.Object.emba_codigo[1])
	istr_busq.argum[9] =	String(dw_3.Object.cclo_fecemb[1])
	istr_busq.argum[6] = String(dw_2.Object.vari_codigo[1])
	istr_busq.argum[12] = String(dw_3.Object.vaca_calibr[1])
	istr_busq.argum[13] = String(iuo_Especie.Codigo)
	istr_busq.argum[14] = String(dw_3.Object.etiq_codigo[1])
	
	OpenWithParm(w_busc_verificacionlotes, istr_busq)
	
	istr_busq	= Message.PowerObjectParm
		
	If istr_busq.argum[4] <> "" Then
		li_Planta								=	Integer(istr_busq.argum[1])
		ll_Lote								=	Integer(istr_busq.argum[3])
		dw_2.Object.plde_codigo[1]	=	Integer(istr_busq.argum[1])
		dw_2.Object.vari_codigo[1]	=	Integer(istr_busq.argum[6])
		dw_2.Object.cclo_numero[1]	=	Long(istr_busq.argum[3])
		dw_2.Object.prod_codigo[1]	=  Long(istr_busq.argum[2])
				 		
		istr_mant.Argumento[1]			=	istr_busq.argum[3]
		istr_mant.argumento[4]			=	istr_busq.argum[1]
		istr_mant.argumento[8]			=	istr_busq.argum[2]
		istr_mant.argumento[10]		=	istr_busq.argum[6]
		istr_mant.argumento[12]		=	istr_busq.argum[7]
		istr_mant.argumento[13]		=	istr_busq.argum[10]
		istr_mant.argumento[16]		=	istr_busq.argum[11]
		istr_mant.argumento[17]		=	istr_busq.argum[8]
		istr_mant.argumento[18]		=	istr_busq.argum[9]
		istr_mant.argumento[20]		=	istr_busq.argum[12]
			
		ll_fila	= dw_3.Retrieve(dw_2.Object.clie_codigo[1],li_Planta,ll_Lote)
		pb_ins_det.Enabled	=	True
		pb_grabar.Enabled	=	True
		
		If ll_fila = -1 Then
			MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
											Information!, RetryCancel!)
			Return
		Else
			dw_1.Reset()
			Return
		End If
	
	End If
End If

end subroutine

public subroutine limpia_data ();String ls_Nula
Setnull(ls_Nula)

dw_3.SetItem(1, "emba_codigo", ls_Nula)
dw_3.SetItem(1, "plde_codpak", Integer (ls_Nula))
dw_3.SetItem(1, "cclo_fecemb", Integer (ls_Nula))
dw_3.SetItem(1, "vaca_calibr", ls_Nula)
dw_3.SetItem(1, "cclo_tamlot", Integer (ls_Nula))
end subroutine

public function boolean existenumerolote (string as_columna, string as_valor);Integer	li_planta, li_especie, li_variedad, li_packing, li_zona,&
			li_NeoLote, li_TamLot, li_Contador, li_tipins, respuesta, li_etiqueta
Long		ll_nfolio, ll_numero, ll_Fila, ll_noguia, ll_productor, ll_lote
String	ls_Embalaje, ls_calibre, ls_Null
Date		ld_Fecemb
Boolean	lb_Retorno = False

SetNull(ls_Null)

li_Zona			=	dw_2.Object.zona_codigo[1]
li_planta		=	dw_2.Object.plde_codigo[1]
ll_lote 			=	dw_2.Object.cclo_numero[1]
ll_numero 		=	dw_2.Object.ccpv_numero[1]
ll_productor	=	dw_2.Object.prod_codigo[1]
li_especie		=	iuo_Especie.Codigo
li_variedad		=	dw_2.Object.vari_codigo[1]
ls_embalaje		=	dw_3.Object.emba_codigo[1]
li_packing		=	dw_3.Object.plde_codpak[1]
ls_calibre		=	dw_3.Object.vaca_calibr[1]
ld_fecemb		=	dw_3.Object.cclo_fecemb[1]
li_tipins		=	dw_2.Object.ccti_codigo[1]

CHOOSE CASE as_columna
	
	CASE "plde_codigo"
		li_planta		=	Integer(as_valor)
		
	CASE "cclo_numero"
		ll_nfolio 		=	Long(as_valor)

	CASE "ccpv_numero"
		ll_numero 		=	Long(as_valor)

	CASE "prod_codigo"
		ll_productor	=	Long(as_valor)		
		
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
	
	CASE "etiq_codigo"		
		li_etiqueta		=	Integer(as_valor)
			
END CHOOSE

IF NOT IsNull(li_Variedad) &
	AND NOT IsNull(ls_Calibre) AND NOT IsNull(ll_Productor) &
	AND NOT IsNull(li_Packing) AND NOT IsNull(ls_Embalaje) AND &
	ld_FecEmb <> Date('01/01/1900')  AND NOT IsNull(li_etiqueta) THEN

	istr_mant.argumento[2]	=	String(ll_Numero)
	istr_mant.argumento[3]	=	String(li_Zona)							
	istr_mant.argumento[4]	=	String(li_Planta)
	istr_mant.argumento[7]	=	String(gi_CodExport)
	istr_mant.argumento[8]	=	String(ll_Productor)
	istr_mant.argumento[9]	=	String(iuo_Especie.Codigo)
	istr_mant.argumento[10]	=	String(li_Variedad)
	istr_mant.argumento[12]	=	ls_embalaje
	istr_mant.argumento[18]	=	String(ld_FecEmb)	
	istr_mant.argumento[16]	=	String(li_Packing)
	istr_mant.argumento[17]	=	ls_Calibre	
	istr_mant.argumento[20]	=	String(li_etiqueta)
	
		SELECT	cclo_numero, cclo_tamlot
			INTO	:ll_lote, :li_Tamlot
		  FROM	dbo.ctlcallotes
		WHERE	plde_codigo	=	:li_Planta
			AND   prod_codigo =  :ll_Productor
			AND   espe_codigo =  :iuo_Especie.Codigo
			AND   vari_codigo =  :li_Variedad
			AND   emba_codigo =  :ls_Embalaje
			AND   vaca_calibr =  :ls_Calibre
			AND   plde_codpak =  :li_Packing
			AND   cclo_fecemb =  :ld_FecEmb
			AND	etiq_codigo = :li_etiqueta;
			
			IF Sqlca.SQLCode = -1 THEN	
				F_errorbasedatos(sqlca,"Lectura tabla CTLCALLOTES")
			ELSEIF ll_Lote > 0 THEN	
				SELECT	Count(*)
					INTO	:li_Contador
					FROM	dbo.ctlcalplaniverifienca
					WHERE	plde_codigo = :li_Planta
					AND   cclo_numero = :ll_Lote
					AND   ccpv_numero = :ll_Numero;
			
				IF sqlca.SQLCode = -1 THEN
					F_errorbasedatos(sqlca,"Lectura tabla ctlcalplaniverifienca")
				ELSEIF li_Contador	>	0	THEN						
					istr_mant.argumento[1]	=	String(ll_Lote)
					
					This.TriggerEvent("ue_recuperadatos")			
							
					lb_Retorno	=	True
				ELSE
					ll_fila	= dw_3.Retrieve(gi_CodExport,li_Planta,ll_Lote)
		
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

RETURN lb_Retorno
end function

public function boolean noexisteembalaje (integer cliente, string embalaje);Integer li_contador

SELECT	Count(*)
	INTO	:li_Contador
	FROM	dbo.embalajesprod
	WHERE	clie_codigo = :cliente
	AND emba_codigo = :embalaje;

IF sqlca.sqlcode = -1 THEN
	F_ErrorBaseDatos(sqlca,"No se pudo leer la tabla Embalajesprod")
	RETURN TRUE
ELSEIF li_Contador = 0 THEN
	RETURN TRUE
ELSE	
	RETURN FALSE	
END IF
	

end function

public function integer nuevolote (integer cliente, integer planta);Integer  li_Contador, li_planta, li_especie, li_variedad, li_packing, li_tipoinsp
Long		ll_numero, ll_noguia, ll_productor, ll_lote
String	ls_Embalaje, ls_calibre
Date		ld_Fecemb

li_planta		=	dw_2.Object.plde_codigo[1]
ll_lote 		=	dw_2.Object.cclo_numero[1]
ll_numero 	=	dw_2.Object.ccpv_numero[1]
ll_productor	=	dw_2.Object.prod_codigo[1]
li_especie	=	dw_2.Object.espe_codigo[1]
li_variedad	=	dw_2.Object.vari_codigo[1] 
ls_embalaje	=	dw_3.Object.emba_codigo[1]
li_packing	=	dw_3.Object.plde_codpak[1]
ls_calibre	=	dw_3.Object.vaca_calibr[1]
ld_fecemb	=	dw_3.Object.cclo_fecemb[1]
ll_noguia		=	dw_3.Object.cclo_noguia[1]
li_tipoinsp	=	dw_2.Object.ccti_codigo[1]

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
	ELSEIF ll_Lote > 0 THEN	
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

public subroutine validalote (integer planta);//Integer	li_lote, li_planta, li_especie, li_variedad, li_packing, li_zona,&
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
//ll_numero 		=	dw_2.Object.ccpv_numero[1]
//ll_productor	=	dw_2.Object.prod_codigo[1]
//li_especie		=	ii_especie
//li_variedad		=	dw_2.Object.vari_codigo[1]
//ls_embalaje		=	dw_3.Object.emba_codigo[1]
//li_packing		=	dw_3.Object.plde_codpak[1]
//ls_calibre		=	dw_3.Object.vaca_calibr[1]
//ld_fecemb		=	dw_3.Object.cclo_fecemb[1]
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
//	CASE "ccpv_numero"
//		ll_numero 		=	Long(as_valor)
//
//	CASE "prod_codigo"
//		ll_productor	=	Long(as_valor)
//		
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
//END CHOOSE
//
//IF NOT IsNull(li_Variedad) &
//	AND NOT IsNull(ls_Calibre) AND NOT IsNull(ll_Productor) &
//	AND NOT IsNull(li_Packing) AND NOT IsNull(ls_Embalaje) AND &
//	ld_FecEmb <> Date('01/01/1900') THEN
//
//	istr_mant.argumento[2]	=	String(ll_Numero)
//	istr_mant.argumento[3]	=	String(li_Zona)							
//	istr_mant.argumento[4]	=	String(li_Planta)
//	istr_mant.argumento[7]	=	String(gi_CodExport)
//	istr_mant.argumento[8]	=	String(ll_Productor)
//	istr_mant.argumento[9]	=	String(ii_Especie)
//	istr_mant.argumento[10]	=	String(li_Variedad)
//	istr_mant.argumento[12]	=	ls_embalaje
//	istr_mant.argumento[18]	=	String(ld_FecEmb)	
//	istr_mant.argumento[16]	=	String(li_Packing)
//	istr_mant.argumento[17]	=	ls_Calibre	
//	
//		SELECT	cclo_numero, cclo_tamlot
//			INTO	:li_lote, :li_Tamlot
//			FROM	dba.ctlcallotes
//			WHERE	plde_codigo	=	:li_Planta
//			AND   prod_codigo =  :ll_Productor
//			AND   espe_codigo =  :ii_Especie
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
//					FROM	dba.ctlcalplaniverifienca
//					WHERE	plde_codigo = :li_Planta
//					AND   cclo_numero = :li_Lote
//					AND   ccpv_numero = :ll_Numero;
//			
//				IF sqlca.SQLCode = -1 THEN
//					F_errorbasedatos(sqlca,"Lectura tabla ctlcalplaniverifienca")
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
//
//RETURN lb_Retorno
end subroutine

public subroutine loterecepcion ();Integer	 li_Planta, li_Especie, li_zona
Long      ll_fila, ll_lote

IF (Isnull(dw_2.Object.plde_codigo[1]) OR dw_2.Object.plde_codigo[1] = 0) OR &
(Isnull(dw_2.Object.espe_codigo[1]) OR dw_2.Object.espe_codigo[1] = 0) OR &
(Isnull(dw_2.Object.vari_codigo[1]) OR dw_2.Object.vari_codigo[1] = 0) OR &
(Isnull(dw_2.Object.prod_codigo[1]) OR dw_2.Object.prod_codigo[1] = 0) THEN
   Messagebox("Atención","Debe Seleccionar Planta, Especie, Variedad y Productor ")
	dw_2.setfocus()
	RETURN
ELSE
	istr_busq.argum[1] = String(dw_2.object.plde_codigo[1])
	istr_busq.argum[2] = String(dw_2.object.espe_codigo[1])
	istr_busq.argum[4] =	String(dw_2.Object.vari_codigo[1])
	istr_busq.argum[5] = String(dw_2.Object.prod_codigo[1])
	
	OpenWithParm(w_busc_loterecepcion, istr_busq)
	
	istr_busq	= Message.PowerObjectParm
		
IF istr_busq.argum[6] <> "" THEN
	   dw_2.Object.lote_codigo[1] =	Integer(istr_busq.argum[6])
		dw_2.Object.ccpv_nlote2[1] =	Integer(istr_busq.argum[7])
		dw_2.Object.ccpv_nlote3[1] =	Integer(istr_busq.argum[8])
		dw_2.Object.ccpv_nlote4[1] =	Integer(istr_busq.argum[9])
		dw_2.Object.ccpv_nlote5[1] =	Integer(istr_busq.argum[10])
//		
//		ll_fila	= dw_2.Retrieve(istr_busq.argum[1],istr_busq.argum[2],&
//		                         istr_busq.argum[4],istr_busq.argum[5])
//		
//		IF ll_fila = -1 THEN
//			MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
//											Information!, RetryCancel!)
//			RETURN
//		ELSE
//			dw_1.Reset()
//			RETURN
//		END IF
	
	END IF
END IF
end subroutine

public function boolean loteduplicado (string columna, string valor);//Long		ll_fila
//Integer	li_codigo
//
//li_codigo	=	dw_1.Object.lote_codigo[il_fila]
//
//CHOOSE CASE columna
//	CASE "lote_codigo"
//		li_codigo	=	Integer(valor)
//
//END CHOOSE
//
//ll_fila	= dw_1.Find("lote_codigo = " + String(li_codigo), + &
//							1, dw_1.RowCount())
//
//IF ll_fila > 0 and ll_fila <> il_fila THEN
//	MessageBox("Error","Nº Lote ya fue ingresado anteriormente",Information!, Ok!)
//	RETURN True
//ELSE
	RETURN False
//END IF
//
end function

public function boolean existemovimiento ();Long	ll_Movto

SELECT	Count(*)
	INTO	:ll_Movto
	FROM	dbo.palletencab
	Using	Sqlca;
//	WHERE	clie_codigo = :gi_codexport;

If sqlca.sqlcode = -1 Then
	F_ErrorBaseDatos(sqlca,"No se pudo leer la tabla Encabezado de Pallet Granel")
	Return False
ElseIf ll_Movto = 0 Then
	Return False
Else
	Return True
End If

end function

protected function boolean noexistelotes (integer ai_valor, integer ai_tipo);Integer li_lote, li_especie, li_Planta, li_Contador
Long ll_fila, ll_Planilla
String ls_Nula
SetNull(ls_Nula)

li_especie	=	iuo_Especie.Codigo
li_Planta	    =	dw_2.Object.plde_codigo[1]
li_Lote		=	dw_2.Object.lote_codigo[1]

CHOOSE CASE ai_Tipo
	CASE 1
		li_especie	=	ai_Valor
	CASE 2
		li_Planta	=	ai_Valor
	CASE 3
		li_Lote		=	ai_Valor
END CHOOSE

SELECT	Count(*)
	INTO	:li_Contador
	FROM	dbo.spro_lotesfrutagranel
	WHERE	lote_pltcod = :li_Planta
	AND   lote_espcod = :li_especie
	AND   lote_codigo = :li_lote;

IF sqlca.sqlcode = -1 THEN
	F_ErrorBaseDatos(sqlca,"No se pudo leer la tabla spro_lotesfrutagranel")
	RETURN TRUE
ELSEIF li_Contador = 0 THEN
	Messagebox("Atención","Lote No Existe en Fruta Granel")
	RETURN TRUE
ELSE
	
	SELECT	Count(*)
		INTO	:li_Contador
		FROM	dbo.ctlcalrecepcionfrutasenca
		WHERE	plde_codigo = :li_Planta
		AND   espe_codigo = :li_especie
		AND   lote_codigo = :li_lote
		AND 	clie_codigo = :gi_CodExport;
	
	IF sqlca.sqlcode = -1 THEN
		F_ErrorBaseDatos(sqlca,"No se pudo leer la tabla de Recepcióm")
		RETURN TRUE
	ELSEIF li_Contador = 0 THEN	
		Messagebox("Atención","Lote No Existe en Planilla de Recepción")
		RETURN TRUE	
	ELSE
//		SELECT	ccpv_numero
//			INTO	:ll_Planilla
//			FROM	dbo.ctlcalplaniverifienca
//			WHERE	plde_codigo = :li_Planta
//			AND   espe_codigo = :li_especie
//			AND   lote_codigo = :li_lote
//			AND 	clie_codigo = :gi_CodExport;
//			
//		IF sqlca.sqlcode = -1 THEN
//			F_ErrorBaseDatos(sqlca,"No se pudo leer la tabla de Verificación")
//			RETURN TRUE
//		ELSEIF ll_Planilla > 0 THEN
//			Messagebox("Atención","Lote Digitado En Planilla " + String(li_Contador) + "")
	//		RETURN TRUE
//		ELSE	
			RETURN FALSE	
//		END IF
	END IF
END IF



end function

public function boolean trae_planilla (long al_planilla, integer ai_planta, integer ai_especie); Long    ll_existe, ll_productor, ll_lote, ll_Cliente
 Integer li_variedad, li_especie, li_zona, li_planta, Cliente
 
ll_Cliente = Long(istr_Mant.Argumento[7])
 
 ii_sw = 0 
 
   SELECT ccpv_numero,zona_codigo,prod_codigo,espe_codigo,   
	          vari_codigo,plde_codigo,cclo_numero,clie_codigo
    INTO :ll_existe,:li_zona,:ll_productor,:li_especie,   
         :li_variedad,:li_planta,:ll_lote,:Cliente
    FROM dbo.ctlcalplaniverifienca  
   WHERE clie_codigo = :ll_Cliente 
	 AND  plde_codigo = :ai_planta 
	 AND  ccpv_numero = :al_planilla  
	 AND  espe_codigo = :ai_especie;
	  
If sqlca.SQLCode = -1 Then
	 F_errorbasedatos(sqlca,"Lectura tabla Verificacion.")
	 Return False
ElseIf sqlca.sqlcode	=	0 Then
	If li_especie = dw_2.Object.espe_codigo[1] Then
		istr_mant.argumento[2] = String(ll_existe)
		istr_mant.argumento[3] = String(li_zona)
		istr_mant.argumento[4] = String(li_planta)
		istr_mant.argumento[1] = String(ll_lote)
		istr_mant.argumento[8] = String(ll_productor)
		istr_mant.argumento[7] = String(Cliente)
		ii_sw = 0
	Else
		ii_sw = 1
	END IF
	Return True 
Else
	Return False
End If
end function

public subroutine ingresoencab ();Boolean lb_Habilita

If iuo_Especie.Codigo <> 82 Then
	IF (IsNull(dw_2.Object.ccpv_numero[1]) OR dw_2.Object.ccpv_numero[1] = 0) OR &
		(IsNull(dw_2.Object.zona_codigo[1]) OR dw_2.Object.zona_codigo[1] = 0) OR &
		(IsNull(dw_2.Object.plde_codigo[1]) OR dw_2.Object.plde_codigo[1] = 0) OR &
		(IsNull(dw_2.Object.ccti_codigo[1]) OR dw_2.Object.ccti_codigo[1] = 0) OR &
		(IsNull(dw_2.Object.prod_codigo[1]) OR dw_2.Object.prod_codigo[1] = 0) OR &
		(IsNull(dw_2.Object.vari_codigo[1]) OR dw_2.Object.vari_codigo[1] = 0) OR &
		(IsNull(dw_2.Object.ccag_codigo[1]) OR dw_2.Object.ccag_codigo[1] = 0) OR &
		(IsNull(dw_2.Object.ccpv_nomver[1]) OR dw_2.Object.ccpv_nomver[1] = 0) OR &
		(IsNull(dw_2.Object.lote_codigo[1]) OR dw_2.Object.lote_codigo[1] = 0) AND &
		(IsNull(dw_2.Object.ccpv_nlote2[1]) OR dw_2.Object.ccpv_nlote2[1] = 0) AND &
		(IsNull(dw_2.Object.ccpv_nlote3[1]) OR dw_2.Object.ccpv_nlote3[1] = 0) AND &
		(IsNull(dw_2.Object.ccpv_nlote4[1]) OR dw_2.Object.ccpv_nlote4[1] = 0) AND &
		(IsNull(dw_2.Object.ccpv_nlote5[1]) OR dw_2.Object.ccpv_nlote5[1] = 0) THEN
		MessageBox("Atención","Faltan datos en encabezado")
		lb_Habilita = FALSE
	ELSE
		lb_Habilita = TRUE
	END IF
Else
	IF (IsNull(dw_2.Object.ccpv_numero[1]) OR dw_2.Object.ccpv_numero[1] = 0) OR &
		(IsNull(dw_2.Object.zona_codigo[1]) OR dw_2.Object.zona_codigo[1] = 0) OR &
		(IsNull(dw_2.Object.plde_codigo[1]) OR dw_2.Object.plde_codigo[1] = 0) OR &
		(IsNull(dw_2.Object.ccti_codigo[1]) OR dw_2.Object.ccti_codigo[1] = 0) OR &
		(IsNull(dw_2.Object.prod_codigo[1]) OR dw_2.Object.prod_codigo[1] = 0) OR &
		(IsNull(dw_2.Object.vari_codigo[1]) OR dw_2.Object.vari_codigo[1] = 0) OR &
		(IsNull(dw_2.Object.ccag_codigo[1]) OR dw_2.Object.ccag_codigo[1] = 0) OR &
		(IsNull(dw_2.Object.ccpv_nomver[1]) OR dw_2.Object.ccpv_nomver[1] = 0) THEN
		MessageBox("Atención","Faltan datos en encabezado")
		lb_Habilita = FALSE
	ELSE
		lb_Habilita = TRUE
	END IF	
End If

pb_ins_det.Enabled	=	lb_Habilita
pb_grabar.Enabled		=	lb_Habilita
end subroutine

public function boolean orden_proceso (integer ai_cliente, integer ai_planta, integer ai_tipo, long al_numero, integer ai_especie);Long	ll_Orden, ll_Bultos, ll_Fila
String	ls_lote,ls_numero,ls_campo

ll_Orden = ids_orden.Retrieve(ai_cliente,ai_planta,ai_tipo,al_numero,iuo_Especie.Codigo)

IF sqlca.sqlcode = -1 THEN
	F_ErrorBaseDatos(sqlca,"No se pudo leer la tabla spro_Ordenproceso")
	RETURN FALSE
ELSEIF ll_Orden = 0 THEN
	MessageBox("Atención","No existe Orden de Proceso. Ingrese otro número")
	RETURN FALSE
ELSEIF ll_Orden > 0 THEN
	dw_2.Object.orpr_tipord[1] = 4
	ll_Bultos	=	0
	
	For ll_Fila = 1 To ids_orden.RowCount()
		If ll_Fila = 1 Then
			dw_2.Object.lote_codigo[1] 	=	ids_orden.Object.lote_codigo[ll_Fila]
			dw_2.Object.ccpv_noplt1[1]	=	ids_orden.Object.lote_pltcod[ll_Fila]
			ll_Bultos								+=	ids_orden.Object.orpd_canbul[ll_Fila]
	   ElseIf ll_Fila = 2 Then
			dw_2.Object.ccpv_nlote2[1] 	=	ids_orden.Object.lote_codigo[ll_Fila]
			dw_2.Object.ccpv_noplt2[1]	=	ids_orden.Object.lote_pltcod[ll_Fila]
			ll_Bultos								+=	ids_orden.Object.orpd_canbul[ll_Fila]
		ElseIf ll_Fila = 3 Then
			dw_2.Object.ccpv_nlote3[1]	=	ids_orden.Object.lote_codigo[ll_Fila]
			dw_2.Object.ccpv_noplt3[1]	=	ids_orden.Object.lote_pltcod[ll_Fila]
			ll_Bultos								+=	ids_orden.Object.orpd_canbul[ll_Fila]
		ElseIf ll_Fila = 4 Then	
			dw_2.Object.ccpv_nlote4[1]	=	ids_orden.Object.lote_codigo[ll_Fila]
			dw_2.Object.ccpv_noplt4[1]	=	ids_orden.Object.lote_pltcod[ll_Fila]
			ll_Bultos								+=	ids_orden.Object.orpd_canbul[ll_Fila]
		ElseIf ll_Fila = 5 Then
			dw_2.Object.ccpv_nlote5[1]	=	ids_orden.Object.lote_codigo[ll_Fila]
			dw_2.Object.ccpv_noplt5[1]	=	ids_orden.Object.lote_pltcod[ll_Fila]
			ll_Bultos								+=	ids_orden.Object.orpd_canbul[ll_Fila]
		End If
	Next
	
	dw_2.Object.ccpv_numban[1]	=	ll_Bultos
	
	Return True
End If
end function

on w_maed_ctlcalplanillaespecies_auto.create
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

on w_maed_ctlcalplanillaespecies_auto.destroy
call super::destroy
destroy(this.dw_3)
destroy(this.cb_aprobar)
destroy(this.tab_1)
end on

event ue_seleccion;call super::ue_seleccion;String	ls_nula

SetNull(ls_nula)

istr_busq.argum[1]	=	String(dw_2.Object.plde_codigo[1])
istr_busq.argum[2]	=	String(iuo_Especie.Codigo)
istr_busq.argum[3]	=	String(dw_2.Object.clie_codigo[1])

OpenWithParm(w_busc_ctlcalplanillacerezas, istr_busq)
istr_busq = Message.PowerObjectParm

	
IF istr_busq.argum[3] <> '' THEN
	istr_mant.argumento[1] 	= istr_busq.argum[1]
	istr_mant.argumento[2] 	= istr_busq.argum[2]
	istr_mant.argumento[3] 	= istr_busq.argum[3]
	istr_mant.argumento[4] 	= istr_busq.argum[4]
	istr_mant.argumento[8] 	= istr_busq.argum[5]
	istr_mant.argumento[10] = istr_busq.argum[6]
	istr_mant.argumento[11] = istr_busq.argum[7]
	istr_mant.argumento[18] = istr_busq.argum[8]
	istr_mant.argumento[7] = istr_busq.argum[9]
	
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
	
	ll_fila_e	= dw_2.Retrieve(Integer(istr_Mant.Argumento[7]),Integer(istr_mant.argumento[4]), & 
										Long(istr_mant.argumento[2]),iuo_Especie.Codigo)

	HabilitaEncab(True)
	dw_2.Object.clie_codigo.Protect					=	1
	dw_2.Object.prod_codigo.Protect					=	1
	dw_2.Object.vari_codigo.Protect					=	1
	dw_2.Object.ccpv_numero.protect 				=	1            
	dw_2.Object.zona_codigo.protect 				=	1 
	dw_2.Object.plde_codigo.protect 				=	1
	dw_2.Object.clie_codigo.BackGround.Color		=	553648127
	dw_2.Object.prod_codigo.BackGround.Color	=	553648127
	dw_2.Object.vari_codigo.BackGround.Color	=	553648127
	dw_2.Object.ccpv_numero.BackGround.Color	=	553648127
	dw_2.Object.zona_codigo.BackGround.Color	=	553648127
	dw_2.Object.plde_codigo.BackGround.Color	=	553648127
	
	Tab_1.TabPage_1.dw_4.GetChild("ccag_codigo", idwc_agronomos)
	idwc_agronomos.Retrieve(Integer(istr_mant.argumento[3]))
										
	Tab_1.TabPage_1.dw_4.GetChild("prod_codigo", idwc_productores)
	idwc_productores.Retrieve(Integer(istr_mant.argumento[3]))
	
	istr_mant.argumento[10] = String(dw_2.Object.vari_codigo[1])
						
	If ll_fila_e = -1 Then
		respuesta = MessageBox("Error en Base de Datos", "No es posible conectar la Base de Datos.", Information!, RetryCancel!)
	Else
		Do		
			ll_fila_d	= dw_1.Retrieve(Integer(istr_Mant.Argumento[7]),Integer(istr_mant.argumento[4]),&
												Long(istr_mant.argumento[2]),iuo_Especie.Codigo)

			If ll_fila_d = -1 Then
				respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
												Information!, RetryCancel!)
			Else
				Do
					ll_fila_f	= dw_3.Retrieve(Integer(istr_Mant.Argumento[7]),Integer(istr_mant.argumento[4]),Long(istr_mant.argumento[1]))
					If ll_fila_f = -1 Then respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", Information!, RetryCancel!)

					If dw_3.Rowcount() > 0 Then
						istr_mant.Argumento[19]	=	String(dw_3.Object.cclo_tamlot[1])
						istr_mant.Argumento[12]	=	dw_3.Object.emba_codigo[1]

						pb_grabar.Enabled	=	True
						pb_eli_det.Enabled	=	True
						pb_imprimir.Enabled  =	True
						cb_aprobar.Enabled	=	True
						dw_2.Enabled			=	True
						dw_3.Enabled			=	True							

						dw_1.SetRow(1)
						dw_1.SelectRow(1,True)
						dw_1.SetFocus()	
						
						If istr_mant.argumento[14] = '2'Then
							dw_2.Enabled 				= False							
							dw_3.Enabled 				= False									
							istr_mant.Solo_Consulta	= True 											
						End If
					End If 	
				Loop While respuesta = 1
				pb_eliminar.Enabled	= True
				pb_grabar.Enabled	= True
				pb_ins_det.Enabled	= True
			End If
		If ll_fila_d > 0 Then
			pb_grabar.Enabled	=	True
			pb_eli_det.Enabled	=	True
			pb_imprimir.Enabled 	=	True
			cb_aprobar.Enabled	=	True
			dw_1.SetRow(1)
			dw_1.SelectRow(1,True)
			dw_1.SetFocus()
			dw_2.Enabled = True
			dw_1.Enabled = True		
			If istr_mant.argumento[14]	=	'2'	Then
				dw_2.Enabled 				= False
				dw_3.Enabled 				= False
				istr_mant.Solo_Consulta	= True
				pb_eliminar.Enabled 	=	False
				pb_grabar.Enabled 	=	False
				pb_imprimir.Enabled 	=	True
				pb_ins_det.Enabled 	=	False
				pb_eli_det.Enabled 	=	False
				cb_aprobar.Enabled	=	False
				HabilitaEncab(False)
			End If
		Else
			pb_ins_det.SetFocus()
		End If
			
		Loop While respuesta = 1

		If respuesta = 2 Then Close(This)
	End If
	dw_2.SetRedraw(True)
Loop While respuesta = 1

If respuesta = 2 Then Close(This)
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
//dw_2.SetColumn(0)
dw_2.Setcolumn("clie_codigo")

dw_3.SetRedraw(False)
dw_3.Reset()
dw_3.InsertRow(0)
dw_3.SetRedraw(True)

dw_2.SetItem(1, "clie_codigo",Integer(istr_mant.argumento[7]))
dw_2.SetItem(1, "espe_codigo",iuo_Especie.Codigo)
dw_2.SetItem(1, "vari_codigo",Integer(istr_mant.argumento[10]))
dw_2.SetItem(1, "ccpv_fecins",Date(istr_mant.argumento[5]))
dw_2.SetItem(1, "zona_codigo",gi_codzona)

dw_3.SetItem(1, "emba_codigo",istr_mant.argumento[12])

dw_2.GetChild("plde_codigo", idwc_plantas)
idwc_plantas.SetTransObject(sqlca)
idwc_plantas.Retrieve(1, -1)
idwc_plantas.SetSort("plde_nombre A")
idwc_plantas.Sort()
dw_2.SetItem(1, "plde_codigo",gi_CodPlanta)

dw_3.GetChild("plde_codpak", idwc_packings)
idwc_packings.SetTransObject(sqlca)
idwc_packings.Retrieve(2,0)
idwc_packings.SetSort("plde_nombre A")
idwc_packings.Sort()

dw_2.GetChild("prod_codigo", idwc_productores)
idwc_productores.SetTransObject(sqlca)
idwc_productores.Retrieve(dw_2.Object.zona_codigo[1])

dw_2.GetChild("ccag_codigo", idwc_agronomos)
idwc_agronomos.SetTransObject(sqlca)
idwc_agronomos.Retrieve(dw_2.Object.zona_codigo[1])

//dw_2.GetChild("cama_codigo", idwc_Camaras)
//idwc_Camaras.SetTransObject(sqlca)
//idwc_Camaras.Retrieve(gi_codPlanta)
//idwc_Camaras.SetSort("cama_nombre A")
//idwc_Camaras.Sort()

li_Grupo = BuscaGrupo(Upper(Gstr_Us.Nombre))

If (li_Grupo > 2) Then
	pb_eliminar.Visible	=	False
	TriggerEvent('resize')
End If

HabilitaEncab(True)
end event

event ue_nuevo_detalle;call super::ue_nuevo_detalle;Integer li_grupo
String ls_Usuario
istr_mant.borra		= False
istr_mant.agrega	= True

If iuo_Especie.Codigo = 21 Then
	OpenWithParm(iw_mantencion, istr_mant)
ElseIf iuo_Especie.Codigo = 41 	Then
	OpenWithParm(iw_mantencion1, istr_mant)
ElseIf iuo_Especie.Codigo = 26 Or iuo_Especie.Codigo = 36 Then
	OpenWithParm(iw_mantencion2, istr_mant)
ElseIf iuo_Especie.Codigo = 10 Then
	OpenWithParm(iw_mantencion7, istr_mant)
ElseIf iuo_Especie.Codigo = 78 Then
	OpenWithParm(iw_mantencion6, istr_mant)
ElseIf iuo_Especie.Codigo = 27 Then
	OpenWithParm(iw_mantencion8, istr_mant)
ElseIf iuo_Especie.Codigo = 81 Then
	OpenWithParm(iw_mantencion3, istr_mant)	
ElseIf iuo_Especie.Codigo = 82 Then
	OpenWithParm(iw_mantencion4, istr_mant)
ElseIf iuo_Especie.Codigo = 23 Then
	OpenWithParm(iw_mantencion5, istr_mant)
End If

If dw_1.RowCount() > 0 Then
	pb_grabar.Enabled		= True
	cb_aprobar.enabled	=	True
End If

 li_Grupo = BuscaGrupo(ls_Usuario)
 If li_Grupo	=	1 Or li_Grupo	=	6 Then
	 pb_eli_det.Enabled	= True
End If

dw_1.SetRow(il_fila)
dw_1.SelectRow(il_fila,True)
end event

event open;/*Argumentos
istr_mant.Argumento[1]	=	Número de Lote
istr_mant.Argumento[2]	=	Número de Planilla
istr_mant.Argumento[3]	=	Codigo Zona
istr_mant.argumento[4]	=	Código Planta
istr_mant.argumento[5]	=	Fecha de sistema
istr_mant.argumento[6]	=	Tipo Inspección
istr_mant.argumento[7]	=	Cliente
istr_mant.argumento[8]	=	Código Productor
istr_mant.argumento[9]	=	Código especie
istr_mant.argumento[10]	=	Código Variedad
istr_mant.argumento[11]	=	Código Agronomo
istr_mant.argumento[12]	=	Código Enbalaje
istr_mant.argumento[13]	=	Tamaño de Lote
istr_mant.argumento[14]	=	Estado de Planilla 1=Pendiente; 2=Cerrada
istr_mant.argumento[15]	=	Resolución Lote (encabezado de planilla)
istr_mant.argumento[16]	=	Código Packing
istr_mant.argumento[17]	=	Código Calibre
istr_mant.argumento[18]	=	Fecha Embalaje
istr_mant.argumento[19]  = 	Marca si genera lote	
istr_mant.argumento[12]	=	Etiqueta 
istr_mant.Argumento[22]	=	dw_1.Retrieve > 0 => 1 ; 0
*/
String	ls_Embalaje

x				= 0
y				= 0
This.Height	= 2020
im_menu		= m_principal

This.Icon										=	Gstr_apl.Icono
This.ParentWindow().ToolBarVisible	=	True
im_menu.Item[1].Item[6].Enabled		=	True
im_menu.Item[7].Visible					=	True

iuo_zonas       	   		= Create uo_zonas
iuo_plantas     	   		= Create uo_plantadesp
iuo_variedades  	   	= Create uo_variedades 
iuo_productor   	   	= Create uo_productores
iuo_agronomos   	   	= Create uo_ctlcalagronomos
iuo_ctlcaltipoinspec	= Create uo_ctlcaltipoinspec
iuo_ctlcalinspectores	= Create	uo_ctlcalinspectores
iuo_embalajesprod	= Create	uo_embalajesprod
iuo_Objetados			= Create uo_loteobjetadopendiente
iuo_Especie				= Create uo_Especie
iuo_Clientes				= Create uo_Cliente
iuo_Etiquetas			=	Create uo_Etiquetas

ids_orden = Create DataStore
ids_orden.dataObject = "dw_lote_ordenproceso"
ids_orden.SetTransObject(sqlca)

iuo_Especie.Existe(Integer(Message.StringParm), False, Sqlca)
iuo_Plantas.Existe(gi_codPlanta, False, Sqlca)

This.Title		= "PLANILLA DE VERIFICACIÓN DE : " + Upper(iuo_Especie.Nombre)
dw_1.Title	= 'DIGITACIÓN PLANILLA VERIFICACIÓN DE ' + Upper(iuo_Especie.Nombre)

dw_2.ShareData(Tab_1.TabPage_1.dw_4)
dw_2.ShareData(Tab_1.TabPage_2.dw_5)

Tab_1.TabPage_1.dw_4.GetChild("zona_codigo", idwc_zonas)
idwc_zonas.SetTransObject(sqlca)
IF idwc_zonas.Retrieve(0) = 0 THEN idwc_zonas.InsertRow(0)
idwc_zonas.SetSort("zona_nombre A")
idwc_zonas.Sort()
Tab_1.TabPage_1.dw_4.SetItem(1, "zona_codigo",gi_codZona)

Tab_1.TabPage_1.dw_4.GetChild("prod_codigo", idwc_productores)
idwc_productores.SetTransObject(sqlca)
idwc_productores.Retrieve(gi_codZona)
idwc_productores.SetSort("prod_nombre A")
idwc_productores.Sort()
Tab_1.TabPage_1.dw_4.SetItem(1, "prod_codigo",gi_codProductor)

Tab_1.TabPage_1.dw_4.GetChild("cate_codigo", idwc_categorias)
idwc_categorias.SetTransObject(sqlca)
idwc_categorias.Retrieve()
idwc_categorias.Sort()
Tab_1.TabPage_1.dw_4.SetItem(1, "cate_codigo",1)

Tab_1.TabPage_1.dw_4.GetChild("ccag_codigo", idwc_agronomos)
idwc_agronomos.SetTransObject(sqlca)
idwc_agronomos.Retrieve(gi_codZona)
idwc_agronomos.SetSort("ccag_nombre A")
idwc_agronomos.Sort()
	
Tab_1.TabPage_1.dw_4.GetChild("plde_codigo", idwc_plantas)
idwc_plantas.SetTransObject(sqlca)
idwc_plantas.Retrieve(1, -1)
idwc_plantas.SetSort("plde_nombre A")
idwc_plantas.Sort()
Tab_1.TabPage_1.dw_4.SetItem(1, "plde_codigo",gi_codPlanta)

Tab_1.TabPage_1.dw_4.GetChild("cama_codigo", idwc_Camaras)
idwc_Camaras.SetTransObject(sqlca)
idwc_Camaras.Retrieve(gi_codPlanta)
idwc_Camaras.SetSort("cama_nombre A")
idwc_Camaras.Sort()

Tab_1.TabPage_1.dw_4.GetChild("vari_codigo", idwc_variedades)
idwc_variedades.SetTransObject(sqlca)
idwc_variedades.Retrieve(iuo_Especie.Codigo,0)
idwc_variedades.SetSort("vari_nombre A")
idwc_variedades.Sort()
Tab_1.TabPage_1.dw_4.SetItem(1, "vari_codigo",gi_CodVariedad)

Tab_1.TabPage_1.dw_4.GetChild("ccpv_nomver", idwc_inspector)
idwc_inspector.SetTransObject(sqlca)
idwc_inspector.Retrieve()
idwc_inspector.SetSort("ccpv_nomver A")
idwc_inspector.Sort()

dw_3.GetChild("plde_codpak", idwc_packings)
idwc_packings.SetTransObject(sqlca)
idwc_packings.Retrieve(2, 0)
idwc_packings.SetSort("plde_nombre A")
idwc_packings.Sort()

Tab_1.TabPage_1.dw_4.GetChild("ccti_codigo", idwc_tipinspec)
idwc_tipinspec.SetTransObject(sqlca)
idwc_tipinspec.Retrieve()
idwc_tipinspec.SetSort("ccti_nombre A")
idwc_tipinspec.Sort()
Tab_1.TabPage_1.dw_4.SetITem(1,"ccti_codigo",1)

dw_3.GetChild("vaca_calibr", idwc_calibres)
idwc_calibres.SetTransObject(sqlca)
idwc_calibres.Retrieve(iuo_Especie.Codigo,0)
idwc_calibres.SetSort("vaca_calibr A")
idwc_calibres.Sort()

dw_3.GetChild("emba_codigo", idwc_embalajes)
idwc_embalajes.SetTransObject(sqlca)
idwc_embalajes.Retrieve(gi_CodExport)
idwc_embalajes.SetFilter("espe_codigo = " + String(iuo_Especie.Codigo))
idwc_embalajes.Filter()
idwc_embalajes.SetSort("emba_codigo A")
idwc_embalajes.Sort()

ls_Embalaje	= idwc_embalajes.GetItemString(1, 'emba_codigo')
dw_3.SetItem(1, "emba_codigo",ls_Embalaje)
istr_mant.argumento[12]	=	ls_Embalaje


Choose Case iuo_Especie.Codigo
	Case 21 //Cerezas
		dw_1.DataObject = 'dw_mues_ctlcalplanillacerezas'
		dw_3.GetChild("emba_codigo", idwc_embalajes)
	Case 41
		dw_1.DataObject 						= 'dw_mues_ctlcalplanillakiwis'
		dw_2.Object.ccpv_reslot.visible		=	True
		dw_2.Object.ccpv_estado.visible	=	False
		dw_2.Object.t_15.Visible 			=	True
		dw_2.Object.t_16.Visible 			=	True	
		dw_2.Object.ccpv_respri.visible	=	True		
		dw_2.Modify("ccpv_reslot.Y = 200")
		dw_2.Modify("t_15.Y = 200")
		dw_2.Modify("ccpv_respri.Y = 280")
		dw_2.Modify("t_16.Y = 280")
		Tab_1.TabPage_1.dw_4.Object.cama_codigo.visible	=	True
		Tab_1.TabPage_1.dw_4.Object.t_18.Visible 			=	True
	Case 10
		dw_1.DataObject 						= 'dw_mues_ctlcalplanillalimas'
		dw_2.Object.t_1.Visible				=	False
		dw_2.Object.t_6.Visible				=	False
		dw_2.Object.ccpv_reslot.visible		=	True
		dw_2.Object.ccpv_estado.visible	=	False
		dw_2.Object.t_15.Visible 			=	True
		dw_2.Object.t_16.Visible 			=	True
		dw_2.Object.ccpv_respri.visible	=	True
		Tab_1.TabPage_1.dw_4.Object.t_20.text = 'Lote'		
		Tab_1.TabPage_1.dw_4.Object.t_2.text = 'Inspector'		
		Tab_1.TabPage_1.dw_4.Object.t_14.text = 'N° Bins'	
	Case 26
		dw_1.DataObject 						= 'dw_mues_ctlcalplanillanaranjas'
		dw_2.Object.t_1.Visible				=	False
		dw_2.Object.t_6.Visible				=	False
		dw_2.Object.ccpv_reslot.visible		=	True
		dw_2.Object.ccpv_estado.visible	=	False
		dw_2.Object.t_15.Visible 			=	True
		dw_2.Object.t_16.Visible 			=	True
		dw_2.Object.ccpv_respri.visible	=	True
		Tab_1.TabPage_1.dw_4.Object.t_20.text = 'Lote'		
		Tab_1.TabPage_1.dw_4.Object.t_2.text = 'Inspector'		
		Tab_1.TabPage_1.dw_4.Object.t_14.text = 'N° Bins'				
	Case 36
		dw_1.DataObject 						= 'dw_mues_ctlcalplanillanaranjas'
		dw_2.Object.t_1.Visible				=	False
		dw_2.Object.t_6.Visible				=	False
		dw_2.Object.ccpv_reslot.visible		=	True
		dw_2.Object.ccpv_estado.visible	=	False
		dw_2.Object.t_15.Visible 			=	True
		dw_2.Object.t_16.Visible 			=	True
		dw_2.Object.ccpv_respri.visible	=	True		
		Tab_1.TabPage_1.dw_4.Object.t_20.text = 'Lote'		
		Tab_1.TabPage_1.dw_4.Object.t_2.text = 'Inspector'		
		Tab_1.TabPage_1.dw_4.Object.t_14.text = 'N° Bins'				
	Case 78
		dw_1.DataObject 						= 'dw_mues_ctlcalplanillalimones'
		dw_2.Object.t_1.Visible				=	False
		dw_2.Object.t_6.Visible				=	False
		dw_2.Object.ccpv_reslot.visible		=	True
		dw_2.Object.ccpv_estado.visible	=	False
		dw_2.Object.t_15.Visible 			=	True
		dw_2.Object.t_16.Visible 			=	True	
		dw_2.Object.ccpv_respri.visible	=	True
	Case 27
		dw_1.DataObject 						= 'dw_mues_ctlcalplanillanaranjas'
		dw_2.Object.t_1.Visible				=	False
		dw_2.Object.t_6.Visible				=	False
		dw_2.Object.ccpv_reslot.visible		=	True
		dw_2.Object.ccpv_estado.visible	=	False
		dw_2.Object.t_15.Visible 			=	True
		dw_2.Object.t_16.Visible 			=	True	
		dw_2.Object.ccpv_respri.visible	=	True
		Tab_1.TabPage_1.dw_4.Object.t_20.text = 'Lote'		
		Tab_1.TabPage_1.dw_4.Object.t_2.text = 'Inspector'		
		Tab_1.TabPage_1.dw_4.Object.t_14.text = 'N° Bins'				
	Case 81
		dw_1.DataObject = 'dw_mues_ctlcalplanillapaltas'
		
	Case 23
		dw_1.DataObject = 'dw_mues_ctlcalplanillaciruelas'
		
	Case 82
		dw_1.DataObject = 'dw_mues_ctlcalplanillagranados'
		dw_2.Object.t_13.Visible 			=	False
		dw_2.Object.t_19.Visible 			=	False
		dw_2.Object.orpr_numero.visible	=	False
		dw_2.Object.lote_codigo.visible	=	False
		dw_2.Object.ccpv_nlote2.visible	=	False
		dw_2.Object.ccpv_nlote3.visible	=	False
		dw_2.Object.ccpv_nlote4.visible	=	False
		dw_2.Object.ccpv_nlote5.visible	=	False
		dw_2.Object.t_14.x					=	dw_2.Object.t_13.x
		dw_2.Object.t_14.y					=	dw_2.Object.t_13.y
		dw_2.Object.ccpv_numban.x		=	dw_2.Object.orpr_numero.x
		dw_2.Object.ccpv_numban.y		=	dw_2.Object.orpr_numero.y
	Case Else
		dw_1.DataObject = 'dw_mues_ctlcalplanillaespecies'
		
End Choose

istr_mant.argumento[3]		=	String(gi_CodZona)
istr_mant.argumento[4]		=	String(gi_CodPlanta)
istr_mant.argumento[5]		=	String(Today())
istr_mant.argumento[7]		=	String(gi_CodExport)
istr_mant.argumento[8]		=	String(gi_CodProductor)
istr_mant.argumento[9]		=	String(iuo_Especie.Codigo)
istr_mant.argumento[10]	=	"1"
istr_mant.argumento[11]	=  '0'
istr_mant.argumento[18]	=	String(Today())
istr_mant.argumento[16]	=	'0'
istr_mant.argumento[17]	=	' '
istr_mant.argumento[22]	=	'0'

IF NOT ExisteMovimiento() AND gstr_parlote.codgen = 0 THEN
	istr_mant.argumento[19] = '1'
END IF

IF gstr_parlote.codgen = 1 OR istr_mant.argumento[19] = '1' THEN
	dw_3.Object.buscalote.Visible	=	False
ELSE
	dw_3.Object.buscalote.Visible	=	True
END IF


dw_1.Modify("datawindow.message.title='Error '+ is_titulo")
dw_1.SetRowFocusIndicator(Off!)

istr_mant.dw						=	dw_1
istr_Mant.dw2						=	dw_2

istr_mant.UsuarioSoloConsulta	=	OpcionSoloConsulta()
GrabaAccesoAplicacion(True, id_FechaAcceso, it_HoraAcceso, This.Title, "Acceso a Aplicación", 1)
pb_nuevo.PostEvent(Clicked!)

dw_1.SetTransObject(sqlca)
dw_2.SetTransObject(sqlca)
dw_3.SetTransObject(sqlca)
end event

event ue_modifica_detalle;If dw_1.RowCount()>0 Then
	istr_mant.Agrega = False
	istr_mant.Borra  = False	
	If iuo_Especie.Codigo = 21 Then//Cerezas
		OpenWithParm(iw_mantencion,istr_mant)
	ElseIf iuo_Especie.Codigo = 41 Then
		OpenWithParm(iw_mantencion1,istr_mant)
	ElseIf iuo_Especie.Codigo = 10 Then
		OpenWithParm(iw_mantencion7, istr_mant)
	ElseIf iuo_Especie.Codigo = 26 Or iuo_Especie.Codigo = 36 Then
		OpenWithParm(iw_mantencion2, istr_mant)
	ElseIf iuo_Especie.Codigo = 78 Then
		OpenWithParm(iw_mantencion6, istr_mant)
	ElseIf iuo_Especie.Codigo = 27 Then
		OpenWithParm(iw_mantencion8, istr_mant)
	ElseIf iuo_Especie.Codigo = 81 Then
		OpenWithParm(iw_mantencion3, istr_mant)	
	ElseIf iuo_Especie.Codigo = 82 Then
		OpenWithParm(iw_mantencion4, istr_mant)
	ElseIf iuo_Especie.Codigo = 23 Then
		OpenWithParm(iw_mantencion5, istr_mant)
	End If
End If

//If dw_1.GetItemStatus(il_Fila,0,Primary!) = DataModified! Or & 
//	dw_1.GetItemStatus(il_Fila,0,Primary!) = NotModified! Then
//	istr_mant.Argumento[22]	=	'1'
//End If
//
end event

event ue_borra_detalle;If dw_1.rowcount() < 1 Then Return

SetPointer(HourGlass!)

ib_borrar = True
w_main.SetMicroHelp("Validando la eliminación de detalle...")

Message.DoubleParm = 0

This.TriggerEvent ("ue_validaborrar_detalle")

If Message.DoubleParm = -1 Then Return

istr_mant.borra	= True
istr_mant.agrega	= False

If iuo_Especie.Codigo = 21 Then
	OpenWithParm(iw_mantencion, istr_mant)
ElseIf iuo_Especie.Codigo = 41 Then
	OpenWithParm(iw_mantencion1, istr_mant)
ElseIf iuo_Especie.Codigo = 26 Then
	OpenWithParm(iw_mantencion2, istr_mant)
ElseIf iuo_Especie.Codigo = 10 Then
	OpenWithParm(iw_mantencion7, istr_mant)
ElseIf iuo_Especie.Codigo = 78 Then
	OpenWithParm(iw_mantencion6, istr_mant)
ElseIf iuo_Especie.Codigo = 27 Then
	OpenWithParm(iw_mantencion8, istr_mant)
ElseIf iuo_Especie.Codigo = 81 Then
	OpenWithParm(iw_mantencion3, istr_mant)
ElseIf iuo_Especie.Codigo = 82 Then
	OpenWithParm(iw_mantencion4, istr_mant)
ElseIf iuo_Especie.Codigo = 23 Then
	OpenWithParm(iw_mantencion5, istr_mant)
End If

istr_mant = Message.PowerObjectParm

If istr_mant.respuesta = 1 Then
	If dw_1.DeleteRow(0) = 1 Then
		ib_borrar = False

		w_main.SetMicroHelp("Borrando Registro...")
		SetPointer(Arrow!)
	Else
		ib_borrar = False
		MessageBox(This.Title,"No se puede borrar actual registro.")
	End If
	If dw_1.RowCount() = 0 Then 
		pb_eli_det.Enabled = False
		pb_grabar.Enabled = False
	End If
End If

istr_mant.borra	 = False
end event

event ue_imprimir;SetPointer(HourGlass!)

Long		fila
DataWindowChild  dwc_packing

istr_info.titulo	=	"INFORME DE PLANILLA VERIFICACIÓN " + Upper(iuo_Especie.Nombre)
istr_info.copias	=	1
OpenWithParm(vinf,istr_info)

If iuo_Especie.Codigo = 21 Then	//Cerezas
	vinf.dw_1.DataObject	=	"dw_info_verificacion_especies"
	vinf.dw_1.Object.DataWindow.Zoom = 90
ElseIf iuo_Especie.Codigo = 23 Then	
	vinf.dw_1.DataObject	=	"dw_info_verificacion_ciruelas"
	vinf.dw_1.Object.DataWindow.Zoom = 76
ElseIf iuo_Especie.Codigo = 41 Then	
//	vinf.dw_1.DataObject	=	"dw_info_ctlcalverificakiwis"
	vinf.dw_1.DataObject	=	"dw_info_verificacion_kiwis"
	vinf.dw_1.Object.DataWindow.Zoom = 80
ElseIf iuo_Especie.Codigo = 27 Then
	vinf.dw_1.DataObject	=	"dw_info_verificacion_citricos"
	vinf.dw_1.Object.DataWindow.Zoom = 80
ElseIf iuo_Especie.Codigo = 26 Then
//	vinf.dw_1.DataObject	=	"dw_info_ctlcalverificanaranjas"
	vinf.dw_1.DataObject	=	"dw_info_verificacion_citricos"
	vinf.dw_1.Object.DataWindow.Zoom = 80
ElseIf iuo_Especie.Codigo = 10 Then
	vinf.dw_1.DataObject	=	"dw_info_verificacion_limas"
//	vinf.dw_1.Object.DataWindow.Zoom = 90
ElseIf iuo_Especie.Codigo = 78 Then
	vinf.dw_1.DataObject	=	"dw_info_ctlcalverificalimones"
	vinf.dw_1.Object.DataWindow.Zoom = 78
ElseIf iuo_Especie.Codigo = 81 Then
	vinf.dw_1.DataObject	=	"dw_info_ctlcalverificapaltas"
	vinf.dw_1.Object.DataWindow.Zoom = 82
ElseIf iuo_Especie.Codigo = 82 Then	
	//vinf.dw_1.DataObject	=	"dw_info_ctlcalverificagranados"
	vinf.dw_1.DataObject = "dw_info_verificacion_granados"
	vinf.dw_1.Object.DataWindow.Zoom = 75//86
End If	

vinf.dw_1.SetTransObject(sqlca)

If iuo_Especie.Codigo = 23 Then	
	fila	=	vinf.dw_1.Retrieve(Integer(istr_Mant.Argumento[7]), Integer(istr_mant.argumento[4]), Long(istr_mant.argumento[2]),iuo_Especie.Codigo, &
								-1, -1, -1, -1, -1, -1, -1, '*', '*', Date('19000101'), Today(), Date('19000101'), Today())
ElseIf iuo_Especie.Codigo = 21 Then
	fila	=	vinf.dw_1.Retrieve(Integer(istr_Mant.Argumento[7]), -1, Integer(istr_mant.argumento[4]), iuo_Especie.Codigo, -1, &
								Date('19000101'), Today(), -1, -1, -1, -1, -1, Date('19000101'), Today(), 'Z', '*', 'Z', &
								'Z', 'Z', 'Z', 'Z', 'Z', 0, 0, 0, Long(istr_mant.argumento[2]))
ElseIf iuo_Especie.Codigo = 82 Or iuo_Especie.Codigo = 41 Then
	fila = vinf.dw_1.Retrieve(Integer(istr_Mant.Argumento[7]), Integer(istr_mant.argumento[3]), Integer(istr_mant.argumento[4]), iuo_Especie.Codigo, -1,&
                             			 Date('19000101'), Today(), -1, -1, -1, -1, -1, Date('19000101'), Today(), 'Z', '*', 'Z','Z', 'Z', 'Z','Z', 'Z', 0,0,0, Long(istr_mant.argumento[2]))

ElseIf iuo_Especie.Codigo = 26 Or iuo_Especie.Codigo = 27 or iuo_Especie.Codigo = 10 Then
	fila = vinf.dw_1.Retrieve(Integer(istr_Mant.Argumento[7]), -1, Integer(istr_mant.argumento[4]), iuo_Especie.Codigo, -1,Date('19000101'), Today(), -1, -1, -1, -1, -1, Date('19000101'), &
									  Today(), 'Z', '*', 'Z','Z', 'Z', 'Z','Z', 'Z', 0,0,0, Long(istr_mant.argumento[2]))
Else
	fila	=	vinf.dw_1.Retrieve(Integer(istr_Mant.Argumento[7]), Integer(istr_mant.argumento[4]), Long(istr_mant.argumento[2]),iuo_Especie.Codigo)
End If

If fila	=	-1 Then
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ElseIf fila	=	0 Then
	MessageBox( "No Existe información", "No existe información para este informe.", &
					StopSign!, Ok!)
Else
	F_Membrete(vinf.dw_1)
	IF gs_Ambiente <> 'Windows' THEN
		F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
	END IF
	
End If
SetPointer(Arrow!)
end event

event resize;Integer	maximo, li_posic_x, li_posic_y, li_visible = 0

maximo	= dw_1.width

IF dw_2.width > maximo THEN maximo = dw_2.width

dw_2.x					= 37 + Round((This.WorkSpaceWidth() - 400 - dw_2.width) / 2, 0)
dw_2.y					= 37

Tab_1.x					=	dw_2.x
Tab_1.y					=	dw_2.y
Tab_1.Width			=	dw_2.Width + 5

dw_3.x					= 37 + Round((This.WorkSpaceWidth() - dw_3.width - 400) / 2, 0)
dw_3.y					= 57 + Tab_1.Height

dw_1.width				= This.WorkSpaceWidth() - 400
dw_1.x					= 37 + Round((This.WorkSpaceWidth() - 400 - dw_1.width) / 2, 0)
dw_1.y					= 64 + Tab_1.Height + dw_3.Height
dw_1.Height				= This.WorkSpaceHeight() - dw_1.y - 41

//gb_1.x 					= This.WorkSpaceWidth() - 310 
//gb_1.y 					= 5
//gb_1.width				= 275

li_posic_x				= This.WorkSpaceWidth() - 250
//li_posic_y				= gb_1.y + 88

IF pb_buscar.Visible THEN
	pb_buscar.x				= li_posic_x - 50
	pb_buscar.y				= li_posic_y + 20
//	pb_buscar.width		= 235
//	pb_buscar.height		= 195
	li_visible ++
	li_posic_y += 250
END IF

IF pb_nuevo.Visible THEN
	pb_nuevo.x				= li_posic_x - 50
	pb_nuevo.y				= li_posic_y + 20
//	pb_nuevo.width		= 235
//	pb_nuevo.height		= 195
	li_visible ++
	li_posic_y += 250
END IF

IF	pb_eliminar.Visible THEN
	pb_eliminar.x			= li_posic_x  - 50
	pb_eliminar.y			= li_posic_y + 30
//	pb_eliminar.width		= 235
//	pb_eliminar.height		= 195
	li_visible ++
	li_posic_y += 250
END IF

IF pb_grabar.Visible THEN
	pb_grabar.x				= li_posic_x - 50
	pb_grabar.y				= li_posic_y + 40
//	pb_grabar.width		= 235
//	pb_grabar.height		= 195
	li_visible ++
	li_posic_y += 250
END IF

IF pb_imprimir.Visible THEN
	pb_imprimir.x			= li_posic_x - 50
	pb_imprimir.y			= li_posic_y + 50
//	pb_imprimir.width		= 235
//	pb_imprimir.height	= 195
	li_visible ++
	li_posic_y += 250
END IF

IF pb_salir.Visible THEN
	pb_salir.x				= li_posic_x - 50
	pb_salir.y				= li_posic_y + 60
//	pb_salir.width			= 235
//	pb_salir.height			= 195
	li_visible ++
	li_posic_y += 250
END IF

//gb_1.height				= 180 * li_visible + 97 /*  (Según Botones Visibles)  */
//gb_2.x 					= gb_1.x
//gb_2.y 					= 1293
//gb_2.width				= 275
//gb_2.height				= 180 * 2 + 97 /*  (2 Botones)  */

pb_ins_det.x			= li_posic_x - 50
pb_ins_det.y			= pb_ins_det.y + 50
//pb_ins_det.y			= gb_2.y + 93
//pb_ins_det.width		= 235
//pb_ins_det.height		= 195

pb_eli_det.x			= li_posic_x - 50
pb_eli_det.y			= pb_ins_det.y + 250
//pb_eli_det.width		= 235
//pb_eli_det.height		= 195
end event

event ue_antesguardar;Integer  li_secuencia, li_planta, li_especie,li_variedad, li_packing, &
			li_loteplanilla, li_Zona, li_Inspeccion, li_Cliente
String	ls_embalaje, ls_calibre, ls_resolu
Long		ll_fila, ll_numero, ll_planilla, ll_noguia,ll_productor,ll_lote
Date		ld_fecemb

ll_numero	=	dw_2.Object.ccpv_numero[1]
li_planta		=	dw_2.Object.plde_codigo[1]
ll_productor	=	dw_2.Object.prod_codigo[1]
li_especie	=	dw_2.Object.espe_codigo[1]
li_variedad	=	dw_2.Object.vari_codigo[1]
ls_resolu		=	dw_2.Object.ccpv_reslot[1]
ls_embalaje	=	dw_3.Object.emba_codigo[1]
li_packing	=	dw_3.Object.plde_codpak[1]
ls_calibre	=	dw_3.Object.vaca_calibr[1]
ld_fecemb	=	dw_3.Object.cclo_fecemb[1]
li_Zona		=	dw_2.Object.zona_codigo[1]
li_inspeccion=	dw_2.Object.ccti_codigo[1]
li_Cliente		=	dw_2.Object.clie_codigo[1]

// bloqueo para grabación de tablas
UPDATE	dba.parempresa
	SET	empr_disco	=	:gstr_parempresa.empr_disco
 WHERE	empr_rutemp	=	:gstr_parempresa.empr_rutemp;	

/*Permite controlar la generación de nº de lote, si la opción es 1 se genera el lote.Esto
se controla en tablas\generación de lotes.*/
IF dw_2.GetItemStatus(1, 0, Primary!) = NewModified! THEN
	IF gstr_parlote.codgen = 1 OR istr_mant.argumento[19] = '1' THEN	
		ll_lote	=	NuevoLote(li_Cliente,li_Planta)
		istr_mant.Argumento[1]= String(ll_lote)
	END IF
ELSE
	istr_mant.Argumento[1]= String(dw_2.Object.cclo_numero[1])
END IF

IF li_Inspeccion = 2 THEN // Packing
	IF ls_resolu='A'	THEN
		dw_3.SetItem(1,"cclo_noguia",1)
	END IF
END IF

//IF gstr_parlote.codgen = 0 AND dw_2.object.lote_codigo <> 0 THEN
//	Messagebox("Atención","Lote generado por Producción") 
//END IF
			
istr_mant.Argumento[3]	=	String(li_zona)

dw_2.SetItem(1, "cclo_numero", Long(istr_mant.argumento[1]))
dw_2.setitem(1, "ccpv_estado", 1)/*estado pendiente*/
dw_3.SetItem(1, "cclo_numero", Long(istr_mant.argumento[1]))
dw_3.SetItem(1, "clie_codigo", Integer(istr_mant.argumento[7]))
dw_3.SetItem(1, "plde_codigo", Integer(istr_mant.argumento[4]))
dw_3.SetItem(1, "prod_codigo", Long(istr_mant.argumento[8]))
dw_3.SetItem(1, "espe_codigo", Integer(istr_mant.argumento[9]))
dw_3.SetItem(1, "vari_codigo", Integer(istr_mant.argumento[10]))
dw_3.SetItem(1, "cclo_numpla", Long(istr_mant.argumento[2]))

ll_Lote	=	Long(istr_mant.Argumento[1])

/*
Deja tomada la transacción hasta el próximo commit.
Así pueden guardar más de una persona a la vez.
*/
UPDATE	dba.ctlcalplaniverifideta
	SET	ccpv_numero=0
	WHERE	1=2;

SELECT	IsNull(Max(cpvd_secuen), 0)
	INTO	:li_Secuencia
	FROM	dba.ctlcalplaniverifideta
	WHERE	ccpv_numero	=  :ll_Numero 
	AND	plde_codigo =	:li_planta
	AND	clie_codigo	=	:li_Cliente
	AND   espe_codigo =  :iuo_Especie.Codigo;

// dw_1.GetItemStatus(ll_Fila, 0, Delete!)=DataModified!

FOR ll_Fila = 1 TO dw_1.RowCount()
	IF dw_1.GetItemStatus(ll_Fila, 0, Primary!) = NewModified! THEN
		li_Secuencia ++
		dw_1.Object.ccpv_numero[ll_Fila] =	ll_Numero
		dw_1.Object.clie_codigo[ll_Fila] 	=	li_Cliente
		dw_1.Object.plde_codigo[ll_Fila] 	=	li_Planta
		dw_1.Object.cpvd_secuen[ll_Fila] =	li_Secuencia
		dw_1.Object.espe_codigo[ll_Fila] 	=	iuo_Especie.Codigo		
	END IF
NEXT

end event

event ue_guardar;String	ls_Usuario
Integer	li_Grupo, li_Planta, li_Folio, li_Fila, li_ContAprob, li_TotReg
Long		ll_nropalletactual, ll_nropalletnuevo, ll_totlot, ll_ccajas, ll_cajasrevisadas, ll_ccajasfalt

ls_Usuario	=	Upper(Gstr_Us.Nombre)
istr_mant.borra	= 	False
istr_mant.agrega	= 	False
li_Grupo = BuscaGrupo(ls_Usuario)
IF ((li_Grupo = 6 OR li_grupo = 1)) OR dw_1.GetItemStatus(il_Fila, 0, Primary!) = NewModified!  THEN 		
	istr_mant.Solo_Consulta = False
END IF

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
	ll_nropalletnuevo = dw_1.Object.cpvd_numpal[li_Fila]

	IF ll_nropalletnuevo <> ll_nropalletactual THEN
		ll_nropalletactual  = dw_1.Object.cpvd_numpal[li_Fila]

		IF dw_1.Object.cpvd_respal[li_Fila] = 'A' THEN
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


	ll_ccajasfalt = dw_3.Object.cclo_tamlot[1] - ll_cajasrevisadas 
	
	ll_ccajas = ll_ccajasfalt + ll_ccajas
	
	tab_1.tabpage_1.dw_4.Object.ccpv_porapr[1] = (100 * ll_ccajas) /dw_3.Object.cclo_tamlot[1]
END IF

	
Message.DoubleParm = 0

OpenWithParm(w_mant_ctlcalplanillacereza_reslot, istr_mant)

li_Grupo = BuscaGrupo(ls_Usuario)
IF  li_Grupo	=	1 OR li_Grupo	=	6 THEN 
	HabilitaEncab(True)
	dw_2.Object.prod_codigo.Protect				=	1
	dw_2.Object.vari_codigo.Protect				=	1
	dw_2.Object.prod_codigo.BackGround.Color	=	553648127
	dw_2.Object.vari_codigo.BackGround.Color	=	553648127
	dw_2.Object.ccpv_numero.protect = 1            
	dw_2.Object.ccpv_numero.BackGround.Color	=	553648127
	dw_2.Object.plde_codigo.protect = 1             
	dw_2.Object.plde_codigo.BackGround.Color	=	553648127
ELSE
 	HabilitaEncab(False)
END IF 

IF Message.DoubleParm = -1 THEN RETURN

call super::ue_guardar
end event

event ue_validaborrar;call super::ue_validaborrar;Long		ll_verifica,  ll_NroLote
Integer	li_Cliente, li_Planta

li_Planta	= Integer(istr_mant.argumento[4])
li_Cliente	= Integer(istr_mant.argumento[7])
ll_NroLote	= Long(istr_mant.argumento[1])

IF Message.DoubleParm = 1 THEN
	/*
	Verifica existencia de Lote en otras planillas
	*/
	SELECT	"Count"(*)
		INTO	:ll_verifica
		FROM	dba.ctlcalplaniverifienca
		WHERE	clie_codigo	=	:li_Cliente
		AND	plde_codigo	=	:li_Planta
		AND	cclo_numero	=	:ll_NroLote ;
	/*
	Elimina Lotes
	*/
	IF ll_verifica = 0  THEN
		dw_3.DeleteRow(1)
	END IF
END IF
end event

event ue_deshace;call super::ue_deshace;//Integer li_lote, li_variedad,  li_tamlot, li_planta, li_especie, li_packing, li_estado, li_zona
//Long    ll_guia, ll_productor, ll_fila, ll_numpla
//Date    ld_fecemb
//String  ls_embalaje, ls_calibre
// 
//  SELECT plde_codigo,cclo_numero,prod_codigo,espe_codigo,   
//         vari_codigo,emba_codigo,vaca_calibr,plde_codpak,   
//         cclo_fecemb,cclo_tamlot,cclo_numpla,cclo_estado,   
//         cclo_noguia  
//    INTO :li_planta,:li_lote,:ll_productor,:li_especie,   
//         :li_variedad,:ls_embalaje,:ls_calibre,:li_packing,   
//         :ld_fecemb,:li_tamlot,:ll_numpla,:li_estado,:ll_guia  
//    FROM dba.ctlcallotes  
//   WHERE clie_codigo = :gi_codexport 
//	  AND plde_codigo = :ai_planta 
//	  AND cclo_numero = :al_lote 
//	  AND espe_codigo = :ai_especie;
// 
//IF sqlca.SQLCode = -1 THEN
//	F_errorbasedatos(sqlca,"Lectura tabla Ctlcallotes ")
//	RETURN FALSE
//ELSEIF sqlca.sqlcode	=	0 THEN
//	
//  SELECT zona_codigo  
//    INTO :li_zona  
//    FROM dba.plantadesp  
//   WHERE clie_codigo = :gi_codexport 
//	  AND plde_codigo = :ai_planta 
//	  AND plde_codpla = 1 ;
//
//	dw_2.SetItem(1, "plde_codigo", li_planta)	
//	dw_2.SetItem(1, "zona_codigo", li_zona)	
//	dw_2.SetItem(1, "prod_codigo", ll_productor)
//	dw_2.SetItem(1, "vari_codigo", li_variedad)
//	dw_3.SetItem(1, "cclo_fecemb", ld_fecemb)
//	dw_3.SetItem(1, "vaca_calibr", ls_calibre)
//	dw_3.SetItem(1, "cclo_tamlot", li_tamlot)
//	dw_3.SetItem(1, "plde_copack", li_packing)
//	dw_3.SetItem(1, "emba_codigo", ls_embalaje)		
//	dw_2.SetItem(1, "lote_codigo", li_lote)
//		
//
//	dw_2.GetChild("plde_codigo", idwc_plantas)
//	idwc_plantas.SetTransObject(sqlca)
//	idwc_plantas.Retrieve(gi_codexport,1, li_zona)
//	
//	dw_2.GetChild("ccag_codigo", idwc_agronomos)
//	idwc_agronomos.SetTransObject(sqlca)
//	idwc_agronomos.Retrieve(0,li_zona)
//	
//		
//	RETURN TRUE
//ELSE	
//	RETURN FALSE
//END IF
end event

event ue_borrar;IF dw_2.RowCount() < 1 THEN RETURN

If iuo_Objetados.Existe(dw_2.Object.clie_codigo[1], dw_2.Object.plde_codigo[1], &
							dw_3.Object.cclo_numero[1], dw_2.Object.ccpv_numero[1], False, SQLCA) Then 
	MessageBox('Error', 'No se puede eliminar la planilla: ' + String (dw_2.Object.ccpv_numero[1], '00000000') + &
				',~r~npor que tiene Lotes Objetados Pendiente.~r~nResolucion: ' + &
				String (iuo_Objetados.ccte_numero, '00000000'), StopSign!, OK!)
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

type dw_1 from w_mant_encab_deta_csd`dw_1 within w_maed_ctlcalplanillaespecies_auto
integer x = 50
integer y = 928
integer width = 4594
integer height = 1192
string title = "DIGITACIÓN PLANILLA VERIFICACIÓN DE CEREZAS"
string dataobject = "dw_mues_ctlcalplanillagranados"
boolean hscrollbar = false
boolean hsplitscroll = true
end type

event dw_1::sqlpreview;//
end event

type dw_2 from w_mant_encab_deta_csd`dw_2 within w_maed_ctlcalplanillaespecies_auto
integer x = 745
integer y = 4
integer width = 3205
integer height = 704
integer taborder = 10
string dataobject = "dw_maed_ctlcalplanillacerezas"
boolean maxbox = true
end type

event dw_2::itemchanged;String	ls_Columna, ls_Nula, ls_Usuario

ls_Usuario	=	Upper(Gstr_Us.Nombre)

SetNull(ls_Nula)

ls_Columna	=	dwo.Name

Choose Case ls_Columna		
	Case "zona_codigo"
		If iuo_zonas.existe(Integer(data),True, sqlca) Then
			dw_2.GetChild("ccag_codigo", idwc_agronomos)
			idwc_agronomos.Retrieve(Integer(Data))
			dw_2.SetItem(1, "ccag_codigo", Integer(ls_Nula))
			
			dw_2.GetChild("plde_codigo", idwc_plantas)
			idwc_plantas.Retrieve(1,-1)
			dw_2.SetItem(1, "plde_codigo", Integer(ls_Nula))
						
			dw_3.GetChild("plde_codpak", idwc_packings)
			idwc_packings.Retrieve(2, 0)
			dw_3.SetItem(1, "plde_codpak", Integer(ls_Nula))
			
			dw_2.GetChild("prod_codigo", idwc_productores)
			idwc_productores.Retrieve(Integer(data))
			dw_2.SetItem(1, "prod_codigo", Integer(ls_Nula))
			
			istr_mant.argumento[3]=data
			limpia_data()
			
		Else
			This.SetItem(1, ls_Columna, Integer(ls_nula))
			Return 1
		End If
		
	Case "plde_codigo"
		If iuo_plantas.existefrigo(Integer(data),True,sqlca) Then
			If Trae_Planilla(dw_2.Object.ccpv_numero[1],Integer(data),iuo_Especie.Codigo) Then
            If ii_sw = 0 Then
					dw_3.Object.buscalote.visible = FALSE
				   Parent.TriggerEvent("ue_recuperadatos")
			   Else
					Messagebox("Atención","Planilla Digitada existe para esta Especie, no puede ingresar",exclamation!)
					dw_2.Object.ccpv_numero.Protect				=	0
					dw_2.SetItem(1,"ccpv_numero",Integer(ls_Nula))
					dw_2.Object.buscalote.visible = True
				End If
				dw_2.SetColumn("ccpv_numero")
				dw_2.SetFocus()
			 ElseIf IsNull(dw_2.Object.cclo_numero[1]) OR dw_2.Object.cclo_numero[1] = 0 Then
				HabilitaEncab(True)
			 End If	
			 istr_mant.argumento[4]	=	data
		Else
			This.SetItem(1, ls_Columna, Integer(ls_nula))
			Return 1			
		End If	
			
	Case "ccpv_numero"
		If Trae_Planilla(Long(data),dw_2.Object.plde_codigo[1],iuo_Especie.Codigo) Then
			If ii_sw = 0 Then
				dw_3.Object.buscalote.visible = FALSE
				
				Parent.TriggerEvent("ue_recuperadatos")
			Else
				Messagebox("Atención","Planilla Digitada existe para esta Especie, no puede ingresar",exclamation!)
				dw_2.Object.ccpv_numero.Protect	=	0
				dw_2.SetItem(1,"ccpv_numero",Integer(ls_Nula))
				dw_3.Object.buscalote.visible 		= True
			End If
			dw_2.SetColumn("ccpv_numero")
			dw_2.SetFocus()
		ElseIf IsNull(dw_2.Object.cclo_numero[1]) OR dw_2.Object.cclo_numero[1] = 0 Then
			HabilitaEncab(True)
		End If
		
		istr_mant.argumento[2]	=	data			

	Case "ccti_codigo"	
		  If iuo_ctlcaltipoinspec.existe(Integer(data),True, sqlca) Then
			  istr_mant.argumento[6]	=	data
		  Else
			  This.SetItem(1, ls_Columna, Integer(ls_nula))
			  Return 1
		  End If
		  
	Case "prod_codigo"
		  If Not iuo_productor.existe(Long(data),dw_2.Object.zona_codigo[1],True, sqlca) Then
		     This.SetItem(1, ls_Columna, Long(ls_nula))
			  Return 1
		  Else
			  istr_mant.argumento[8]	=	data
			  limpia_data()
		  End If		  
		 			  			
	Case "vari_codigo"
		If iuo_Variedades.Existe(iuo_Especie.Codigo,Integer(data),True, sqlca) Then
			  istr_mant.argumento[10]	=	data 
			  dw_3.GetChild("vaca_calibr", idwc_calibres)
			  idwc_calibres.Retrieve(iuo_Especie.Codigo,Integer(data))
			  dw_3.SetItem(1, "vaca_calibr", ls_Nula)	
		  Else
			  This.SetItem(1, ls_Columna, Integer(ls_nula))
			  Return 1
			  
		  End If	
		  
				
	Case "lote_codigo","ccpv_nlote2","ccpv_nlote3","ccpv_nlote4","ccpv_nlote5"
		If NoExisteLotes(Long(data),3) Then
		  This.Setitem(1, ls_Columna,Integer(ls_Nula))
		  Return 1
		End If
		
	Case "cclo_numero"
		   istr_mant.argumento[1]	=	data
				
	Case "ccag_codigo"
		  If Not iuo_agronomos.ofp_recupera_ctlcalagronomos(sqlca,dw_2.Object.zona_codigo[1],Integer(data),True) Then
			  This.SetItem(1, ls_Columna, Integer(ls_nula))
			  Return 1	
		  Else
			  istr_mant.argumento[11]	=	data		
	     End If
		  
	 Case "ccpv_nomver"
       If Not iuo_ctlcalinspectores.existe(sqlca,Integer(data),True) Then
			This.SetItem(1, ls_Columna, Long(ls_nula))
		  	Return 1	
		End If	
	
   Case "orpr_numero" 
		If gstr_parlote.codgen = 0 AND istr_mant.argumento[19] <> "1" Then
			If Not orden_proceso(Integer(istr_mant.argumento[7]),Integer(istr_mant.argumento[4]),&
										  4,Long(Data),iuo_Especie.Codigo) Then
				This.SetItem(1,"orpr_numero",Long(ls_nula))
				This.SetItem(1,"lote_codigo",Integer(ls_nula))
				This.SetItem(1,"ccpv_nlote2",Integer(ls_nula))
				This.SetItem(1,"ccpv_nlote3",Integer(ls_nula))
				This.SetItem(1,"ccpv_nlote4",Integer(ls_nula))
				This.SetItem(1,"ccpv_nlote5",Integer(ls_nula))
				Return 1
			End If
	   End If
			
	Case "ccpe_fechin"
		istr_mant.argumento[5]	=	data

End Choose

habilitaingreso(ls_Columna)
end event

event dw_2::clicked;call super::clicked;String	ls_columna

ls_columna = dwo.name

CHOOSE CASE ls_columna
	CASE "loterecepcion"
		loterecepcion()
	
END CHOOSE
end event

type pb_nuevo from w_mant_encab_deta_csd`pb_nuevo within w_maed_ctlcalplanillaespecies_auto
integer x = 4736
integer y = 492
integer taborder = 0
end type

type pb_eliminar from w_mant_encab_deta_csd`pb_eliminar within w_maed_ctlcalplanillaespecies_auto
integer x = 4736
integer y = 672
integer taborder = 0
end type

type pb_grabar from w_mant_encab_deta_csd`pb_grabar within w_maed_ctlcalplanillaespecies_auto
integer x = 4736
integer y = 856
end type

type pb_imprimir from w_mant_encab_deta_csd`pb_imprimir within w_maed_ctlcalplanillaespecies_auto
integer x = 4736
integer y = 1032
end type

type pb_salir from w_mant_encab_deta_csd`pb_salir within w_maed_ctlcalplanillaespecies_auto
integer x = 4736
integer y = 1212
end type

type pb_ins_det from w_mant_encab_deta_csd`pb_ins_det within w_maed_ctlcalplanillaespecies_auto
integer x = 4741
integer y = 1504
integer taborder = 30
end type

type pb_eli_det from w_mant_encab_deta_csd`pb_eli_det within w_maed_ctlcalplanillaespecies_auto
integer x = 4741
integer y = 1676
integer taborder = 0
end type

type pb_buscar from w_mant_encab_deta_csd`pb_buscar within w_maed_ctlcalplanillaespecies_auto
integer x = 4736
integer y = 312
integer taborder = 0
end type

type dw_3 from datawindow within w_maed_ctlcalplanillaespecies_auto
integer x = 745
integer y = 700
integer width = 3205
integer height = 224
integer taborder = 20
boolean bringtotop = true
string dataobject = "dw_mues_ctlcallotes"
borderstyle borderstyle = stylelowered!
end type

event itemchanged;String	ls_Columna, ls_Nula
Integer	li_cont 
String	ls_Null

SetNull(ls_Nula)

ls_Columna	=	dwo.Name
dw_3.Object.cclo_numpla[1] = long(istr_mant.argumento[2])

Choose Case ls_Columna	
	Case "emba_codigo"
		If Not iuo_embalajesprod.existe(gi_CodExport,data,TRUE, sqlca) Then
			This.SetItem(1, ls_Columna, ls_Nula)			
			Return 1
		Else
			istr_mant.argumento[12]	=	data	
			ExisteNumeroLote(ls_columna, data)
		End If	
		
	Case "cclo_fecemb"
		If Date(istr_mant.Argumento[5]) < Date(Data) Then
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
		Else
			dw_3.Object.vaca_calibr.Protect = 0
			dw_3.Object.plde_codpak.Protect = 0
			dw_3.Object.cclo_tamlot.Protect = 0
			istr_mant.argumento[18]	=	data
		End If 	
		
	Case "vaca_calibr"
		istr_mant.argumento[17]	=	data

	Case "etiq_codigo"
		If iuo_Etiquetas.Existe(Integer(data),True, Sqlca) Then
			istr_mant.argumento[20]	=	data
		Else  	
			This.SetItem(1, ls_Columna, Long(ls_nula))
			Return 1			
		End If	
		
	Case "plde_codpak"
		If iuo_plantas.existepacking(Integer(data),TRUE,sqlca) Then
			istr_mant.argumento[16]	=	data
		Else  	
			This.SetItem(1, ls_Columna, Long(ls_nula))
			Return 1			
		End If
		
	Case "cclo_tamlot"	
		istr_mant.argumento[13]	=	Data		
		
End Choose 

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

Choose Case ls_columna
	Case "buscalote"
		BuscaLote()
		HabilitaIngreso(ls_Columna)
	
End Choose
end event

type cb_aprobar from commandbutton within w_maed_ctlcalplanillaespecies_auto
boolean visible = false
integer x = 4686
integer y = 56
integer width = 261
integer height = 112
integer taborder = 70
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "CERRAR"
end type

event clicked;Integer	li_Grupo 

String ls_Usuario
ls_Usuario	=	Upper(Gstr_Us.Nombre)

dw_2.SetItem(1,"ccpv_estado",2)
dw_3.SetItem(1,"cclo_tamlot",Integer(istr_mant.Argumento[13]))


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

type tab_1 from tab within w_maed_ctlcalplanillaespecies_auto
integer x = 745
integer y = 4
integer width = 3205
integer height = 704
integer taborder = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean italic = true
long backcolor = 30586022
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
integer width = 3168
integer height = 576
long backcolor = 12632256
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
integer x = 9
integer y = 8
integer width = 3154
integer height = 556
integer taborder = 11
boolean bringtotop = true
string dataobject = "dw_maed_ctlcalplanillacerezas_enca"
boolean vscrollbar = false
end type

event itemchanged;call super::itemchanged;String	ls_Columna, ls_Nula, ls_Usuario

ls_Usuario	=	Upper(Gstr_Us.Nombre)

SetNull(ls_Nula)

ls_Columna	=	dwo.Name
Choose Case ls_Columna		
	Case 'clie_codigo'
		If Not iuo_Clientes.Existe(Long(Data), True, Sqlca) Then
			This.SetItem(Row, ls_Columna, Integer(ls_Nula))
			Return 1
		Else
			istr_Mant.Argumento[7]	=	String(iuo_Clientes.Codigo)	
		End If
		
	Case "zona_codigo"
		If iuo_zonas.existe(Integer(data),True, sqlca) Then
			This.GetChild("ccag_codigo", idwc_agronomos)
			idwc_agronomos.Retrieve(Integer(Data))
			This.SetItem(1, "ccag_codigo", Integer(ls_Nula))
			
			This.GetChild("plde_codigo", idwc_plantas)
			idwc_plantas.Retrieve(1,-1)
			This.SetItem(1, "plde_codigo", Integer(ls_Nula))
						
			dw_3.GetChild("plde_codpak", idwc_packings)
			idwc_packings.Retrieve(2, 0)
			dw_3.SetItem(1, "plde_codpak", Integer(ls_Nula))
			
			This.GetChild("prod_codigo", idwc_productores)
			idwc_productores.Retrieve(Integer(data))
			This.SetItem(1, "prod_codigo", Integer(ls_Nula))
			
			istr_mant.argumento[3]=data
			limpia_data()
			
		Else
			This.SetItem(1, ls_Columna, Integer(ls_nula))
			Return 1
		End If
		
	Case "plde_codigo"
		If iuo_plantas.existefrigo(Integer(data),True,sqlca) Then
			If Trae_Planilla(This.Object.ccpv_numero[1],Integer(data),iuo_Especie.Codigo) Then
            		If ii_sw = 0 Then
					dw_3.Object.buscalote.visible = FALSE
				   	w_maed_ctlcalplanillacerezas.TriggerEvent("ue_recuperadatos")
			   	Else
					Messagebox("Atención","Planilla Digitada existe para esta Especie, no puede ingresar",exclamation!)
					This.Object.ccpv_numero.Protect				=	0
					This.SetItem(1,"ccpv_numero",Integer(ls_Nula))
					This.Object.buscalote.visible = True
				End If
				This.SetColumn("ccpv_numero")
				This.SetFocus()
			 ElseIf IsNull(This.Object.cclo_numero[1]) OR This.Object.cclo_numero[1] = 0 Then
				HabilitaEncab(True)
			 End If	
			 istr_mant.argumento[4]	=	data
		Else
			This.SetItem(1, ls_Columna, Integer(ls_nula))
			Return 1			
		End If	
			
	Case "ccpv_numero"
		If Trae_Planilla(Long(data),This.Object.plde_codigo[1],iuo_Especie.Codigo) Then
			If ii_sw = 0 Then
				dw_3.Object.buscalote.visible = FALSE
				
				w_maed_ctlcalplanillacerezas.TriggerEvent("ue_recuperadatos")
			Else
				Messagebox("Atención","Planilla Digitada existe para esta Especie, no puede ingresar",exclamation!)
				This.Object.ccpv_numero.Protect	=	0
				This.SetItem(1,"ccpv_numero",Integer(ls_Nula))
				dw_3.Object.buscalote.visible 		= True
			End If
			This.SetColumn("ccpv_numero")
			This.SetFocus()
		ElseIf IsNull(This.Object.cclo_numero[1]) OR This.Object.cclo_numero[1] = 0 Then
			HabilitaEncab(True)
		End If
		
		istr_mant.argumento[2]	=	data			

	Case "ccti_codigo"	
		  If iuo_ctlcaltipoinspec.existe(Integer(data),True, sqlca) Then
			  istr_mant.argumento[6]	=	data
		  Else
			  This.SetItem(1, ls_Columna, Integer(ls_nula))
			  Return 1
		  End If
		  
	Case "prod_codigo"
		  If Not iuo_productor.existe(Long(data),This.Object.zona_codigo[1],True, sqlca) Then
		     This.SetItem(1, ls_Columna, Long(ls_nula))
			  Return 1
		  Else
			  istr_mant.argumento[8]	=	data
			  limpia_data()
		  End If		  
		 			  			
	Case "vari_codigo"
		If iuo_Variedades.Existe(iuo_Especie.Codigo,Integer(data),True, sqlca) Then	
			  istr_mant.argumento[10]	=	data 
			  dw_3.GetChild("vaca_calibr", idwc_calibres)
			  idwc_calibres.Retrieve(iuo_Especie.Codigo,Integer(data))
			  dw_3.SetItem(1, "vaca_calibr", ls_Nula)	
		  Else
			  This.SetItem(1, ls_Columna, Integer(ls_nula))
			  Return 1
			  
		  End If	
		  
				
	Case "lote_codigo","ccpv_nlote2","ccpv_nlote3","ccpv_nlote4","ccpv_nlote5"
		If NoExisteLotes(Long(data),3) Then
		  This.Setitem(1, ls_Columna,Integer(ls_Nula))
		  Return 1
		End If
		
	Case "cclo_numero"
		   istr_mant.argumento[1]	=	data
				
	Case "ccag_codigo"
		  If Not iuo_agronomos.ofp_recupera_ctlcalagronomos(sqlca,This.Object.zona_codigo[1],Integer(data),True) Then
			  This.SetItem(1, ls_Columna, Integer(ls_nula))
			  Return 1	
		  Else
			  istr_mant.argumento[11]	=	data		
	     End If
		  
	 Case "ccpv_nomver"
       If Not iuo_ctlcalinspectores.existe(sqlca,Integer(data),True) Then
			This.SetItem(1, ls_Columna, Long(ls_nula))
		  	Return 1	
		End If	
	
   Case "orpr_numero" 
		If gstr_parlote.codgen = 0 AND istr_mant.argumento[19] <> "1" Then
			If Not orden_proceso(Integer(istr_mant.argumento[7]),Integer(istr_mant.argumento[4]),&
										  4,Long(Data),iuo_Especie.Codigo) Then
				This.SetItem(1,"orpr_numero",Long(ls_nula))
				This.SetItem(1,"lote_codigo",Integer(ls_nula))
				This.SetItem(1,"ccpv_nlote2",Integer(ls_nula))
				This.SetItem(1,"ccpv_nlote3",Integer(ls_nula))
				This.SetItem(1,"ccpv_nlote4",Integer(ls_nula))
				This.SetItem(1,"ccpv_nlote5",Integer(ls_nula))
				Return 1
			End If
	   End If
			
	Case "ccpv_fecins"
		istr_mant.argumento[5]	=	data
		     IF date(istr_mant.Argumento[5]) > date(f_fechahora()) THEN
                  MessageBox("ERROR"," La fecha de Inspección No Debe Ser Mayor a la Fecha de Digitación.", StopSign!, Ok!)
				Return 1		
			END IF
		

End Choose

habilitaingreso(ls_Columna)
end event

event buttonclicked;call super::buttonclicked;String	ls_boton

ls_boton = dwo.name

Choose Case ls_boton
	Case "loterecepcion"
		loterecepcion()
	
End Choose
end event

event itemerror;call super::itemerror;return 1
end event

type tabpage_2 from userobject within tab_1
integer x = 18
integer y = 112
integer width = 3168
integer height = 576
long backcolor = 12632256
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
integer width = 3154
integer height = 556
integer taborder = 11
string dataobject = "dw_maed_ctlcalplanillacerezas_obse"
boolean vscrollbar = false
end type

