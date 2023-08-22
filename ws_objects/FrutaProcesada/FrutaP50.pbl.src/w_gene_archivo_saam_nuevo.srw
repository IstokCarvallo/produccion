$PBExportHeader$w_gene_archivo_saam_nuevo.srw
forward
global type w_gene_archivo_saam_nuevo from window
end type
type uo_selplantas from uo_seleccion_plantas within w_gene_archivo_saam_nuevo
end type
type uo_selcliente from uo_seleccion_clientesprod within w_gene_archivo_saam_nuevo
end type
type dw_6 from datawindow within w_gene_archivo_saam_nuevo
end type
type dw_5 from datawindow within w_gene_archivo_saam_nuevo
end type
type cb_oper from uo_buscar within w_gene_archivo_saam_nuevo
end type
type em_operacion from editmask within w_gene_archivo_saam_nuevo
end type
type st_9 from statictext within w_gene_archivo_saam_nuevo
end type
type st_8 from statictext within w_gene_archivo_saam_nuevo
end type
type st_11 from statictext within w_gene_archivo_saam_nuevo
end type
type ddlb_1 from dropdownlistbox within w_gene_archivo_saam_nuevo
end type
type em_planilla from editmask within w_gene_archivo_saam_nuevo
end type
type dw_guia from datawindow within w_gene_archivo_saam_nuevo
end type
type cbx_pak from checkbox within w_gene_archivo_saam_nuevo
end type
type cbx_guia from checkbox within w_gene_archivo_saam_nuevo
end type
type st_7 from statictext within w_gene_archivo_saam_nuevo
end type
type em_despacho from editmask within w_gene_archivo_saam_nuevo
end type
type cbx_prod from checkbox within w_gene_archivo_saam_nuevo
end type
type cbx_cal from checkbox within w_gene_archivo_saam_nuevo
end type
type cbx_var from checkbox within w_gene_archivo_saam_nuevo
end type
type dw_2 from datawindow within w_gene_archivo_saam_nuevo
end type
type st_4 from statictext within w_gene_archivo_saam_nuevo
end type
type st_3 from statictext within w_gene_archivo_saam_nuevo
end type
type sle_mensa from singlelineedit within w_gene_archivo_saam_nuevo
end type
type em_fzarpe from editmask within w_gene_archivo_saam_nuevo
end type
type sle_nave from singlelineedit within w_gene_archivo_saam_nuevo
end type
type dw_1 from datawindow within w_gene_archivo_saam_nuevo
end type
type st_5 from statictext within w_gene_archivo_saam_nuevo
end type
type dw_3 from datawindow within w_gene_archivo_saam_nuevo
end type
type pb_salir from picturebutton within w_gene_archivo_saam_nuevo
end type
type pb_grabar from picturebutton within w_gene_archivo_saam_nuevo
end type
type st_1 from statictext within w_gene_archivo_saam_nuevo
end type
type dw_4 from datawindow within w_gene_archivo_saam_nuevo
end type
type st_6 from statictext within w_gene_archivo_saam_nuevo
end type
type st_2 from statictext within w_gene_archivo_saam_nuevo
end type
end forward

global type w_gene_archivo_saam_nuevo from window
integer width = 2578
integer height = 1692
boolean titlebar = true
boolean controlmenu = true
windowtype windowtype = response!
long backcolor = 16777215
string icon = "TABLA.ICO"
event ue_guardar ( )
event ue_buscar pbm_custom12
event ue_ordenar pbm_custom13
event ue_carga_detalle pbm_custom27
event ue_listo ( )
event ue_antesguardar pbm_custom75
event ue_seleccion pbm_custom17
event ue_imprimir pbm_custom03
event ue_genera_facturas pbm_custom04
uo_selplantas uo_selplantas
uo_selcliente uo_selcliente
dw_6 dw_6
dw_5 dw_5
cb_oper cb_oper
em_operacion em_operacion
st_9 st_9
st_8 st_8
st_11 st_11
ddlb_1 ddlb_1
em_planilla em_planilla
dw_guia dw_guia
cbx_pak cbx_pak
cbx_guia cbx_guia
st_7 st_7
em_despacho em_despacho
cbx_prod cbx_prod
cbx_cal cbx_cal
cbx_var cbx_var
dw_2 dw_2
st_4 st_4
st_3 st_3
sle_mensa sle_mensa
em_fzarpe em_fzarpe
sle_nave sle_nave
dw_1 dw_1
st_5 st_5
dw_3 dw_3
pb_salir pb_salir
pb_grabar pb_grabar
st_1 st_1
dw_4 dw_4
st_6 st_6
st_2 st_2
end type
global w_gene_archivo_saam_nuevo w_gene_archivo_saam_nuevo

type variables
str_mant 		istr_mant
str_busqueda	istr_busq
Date				id_FechaAcceso, id_fecha
Time		it_HoraAcceso
integer  	ii_var, ii_cal, ii_prod, ii_packing, ii_condicion, il_embarcador, ii_controlcorreo
String		is_archivo, is_tipoplanilla, is_correosaampuerto

DataWindowChild	idwc_planta
end variables

forward prototypes
protected function boolean wf_actualiza_db (boolean borrando)
public subroutine enviamail ()
public function boolean existeplanilla (long al_planilla)
public function boolean existeguia (integer cliente, integer planta, long numero, long planilla, string tipoplanilla)
public function boolean existeoperacion (string operacion)
public function boolean existearchivo (integer li_cliente, integer li_planta, long ll_guia, long al_planilla, string as_tipoplanilla, string as_instructivo)
public function boolean validadespacho (integer li_cliente, integer li_planta, long ll_guia, long al_planilla, string as_tipoplanilla, string as_instructivo)
public function boolean valida_datos ()
public function boolean obtiene_condicion (integer ai_cliente, integer ai_planta, long al_pallet)
public subroutine genera_saam ()
public subroutine genera_stembex ()
end prototypes

event ue_guardar();Integer	li_cliente

li_cliente	=	integer(istr_mant.argumento[1])

IF li_cliente = 81 THEN
	genera_saam()
ELSE	
	genera_stembex()
END IF	
end event

event ue_listo;w_main.SetMicroHelp("Listo")
SetPointer(Arrow!)

end event

protected function boolean wf_actualiza_db (boolean borrando);Boolean	lb_AutoCommit, lb_Retorno

//IF Not dw_2.uf_check_required(0) THEN RETURN False

//IF Not dw_1.uf_validate(0) THEN RETURN False

lb_AutoCommit		=	sqlca.AutoCommit
sqlca.AutoCommit	=	False

IF Borrando THEN
//	IF dw_1.Update(True, False) = 1 THEN
//		IF dw_2.Update(True, False) = 1 THEN
//			Commit;
//			
//			IF sqlca.SQLCode <> 0 THEN
//				F_ErrorBaseDatos(sqlca, This.Title)
//				
//				RollBack;
//			ELSE
//				lb_Retorno	=	True
//				
//				dw_1.ResetUpdate()
//				dw_2.ResetUpdate()
//			END IF
//		ELSE
//			F_ErrorBaseDatos(sqlca, This.Title)
//			
//			RollBack;
//		END IF
//	ELSE
//		F_ErrorBaseDatos(sqlca, This.Title)
//	END IF
ELSE
//	IF dw_2.Update(True, False) = 1 THEN
		IF dw_4.Update(True, False) = 1 THEN
			Commit;
			
			IF sqlca.SQLCode <> 0 THEN
				F_ErrorBaseDatos(sqlca, This.Title)
				
				RollBack;
			ELSE
				lb_Retorno	=	True
				
				//dw_1.ResetUpdate()
				//dw_2.ResetUpdate()
				dw_4.ResetUpdate()
			END IF
		ELSE
			F_ErrorBaseDatos(sqlca, This.Title)
			
			RollBack;
		END IF
//	ELSE
//		F_ErrorBaseDatos(sqlca, This.Title)
//	END IF
END IF

sqlca.AutoCommit	=	lb_AutoCommit

RETURN lb_Retorno
end function

public subroutine enviamail ();String		ls_Nombre, ls_NomReporte, ls_Archivo, ls_DirectorioAct, ls_asunto, ls_texto, ls_ErrorMsg, ls_correoaereo, ls_correomaritimo, ls_correoterrestre
Long			ll_Fila, ll_consignatario, ll_Archivo, ll_result
Boolean		lb_Existe
Integer		li_imprimio, li_planta
String		ls_planta, ls_correoplanta, ls_tipodespa, ls_correostembex
str_parms	lstr_parms

ls_planta = String(uo_SelPlantas.Codigo,'0000')
ii_controlcorreo = 0


SELECT plde_corplt,plde_corsam,plde_corter,plde_cormat 
INTO :ls_correoplanta,:ls_correoaereo,:ls_correoterrestre,:ls_correomaritimo
FROM dbo.plantadesp
WHERE plde_codigo=:uo_SelPlantas.Codigo;

	// Ejemplo:
	// Se debe llamar la funcion send_mail con los siguientes
	// parametros:
	//   - Smtp server
	//   - Originador del mail (From)
	//   - Destino (To)
	//   - Con Copia (CC)
	//   - Bcc.
	//   - Titulo del correo (subject)
	//   - Texto del mensaje
	//   - Lista de archivos adjuntos (Attach)
	//   - Mensaje de resultado ( si hay error)
	
	// NOTA:
	// Las direcciones de correo deben ir entre signo < y > siempre
	// Si son varias direcciones en alguno de los parametros deben ir
	// Separadas por punto y coma (;)

SetPointer(HourGlass!)

ls_NomReporte									=	is_archivo
lstr_parms.string_arg[1]					=	String(1)
lstr_parms.string_arg[2]					=	is_archivo
lstr_parms.string_arg[3]					=	String(1)

ll_Archivo = 1

lstr_parms.string_arg[ll_Archivo+3]		=	gs_disco+":\GeneradosSAAM\" + is_archivo //+ ".txt"

ls_tipodespa	=	Mid(is_archivo,12,1)

ls_asunto	 = "Envío Documento"
ls_asunto	 =	 lstr_parms.string_arg[2]+' - ' + ls_asunto
ls_texto		 =	 ls_texto + ': '+lstr_parms.string_arg[2]+' con fecha ' + String(Today(),'dd/mm/yyyy')+'.' 

IF Upper(ls_tipodespa) = 'M' THEN
	ls_correostembex = ls_correomaritimo
ELSEIF Upper(ls_tipodespa) = 'A' THEN
	ls_correostembex = ls_correoaereo
ELSEIF Upper(ls_tipodespa) = 'T' THEN
	ls_correostembex = ls_correoterrestre
END IF

//IF Upper(ls_tipodespa) = 'P' OR Upper(ls_tipodespa) = 'N' OR Upper(ls_tipodespa) = 'S' THEN
//	IF li_planta = 2 OR li_planta = 4 THEN
//		ll_result = send_mail("smtp.rioblanco.cl","<archivostembex"+ls_planta+"@rioblanco.cl>",ls_correoplanta,"","",ls_asunto,ls_texto,lstr_parms.string_arg[ll_Archivo+3],ls_ErrorMsg)
//	ELSEIF li_planta = 3 THEN
//		ll_result = send_mail("smtp.rioblanco.cl","<archivostembex"+ls_planta+"@rioblanco.cl>",ls_correoplanta,"","",ls_asunto,ls_texto,lstr_parms.string_arg[ll_Archivo+3],ls_ErrorMsg)
//	ELSE
//		ll_result = send_mail("smtp.rioblanco.cl","<archivostembex"+ls_planta+"@rioblanco.cl>",ls_correoplanta,"","",ls_asunto,ls_texto,lstr_parms.string_arg[ll_Archivo+3],ls_ErrorMsg)
//	END IF	
//END IF
	
IF ll_result < 0 THEN
	messagebox("Error en el envio de mail - Error No" + string(ll_result),ls_ErrorMsg+ "~r~nIntente enviar correo en forma manual")
	OpenWithParm(w_correo_archivo_saam, lstr_parms)
	ii_controlcorreo = 1
ELSE
	Messagebox("Aviso", "Envio de correo exitoso")
	ii_controlcorreo = 0
END IF

SetPointer(Arrow!)
end subroutine

public function boolean existeplanilla (long al_planilla);Integer	li_codexp, li_planta, li_cansel, li_tecnic
Date		ld_fecha
String	ls_numsel, ls_ubisel, ls_tratam, ls_contraparte
Long		ll_cont

li_codexp		=	Integer(istr_mant.argumento[1])
li_planta		=	Integer(istr_mant.argumento[2])

IF al_planilla <> 0 OR li_planta = 0 THEN

	SELECT Min(defe_fecdes)
		INTO	:ld_fecha
		FROM	dbo.DESPAFRIGOEN 
		WHERE	plde_codigo =	:li_planta
		AND	clie_codigo	=	:li_codexp
		AND	defe_plasag	=	:al_planilla;
				
	IF sqlca.SQLCode = -1 THEN
		F_ErrorBaseDatos(sqlca,"Lectura de la Tabla DESPAFRIGOEN")
		em_planilla.SetFocus()
		RETURN False
	ELSEIF sqlca.SQLCode = 100 THEN
		MessageBox("Atención", "No existe Planilla S.A.G. Indicado.~r~rIngrese otro Número.", &
						Exclamation!, Ok!)
		pb_grabar.Enabled	= False
		em_planilla.SetFocus()
		RETURN False
	ELSE
		
		SELECT count()
		INTO	:ll_cont
		FROM	dbo.DESPAFRIGOEN 
		WHERE	plde_codigo =	:li_planta
		AND	clie_codigo	=	:li_codexp
		AND	defe_plasag	=	:al_planilla;
		
		IF ll_cont = 0 THEN
			MessageBox("Atención", "No existe Planilla S.A.G. Indicado.~r~rIngrese otro Número.", &
						Exclamation!, Ok!)
			pb_grabar.Enabled	= False
			em_planilla.Text = ''
			em_planilla.SetFocus()
			RETURN False
		END IF	
		
		em_fzarpe.Text = String(ld_fecha)
		sle_nave.Text = 'Listo Para Generar'

		SELECT  defe_cansel, defe_numsel, defe_ubisel, defe_tratam, defe_tecnic
			INTO	:li_cansel,  :ls_numsel,  :ls_ubisel,  :ls_tratam,  :li_tecnic
			FROM	dbo.DESPAFRIGOEN 
			WHERE	plde_codigo =	:li_planta
			AND	clie_codigo	=	:li_codexp
			AND	defe_plasag	=	:al_planilla
			AND   defe_nturno =  :is_tipoplanilla
			AND   defe_fecdes =  :ld_fecha;				
		
		SELECT tecn_nombre+' '+tecn_apepat+' '+tecn_apemat
		   INTO  :ls_contraparte
		   FROM dbo.cargostecnicos
			WHERE tecn_codigo = :li_tecnic
			AND   plde_codigo = :li_planta;		
	
//		em_fecha_des.text		= String(ld_fecha)
		pb_grabar.Enabled	= True
		
		RETURN True
	END IF
ELSE
	MessageBox("Atención", "Faltan parámetros de búsqueda.~r~rIngreselos todos.", &
					Exclamation!, Ok!)
	RETURN False
END IF
end function

public function boolean existeguia (integer cliente, integer planta, long numero, long planilla, string tipoplanilla);Long li_Existe
	
  SELECT defe_guides  
    INTO :li_existe  
    FROM dbo.despafrigoen  
   WHERE clie_codigo = :cliente AND  
         plde_codigo = :planta  AND  
         defe_guides = :numero  AND
			defe_nturno = :tipoplanilla AND
			defe_plasag = :planilla;

	IF sqlca.SQLCode = -1 THEN
		F_ErrorBaseDatos(sqlca,"Lectura de la Tabla despafrigoen")
		em_despacho.SetFocus()
		RETURN False
	ELSEIF sqlca.SQLCode = 100 THEN
		MessageBox("Atención", "No existe Guía Despacho Indicada.~r~rIngrese otro Número.", &
						Exclamation!, Ok!)
		pb_grabar.Enabled	= False
		em_despacho.SetFocus()
		RETURN False
	ELSE
		pb_grabar.Enabled	= True
		RETURN True
	END IF



end function

public function boolean existeoperacion (string operacion);Integer	li_codexp,li_puerto
String	ls_nave, ls_embarque
Date		ld_fzarpe

li_codexp			=	uo_SelCliente.Codigo
ls_embarque	=	operacion

IF ls_embarque <> "" THEN
	
	SELECT	embq_nomnav, embq_fzarpe, embq_ptoori, embc_codigo
		INTO	:ls_nave, :ld_fzarpe, :li_puerto, :il_embarcador
		FROM	dbo.EMBARQUEPROD
		WHERE	clie_codigo	=	:li_codexp
		AND	embq_codigo =	:ls_embarque ;
				
	IF sqlca.SQLCode = -1 THEN
		F_ErrorBaseDatos(sqlca,"Lectura de la Tabla EMBARQUEPROD")
		em_operacion.SetFocus()
		RETURN False
	ELSEIF sqlca.SQLCode = 100 THEN
		MessageBox("Atención", "No existe Instructivo Indicada.~r~rIngrese otro Número.", &
						Exclamation!, Ok!)
		pb_grabar.Enabled	= False
		em_operacion.SetFocus()
		RETURN False
	ELSE
		SELECT puer_correo
		INTO :is_correosaampuerto
		FROM dbo.puertos 
		WHERE puer_codigo = :li_puerto;
		
		sle_nave.text		= ls_nave
		em_fzarpe.text		= String(ld_fzarpe)
		id_fecha				= ld_fzarpe
		pb_grabar.Enabled	= True
		RETURN True
	END IF
ELSE
	MessageBox("Atención", "Faltan parámetros de búsqueda.~r~rIngreselos todos.", &
					Exclamation!, Ok!)
	RETURN False
END IF
end function

public function boolean existearchivo (integer li_cliente, integer li_planta, long ll_guia, long al_planilla, string as_tipoplanilla, string as_instructivo);Date ld_fecha
Time lt_hora
Integer li_contador, li_code_gengde
Long ll_numero
String	ls_embarque

//ls_embarque = String(em_operacion.Text)

SELECT defe_numero
INTO	:ll_numero
FROM	dbo.DESPAFRIGOEN 
WHERE	plde_codigo =	:li_planta
AND	clie_codigo	=	:li_cliente
AND	defe_guides	=	:ll_guia
AND   :al_planilla in (-1,defe_plasag)
AND	:as_tipoplanilla in ('-1',defe_nturno)
AND   embq_codigo =  :as_instructivo;

IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca,"Lectura de la Tabla DESPAFRIGOEN")
	RETURN True
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "No existe Número Guia Indicado.~r~rIngrese otro Número.", &
					Exclamation!, Ok!)
	RETURN True
END IF

SELECT count(),max(CODE_FECHAA),max(CODE_HORAAP)
INTO :li_contador,:ld_fecha,:lt_hora
FROM dbo.CONTROLDESPACHOS
where plde_codigo =	:li_planta
AND	clie_codigo	=	:li_cliente
AND	defe_numero	=	:ll_numero;

IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca,"Lectura de la Tabla CONTROLDESPACHOS")
	RETURN True
END IF

IF li_contador > 0 THEN
	
	Select CODE_GEMSAA 
	INTO :li_code_gengde
	from dbo.CONTROLDESPACHOS
	where  plde_codigo =	:li_planta
		AND	clie_codigo =	:li_cliente
		AND	defe_numero =	:ll_numero
		AND	CODE_FECHAA =  :ld_fecha
		AND	CODE_HORAAP = 	:lt_hora;
		
	IF li_code_gengde = 1 THEN
		MessageBox("Atención", "El Archivo S.A.A.M ya Fue Generado.", &
					Exclamation!, Ok!)
		Return True
	END IF		
END IF
Return FALSE




	
end function

public function boolean validadespacho (integer li_cliente, integer li_planta, long ll_guia, long al_planilla, string as_tipoplanilla, string as_instructivo);Long ll_numero
Integer li_contador, li_count, li_code_gengde, li_code_gemide, li_code_demade
Date ld_fecha
Time lt_hora
String	ls_embarque

//ls_embarque = String(em_operacion.Text)

SELECT defe_numero
INTO	:ll_numero
FROM	dbo.DESPAFRIGOEN 
WHERE	plde_codigo =	:li_planta
AND	clie_codigo	=	:li_cliente
AND	defe_guides	=	:ll_guia
AND   defe_nturno = :as_tipoplanilla
AND	defe_plasag = :al_planilla
AND   embq_codigo = :as_instructivo;

IF sqlca.SQLCode = -1 THEN
		F_ErrorBaseDatos(sqlca,"Lectura de la Tabla DESPAFRIGOEN")
		RETURN True
	ELSEIF sqlca.SQLCode = 100 THEN
		MessageBox("Atención", "No existe Número Guia Indicado.~r~rIngrese otro Número.", &
						Exclamation!, Ok!)
		RETURN True
END IF

SELECT count(),max(CODE_FECHAA),max(CODE_HORAAP)
INTO :li_contador,:ld_fecha,:lt_hora
FROM dbo.CONTROLDESPACHOS
where plde_codigo =	:li_planta
AND	clie_codigo	=	:li_cliente
AND	defe_numero	=	:ll_numero;

IF sqlca.SQLCode = -1 THEN
		F_ErrorBaseDatos(sqlca,"Lectura de la Tabla CONTROLDESPACHOS")
		RETURN True
END IF

IF li_contador > 0 THEN
	Select CODE_GENGDE 
	INTO :li_code_gengde
	from dbo.CONTROLDESPACHOS
	where  plde_codigo =	:li_planta
		AND	clie_codigo =	:li_cliente
		AND	defe_numero =	:ll_numero
		AND	CODE_FECHAA =  :ld_fecha;
		
	IF Isnull(li_code_gengde) OR li_code_gengde = 0 THEN
		MessageBox("Atención", "Falta Generar Informe de Guía de Despacho.", &
					Exclamation!, Ok!)
		Return True
	END IF		
		
END IF	

IF li_contador > 0 THEN
	Select CODE_GEMIDE 
	INTO :li_code_gemide
	from dbo.CONTROLDESPACHOS
	where  plde_codigo =	:li_planta
		AND	clie_codigo =	:li_cliente
		AND	defe_numero =	:ll_numero
		AND	CODE_FECHAA =  :ld_fecha;
		
	IF Isnull(li_code_gemide) OR li_code_gemide = 0 THEN
		MessageBox("Atención", "Falta Generar Impresión de Despacho.", &
					Exclamation!, Ok!)
		Return True
	END IF		
	
END IF	

IF li_contador > 0 THEN
	Select CODE_DEMADE 
	INTO :li_code_demade
	from dbo.CONTROLDESPACHOS
	where  plde_codigo =	:li_planta
		AND	clie_codigo =	:li_cliente
		AND	defe_numero =	:ll_numero
		AND	CODE_FECHAA =  :ld_fecha;
	
	IF Isnull(li_code_demade) OR li_code_demade = 0 THEN
		MessageBox("Atención", "Falta Generar Listado Anexo de Despacho.", &
					Exclamation!, Ok!)
		Return True
	END IF		
	
END IF	

return False			
			

end function

public function boolean valida_datos ();Long ll_fila, ll_cont, ll_pallet
String	ls_mensaje
Integer	li_cliente, li_planta

li_cliente 	= Integer(istr_mant.argumento[1])
li_planta	= Integer(istr_mant.argumento[2])

ls_mensaje = ''

FOR ll_fila = 1 TO dw_5.RowCount()
	
	ll_pallet = dw_5.Object.paen_numero[ll_fila]
	
	SELECT Count()
	INTO :ll_cont
	FROM dbo.palletfruta
	WHERE paen_numero = :ll_pallet
	AND clie_codigo = :li_cliente
	AND plde_codigo = :li_planta
	AND (isnull(PAFR_CUART1,0) = 0
	OR isnull(PAFR_CUART4,0) = 0
	OR isnull(PAFR_HUERT1,0) = 0
	OR isnull(PAFR_HUERT4,0) = 0);
	
	IF ll_cont > 0 THEN
		ls_mensaje = ls_mensaje + String(ll_Pallet)+','
	END IF	
	
//	SELECT Count()
//	INTO :ll_cont
//	FROM dbo.palletfruta
//	WHERE paen_numero = :ll_pallet
//	AND (isnull(PAFR_CUART4,0) = 0;
//	
//	IF ll_cont > 0 THEN
//		ls_mensaje = ls_mensaje + String(ll_Pallet)+','
//	END IF	
//	
//	SELECT Count()
//	INTO :ll_cont
//	FROM dbo.palletfruta
//	WHERE paen_numero = :ll_pallet
//	AND isnull(PAFR_HUERT1,0) = 0;
//	
//	IF ll_cont > 0 THEN
//		ls_mensaje = ls_mensaje + String(ll_Pallet)+','
//	END IF	
//	
//	SELECT Count()
//	INTO :ll_cont
//	FROM dbo.palletfruta
//	WHERE paen_numero = :ll_pallet
//	AND isnull(PAFR_HUERT4,0) = 0;
//	
//	IF ll_cont > 0 THEN
//		ls_mensaje = ls_mensaje + String(ll_Pallet)+','
//	END IF	
NEXT	

IF ls_mensaje <> '' THEN
	MessageBox("Atención", "Pallet "+String(ls_mensaje)+" con Predio o Cuartel Nulo.~r~rArregle por Movimientos -> Mantención -> Mantención Predio Cuartel Detalle Pallet.", &
						Exclamation!, Ok!)
	Return True
END IF



Return False

	
end function

public function boolean obtiene_condicion (integer ai_cliente, integer ai_planta, long al_pallet);Long li_Existe
Date ld_fecha

	ii_condicion = 0
	
  SELECT max(fumi_fecfum)  
    INTO :ld_fecha
    FROM dbo.fumigadet as det,dbo.fumigaenc as enc 
   WHERE det.clie_codigo = :ai_cliente AND  
         det.plde_codigo = :ai_planta  AND  
         det.paen_numero = :al_pallet AND
			det.clie_codigo = enc.clie_codigo AND
			det.plde_codigo = enc.plde_codigo AND
			det.cond_codigo = enc.cond_codigo AND
			det.fumi_numero = enc.fumi_numero
	GROUP BY det.cond_codigo;
	
	
	SELECT det.cond_codigo  
    INTO :ii_condicion 
    FROM dbo.fumigadet as det,dbo.fumigaenc as enc 
   WHERE det.clie_codigo = :ai_cliente AND  
         det.plde_codigo = :ai_planta  AND  
         det.paen_numero = :al_pallet AND
			det.clie_codigo = enc.clie_codigo AND
			det.plde_codigo = enc.plde_codigo AND
			det.cond_codigo = enc.cond_codigo AND
			det.fumi_numero = enc.fumi_numero AND
			enc.fumi_fecfum = :ld_fecha;
	

	IF sqlca.SQLCode = -1 THEN
		F_ErrorBaseDatos(sqlca,"Lectura de la Tabla fumigadet")
		//em_despacho.SetFocus()
		RETURN True
	ELSEIF sqlca.SQLCode = 100 THEN
//		MessageBox("Atención", "No existe Guía Despacho Indicada.~r~rIngrese otro Número.", &
//						Exclamation!, Ok!)
//		pb_grabar.Enabled	= False
//		em_despacho.SetFocus()
		RETURN True
	ELSE
//		pb_grabar.Enabled	= True
		RETURN False
	END IF





Return False
end function

public subroutine genera_saam ();Long			ll_fila, ll_filas, ll_filadet, ll_guia, ll_numero, ll_cont, ll_planilla, ll_filcont
String		ls_Cliente, ls_Planta, ls_Patente, ls_Archivo, ls_Registro, &
				ls_termog, ls_guia, ls_Instructivo
double		ll_termog
Integer		li_copallet, li_cliente, li_planta, li_Tipova
Datetime    ldt_hora

ldt_hora			=	Datetime(Date(Today()),Time(Today())	)

dw_2.DataObject = "dw_info_archivo_saam_packing_nuevo"
dw_1.DataObject = "dw_gene_archivo_saam_plano_nuevo"
dw_4.DataObject = "dw_gene_archivo_saam_estado_nuevo"

dw_2.reset()
dw_1.reset()
dw_4.reset()

dw_1.SetTransObject(Sqlca)
dw_2.SetTransObject(Sqlca)

// Para actualizar el estado=1 Cerrado, una vez generado el archivo
dw_4.SetTransObject(SQLCA)
dw_guia.SetTransObject(SQLCA)

IF cbx_guia.CheCked THEN
	ll_guia = -1
	ls_guia =  String(00000000, '00000000')
ELSE
	ll_guia = Long(em_despacho.text)
	ls_guia = String(ll_guia, '00000000')
END IF

ls_Cliente		=	String(Integer(istr_mant.Argumento[1]), '000')
ls_Planta		=	String(Integer(istr_mant.Argumento[2]), '0000')
ls_Instructivo	=	em_operacion.text
ll_planilla	=  Long(em_planilla.text)
ls_Archivo	=  "SAAM" +ls_Instructivo+"-"+ string(ll_planilla) + "." + Mid(ls_Planta, 2, 3) + ls_guia
is_archivo	=	ls_Archivo

ll_filcont		=  dw_5.Retrieve(Integer(istr_mant.argumento[1]),Integer(istr_mant.argumento[2]),&
										ll_planilla,ii_var,ii_cal,ii_prod,ll_guia, ii_packing, is_tipoplanilla,ls_Instructivo)
IF ll_filcont > 0 THEN
	IF valida_datos() THEN
		Return
	END IF	
END IF	

ll_filas		=  dw_2.Retrieve(Integer(istr_mant.argumento[1]),Integer(istr_mant.argumento[2]),&
										ll_planilla,ii_var,ii_cal,ii_prod,ll_guia, ii_packing, is_tipoplanilla,ls_Instructivo)
									  
li_cliente = Integer(istr_mant.argumento[1])

IF ll_filas = -1 THEN
	F_ErrorBaseDatos(sqlca,"Lectura de la Tabla EMBARQUEPROD")
ELSEIF ll_filas = 0 THEN
	MessageBox("Atención", "No hay información con Operación Indicada.~r~rIngrese otra Operación.", &
					Exclamation!, Ok!)
	pb_grabar.Enabled	= False
ELSE
	dw_2.SetSort('paen_numero')
   dw_2.Sort()

	FOR ll_fila = 1 TO ll_filas
		ls_Patente	=	String(dw_2.Object.defe_patent[ll_fila])
		
		IF IsNull(ls_Patente) THEN ls_Patente = ''
		
		ls_Registro	=	dw_2.Object.embq_codigo[ll_fila]
		ls_Registro	+=	String(dw_2.Object.defe_guides[ll_fila], '00000000')
		ls_Registro	+=	String(dw_2.Object.reci_codigo[ll_fila], '00000')
		ls_Registro	+=	String(dw_2.Object.plde_codigo[ll_fila], '0000')
		ls_Registro	+=	ls_Patente + Fill(' ',6 - Len(ls_Patente))
		ls_Registro	+=	String(dw_2.Object.espe_codigo[ll_fila], '00')
				
		Obtiene_condicion(dw_2.Object.clie_codigo[ll_fila],dw_2.Object.plde_codigo[ll_fila],dw_2.Object.paen_numero[ll_fila])
				
		IF ii_condicion = 1 THEN
			ls_Registro	+=	'F'
		ELSEIF dw_2.Object.paen_inspec[ll_fila] = 0 AND ii_condicion <> 1 AND ii_condicion <> 2  THEN
			ls_Registro	+=	'N'
		ELSEIF dw_2.Object.paen_inspec[ll_fila] = 1 THEN
			ls_Registro += 'I'
		ELSEIF ii_condicion = 2 THEN
			ls_Registro	+=	'U'
		END IF	
		
		ls_Registro	+=	ls_Cliente
		ls_Registro	+=	String(dw_2.Object.paen_numero[ll_fila], '0000000')
		ls_Registro	+=	String(dw_2.Object.etiq_codigo[ll_fila], '000')
		ls_Registro	+=	String(dw_2.Object.paen_fecemb[ll_fila], 'ddmmyy')
		ls_Registro	+=	String(dw_2.Object.prod_codigo[ll_fila], '00000')
		ls_Registro	+=	String(dw_2.Object.emba_codigo[ll_fila], '@@@@@@@@@@')
		ls_Registro	+=	String(dw_2.Object.vari_codigo[ll_fila], '0000')
		ls_Registro	+=	String(dw_2.Object.pafr_calibr[ll_fila], '@@@')
		ls_Registro	+=	String(dw_2.Object.pafr_ccajas[ll_fila], '0000000')
		ls_Registro	+=	String(dw_2.Object.cate_codigo[ll_fila], '000')
		
		IF dw_2.Object.tica_codigo[ll_fila] = 1 THEN
			ls_Registro	+=	'P'
		ELSEIF dw_2.Object.tica_codigo[ll_fila] = 2 THEN
			ls_Registro	+=	'C'
		ELSE
			ls_Registro	+=	'F'
		END IF
		
		IF dw_2.Object.embq_ptoori[ll_fila] = 901 THEN
			ls_Registro	+=	'06'
		ELSEIF dw_2.Object.embq_ptoori[ll_fila] = 902 THEN
			ls_Registro	+=	'01'
		ELSEIF dw_2.Object.embq_ptoori[ll_fila] = 903 THEN
			ls_Registro	+=	'02'
		ELSEIF dw_2.Object.embq_ptoori[ll_fila] = 904 THEN
			ls_Registro	+=	'03'
		ELSEIF (dw_2.Object.embq_ptoori[ll_fila] = 905 OR dw_2.Object.embq_ptoori[ll_fila] = 900 OR dw_2.Object.embq_ptoori[ll_fila] = 930 OR dw_2.Object.embq_ptoori[ll_fila] = 900) THEN
			ls_Registro	+=	'04'
		ELSEIF dw_2.Object.embq_ptoori[ll_fila] = 906 THEN
			ls_Registro	+=	'05'
		ELSE
			ls_Registro	+=	'04'
		END IF
		ls_Registro +=String(dw_2.Object.packing[ll_fila], '0000')

		IF IsNull(dw_2.Object.defe_termog[ll_fila]) THEN
			ll_termog = 0
			ls_termog = '0'
		ELSE
			ls_termog = String(dw_2.Object.defe_termog[ll_fila])
		END IF
	   ls_Registro +=String(ls_termog, Fill("@", 15))
		
		IF IsNull(dw_2.Object.copa_codigo[ll_fila]) THEN
			li_copallet	= 	0
		ELSE
			li_copallet =	dw_2.Object.copa_codigo[ll_fila]
		END IF
	   ls_Registro +=String(li_copallet, Fill("0", 3))
		
		ll_filadet	=	dw_1.InsertRow(0)
		dw_1.Object.registro[ll_filadet]	=	ls_Registro

		IF existearchivo(Integer(istr_mant.argumento[1]),Integer(istr_mant.argumento[2]),dw_2.Object.defe_guides[ll_fila],&
			Long(em_planilla.Text),is_tipoplanilla,ls_Instructivo) THEN
			Return
		END IF
		
		IF validadespacho(Integer(istr_mant.argumento[1]),Integer(istr_mant.argumento[2]),dw_2.Object.defe_guides[ll_fila],&
			Long(em_planilla.Text),is_tipoplanilla,ls_Instructivo) THEN
			Return
		END IF
	
	NEXT
	
	IF dw_1.SaveAs(gs_disco+":\GeneradosSAAM\" + ls_Archivo, Text!, False) = -1 THEN
		MessageBox("Atención","No se pudo generar el archivo " + ls_Archivo)
	ELSE
		enviamail()
		
		sle_mensa.text	= "Archivo " + ls_Archivo + " Generado. Avise a Computación"
		
		ll_filas		=  dw_4.Retrieve(Integer(istr_mant.argumento[1]), &
									  Integer(istr_mant.argumento[2]),ll_planilla,ll_guia,is_tipoplanilla,ls_Instructivo)
									  
		IF ll_filas = -1 THEN
			F_ErrorBaseDatos(sqlca,"Lectura de la Tabla DESPAFRIGOEN")
		ELSEIF ll_filas = 0 THEN
			MessageBox("Atención", "No hay información ", &
							Exclamation!, Ok!)
			pb_grabar.Enabled	= False
		ELSE		
			FOR ll_fila	= 1 TO ll_filas
				dw_4.SetItem(ll_fila,'defe_estado',1)
				ll_numero  = dw_4.Object.defe_numero[ll_fila]
				li_cliente = dw_4.Object.clie_codigo[ll_fila]
				li_planta  = dw_4.Object.plde_codigo[ll_fila]
				
				Update dbo.CONTROLDESPACHOS Set
				CODE_GEMSAA = 1,
				CODE_FECSAA = :ldt_hora
				WHERE clie_codigo = :li_cliente
				AND	plde_codigo = :li_planta
				AND	defe_numero = :ll_numero;
				commit;
		   NEXT
			wf_actualiza_db(False)
		END IF
	END IF
END IF

em_operacion.SetFocus()

MESSAGEBOX("ok","EJECUTADO SIN PROBLEMAS")
end subroutine

public subroutine genera_stembex ();Long			ll_fila, ll_filas, ll_filadet, ll_guia, ll_numero, ll_cont, ll_planilla, ll_filcont, ll_pallet, ll_palletdes, ll_despacho,&
				ll_existe, ll_fiborra
String		ls_Cliente, ls_Planta, ls_Patente, ls_Archivo, ls_Registro, ls_estdespacho, ls_paen_estdes, &
				ls_termog, ls_guia, ls_Instructivo
double		ll_termog
Integer		li_copallet, li_cliente, li_planta, li_Tipova
Datetime    ldt_hora

ldt_hora			=	Datetime(Date(Today()),Time(Today())	)

dw_2.DataObject = "dw_info_archivo_stembex_packing_nuevo"
dw_1.DataObject = "dw_gene_archivo_saam_plano_nuevo"
dw_4.DataObject = "dw_gene_archivo_saam_estado_nuevo"

dw_2.reset()
dw_1.reset()
dw_4.reset()

dw_1.SetTransObject(Sqlca)
dw_2.SetTransObject(Sqlca)

// Para actualizar el estado=1 Cerrado, una vez generado el archivo
dw_4.SetTransObject(SQLCA)
dw_guia.SetTransObject(SQLCA)

IF cbx_guia.CheCked THEN
	ll_guia = -1
	ls_guia =  String(00000000, '00000000')
ELSE
	ll_guia = Long(em_despacho.text)
	ls_guia = String(ll_guia, '00000000')
END IF

ls_Cliente		=	String(Integer(istr_mant.Argumento[1]), '000')
ls_Planta		=	String(Integer(istr_mant.Argumento[2]), '0000')
ls_Instructivo	=	em_operacion.text
ll_planilla	=  Long(em_planilla.text)

IF ll_planilla = 0 THEN
	ll_planilla = -1
	is_tipoplanilla = '-1'
END IF	

ls_Archivo	=  "STEMBEX" +ls_Instructivo+"-"+ string(ll_planilla) + "." + Mid(ls_Planta, 2, 3) + ls_guia
is_archivo	=	ls_Archivo

ll_filcont		=  dw_5.Retrieve(Integer(istr_mant.argumento[1]),Integer(istr_mant.argumento[2]),&
										ll_planilla,ii_var,ii_cal,ii_prod,ll_guia, ii_packing, is_tipoplanilla,ls_Instructivo)
IF ll_filcont > 0 THEN
//	IF valida_datos() THEN
//		Return
//	END IF	
END IF	

ll_filas		=  dw_2.Retrieve(Integer(istr_mant.argumento[1]),Integer(istr_mant.argumento[2]),&
										ll_planilla,ii_var,ii_cal,ii_prod,ll_guia, ii_packing, is_tipoplanilla,ls_Instructivo)
									  
li_cliente = Integer(istr_mant.argumento[1])

IF ll_filas = -1 THEN
	F_ErrorBaseDatos(sqlca,"Lectura de la Tabla EMBARQUEPROD")
ELSEIF ll_filas = 0 THEN
	MessageBox("Atención", "No hay información con Operación Indicada.~r~rIngrese otra Operación.", &
					Exclamation!, Ok!)
	pb_grabar.Enabled	= False
ELSE
	dw_2.SetSort('paen_numero')
   dw_2.Sort()

	FOR ll_fila = 1 TO ll_filas
		ls_Patente	=	String(dw_2.Object.defe_patent[ll_fila])
		
		IF IsNull(ls_Patente) THEN ls_Patente = ''
		
		ls_Registro	=	dw_2.Object.embq_codigo[ll_fila]
		ls_Registro	+=	String(dw_2.Object.defe_guides[ll_fila], '00000000')
		ls_Registro	+=	String(dw_2.Object.reci_codigo[ll_fila], '00000')
		ls_Registro	+=	String(dw_2.Object.plde_codigo[ll_fila], '0000')
		ls_Registro	+=	String(mid(ls_Patente,1,6),'@@@@@@')			//ls_Patente + Fill(' ',6 - Len(ls_Patente))
		ls_Registro	+=	String(dw_2.Object.espe_codigo[ll_fila], '00')
				
		//Obtiene_condicion(dw_2.Object.clie_codigo[ll_fila],dw_2.Object.plde_codigo[ll_fila],dw_2.Object.paen_numero[ll_fila])
		
		ls_Registro	+=	String(dw_2.Object.cond_codigo[ll_fila], '0')//String(ii_condicion,'0')
				
//		IF ii_condicion = 1 THEN
//			ls_Registro	+=	'F'
//		ELSEIF dw_2.Object.paen_inspec[ll_fila] = 0 AND ii_condicion <> 1 AND ii_condicion <> 2  THEN
//			ls_Registro	+=	'N'
//		ELSEIF dw_2.Object.paen_inspec[ll_fila] = 1 THEN
//			ls_Registro += 'I'
//		ELSEIF ii_condicion = 2 THEN
//			ls_Registro	+=	'U'
//		END IF	
		
		ls_Registro	+=	ls_Cliente
		ls_Registro	+=	String(dw_2.Object.paen_numero[ll_fila], '0000000')
		ll_pallet 	 = dw_2.Object.paen_numero[ll_fila]
		ls_Registro	+=	String(dw_2.Object.etiq_codigo[ll_fila], '000')
		ls_Registro	+=	String(dw_2.Object.paen_fecemb[ll_fila], 'ddmmyy')
		ls_Registro	+=	String(dw_2.Object.prod_codigo[ll_fila], '00000')
		ls_Registro	+=	String(dw_2.Object.emba_codigo[ll_fila], '@@@@@@@@@@')
		ls_Registro	+=	String(dw_2.Object.vari_codigo[ll_fila], '0000')
		ls_Registro	+=	String(dw_2.Object.pafr_calibr[ll_fila], '@@@')
		ls_Registro	+=	String(dw_2.Object.pafr_ccajas[ll_fila], '0000000')
		ls_Registro	+=	String(dw_2.Object.cate_codigo[ll_fila], '000')
		
		ls_Registro	+=	String(dw_2.Object.tica_codigo[ll_fila],'0')
		
//		IF dw_2.Object.tica_codigo[ll_fila] = 1 THEN
//			ls_Registro	+=	'P'
//		ELSEIF dw_2.Object.tica_codigo[ll_fila] = 2 THEN
//			ls_Registro	+=	'C'
//		ELSE
//			ls_Registro	+=	'F'
//		END IF
		
//		IF dw_2.Object.embq_ptoori[ll_fila] = 901 THEN
//			ls_Registro	+=	'06'
//		ELSEIF dw_2.Object.embq_ptoori[ll_fila] = 902 THEN
//			ls_Registro	+=	'01'
//		ELSEIF dw_2.Object.embq_ptoori[ll_fila] = 903 THEN
//			ls_Registro	+=	'02'
//		ELSEIF dw_2.Object.embq_ptoori[ll_fila] = 904 THEN
//			ls_Registro	+=	'03'
//		ELSEIF (dw_2.Object.embq_ptoori[ll_fila] = 905 OR dw_2.Object.embq_ptoori[ll_fila] = 900 OR dw_2.Object.embq_ptoori[ll_fila] = 930 OR dw_2.Object.embq_ptoori[ll_fila] = 900) THEN
//			ls_Registro	+=	'04'
//		ELSEIF dw_2.Object.embq_ptoori[ll_fila] = 906 THEN
//			ls_Registro	+=	'05'
//		ELSE
//			ls_Registro	+=	'04'
//		END IF
//		
		ls_Registro	+=	String(dw_2.Object.embq_ptoori[ll_fila], '000')
		ls_Registro += String(dw_2.Object.packing[ll_fila], '0000')

		IF IsNull(dw_2.Object.defe_termog[ll_fila]) THEN
			ll_termog = 0
			ls_termog = '0'
		ELSE
			ls_termog = String(dw_2.Object.defe_termog[ll_fila])
		END IF
	   ls_Registro +=String(ls_termog, Fill("@", 15))
		
		IF IsNull(dw_2.Object.copa_codigo[ll_fila]) THEN
			li_copallet	= 	0
		ELSE
			li_copallet =	dw_2.Object.copa_codigo[ll_fila]
		END IF
	   ls_Registro +=String(li_copallet, Fill("0", 3))
		
		ls_Registro += String(dw_2.Object.paen_inspec[ll_fila],'0')
		
//		IF dw_2.Object.paen_inspec[ll_fila] = 1 THEN
//			ls_Registro += '1'
//		ELSE	
//			ls_Registro += '0'
//		END IF	
		li_planta = Integer(istr_mant.Argumento[2])
						
		SELECT isnull(paen_estdes,'')
		INTO :ls_estdespacho
		FROM dbo.palletencab
		WHERE clie_codigo = :li_cliente
		AND plde_codigo = :li_planta
		AND paen_numero = : ll_pallet; 
		COMMIT;
		
		IF ls_estdespacho = '' THEN
			ls_paen_estdes = 'A'
			
			UPDATE dbo.palletencab SET
			paen_estdes = :ls_paen_estdes
			WHERE clie_codigo = :li_cliente
			AND plde_codigo = :li_planta
			AND paen_numero = : ll_pallet;
			COMMIT;
			
		ELSEIF ls_estdespacho = 'M' THEN	
			ls_paen_estdes = 'M'
		ELSEIF ls_estdespacho = 'A' THEN	
			ls_paen_estdes = 'A'	
		ELSEIF ls_estdespacho = 'E' THEN	
			ls_paen_estdes = ' '		
		END IF
					
		ls_Registro += "A"//ls_paen_estdes
		
		ls_Registro += String(dw_2.Object.defe_nrcont[ll_fila],'@@@@@@@@@@@@@@@@@@@@')
		ls_Registro += String(dw_2.Object.listasellos[ll_fila],'@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@')
		ls_Registro += String(dw_2.Object.defe_chofer[ll_fila],'@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@')
		ls_Registro += String(dw_2.Object.defe_celcho[ll_fila],'@@@@@@@@@@@@@@@')
		ls_Registro += String(dw_2.Object.defe_pataco[ll_fila],'@@@@@@@@@@@@@@@')
		ls_Registro += String(dw_2.Object.hora[ll_fila],'00')
		ls_Registro += String(dw_2.Object.minutos[ll_fila],'00')
		ls_Registro += String(dw_2.Object.puer_codigo[ll_fila],'000')
		
		ll_filadet	=	dw_1.InsertRow(0)
		dw_1.Object.registro[ll_filadet]	=	ls_Registro
		
//		IF existearchivo(Integer(istr_mant.argumento[1]),Integer(istr_mant.argumento[2]),dw_2.Object.defe_guides[ll_fila],&
//			Long(ll_planilla),is_tipoplanilla,ls_Instructivo) THEN
//			Return
//		END IF
//		
//		IF validadespacho(Integer(istr_mant.argumento[1]),Integer(istr_mant.argumento[2]),dw_2.Object.defe_guides[ll_fila],&
//			Long(ll_planilla),is_tipoplanilla,ls_Instructivo) THEN
//			Return
//		END IF
	NEXT
	
	dw_6.SetTransObject(Sqlca)
	ll_despacho =	dw_2.Object.defe_numero[1]
	
//	ll_existe = dw_6.Retrieve(li_cliente,li_planta,ll_despacho)
//	
//	IF ll_existe > 0 THEN
//		FOR ll_fiborra = 1 TO dw_6.RowCount()
//			ls_Registro	=	dw_6.Object.embq_codigo[ll_fiborra]
//			ls_Registro	+=	String(dw_6.Object.defe_guides[ll_fiborra], '00000000')
//			ls_Registro	+=	String(dw_6.Object.plde_codigo[ll_fiborra], '0000')
//			ls_Registro	+=	String('000000000000000')
//			ls_Registro	+=	String(li_cliente)
//			ls_Registro	+=	String(dw_6.Object.paen_numero[ll_fiborra], '0000000')
//			ls_Registro	+=	String('00000000000000000000000000000000000000000000000000000000000000000000')
//			ls_Registro += String(dw_6.Object.borr_estado[ll_fiborra], '@')
//			
//			ll_filadet	=	dw_1.InsertRow(0)
//			dw_1.Object.registro[ll_filadet]	=	ls_Registro
//		NEXT	
//	END IF	
	
	IF dw_1.SaveAs(gs_disco+":\GeneradosSAAM\" + ls_Archivo, Text!, False) = -1 THEN
		MessageBox("Atención","No se pudo generar el archivo " + ls_Archivo)
		Return
	ELSE
		enviamail()
				
		sle_mensa.text	= "Archivo " + ls_Archivo + " Generado. Avise a Computación"
		
		ll_filas		=  dw_4.Retrieve(Integer(istr_mant.argumento[1]), &
									  Integer(istr_mant.argumento[2]),ll_planilla,ll_guia,is_tipoplanilla,ls_Instructivo)
									  
		IF ll_filas = -1 THEN
			F_ErrorBaseDatos(sqlca,"Lectura de la Tabla DESPAFRIGOEN")
		ELSEIF ll_filas = 0 THEN
//			MessageBox("Atención", "No hay información ", &
//							Exclamation!, Ok!)
//			pb_grabar.Enabled	= False
		ELSE		
			FOR ll_fila	= 1 TO ll_filas
				dw_4.SetItem(ll_fila,'defe_estado',1)
				ll_numero  = dw_4.Object.defe_numero[ll_fila]
				li_cliente = dw_4.Object.clie_codigo[ll_fila]
				li_planta  = dw_4.Object.plde_codigo[ll_fila]
				
				Update dbo.CONTROLDESPACHOS Set
				CODE_GEMSAA = 1,
				CODE_FECSAA = :ldt_hora
				WHERE clie_codigo = :li_cliente
				AND	plde_codigo = :li_planta
				AND	defe_numero = :ll_numero;
				commit;
		   NEXT
			wf_actualiza_db(False)
		END IF
	END IF
END IF

em_operacion.SetFocus()

IF ii_controlcorreo = 1 THEN
	MESSAGEBOX("Atención","Envie Archivo de Forma Manual")
ELSEIF ii_controlcorreo = 0 THEN
	MESSAGEBOX("ok","EJECUTADO SIN PROBLEMAS")
END IF	


end subroutine

on w_gene_archivo_saam_nuevo.create
this.uo_selplantas=create uo_selplantas
this.uo_selcliente=create uo_selcliente
this.dw_6=create dw_6
this.dw_5=create dw_5
this.cb_oper=create cb_oper
this.em_operacion=create em_operacion
this.st_9=create st_9
this.st_8=create st_8
this.st_11=create st_11
this.ddlb_1=create ddlb_1
this.em_planilla=create em_planilla
this.dw_guia=create dw_guia
this.cbx_pak=create cbx_pak
this.cbx_guia=create cbx_guia
this.st_7=create st_7
this.em_despacho=create em_despacho
this.cbx_prod=create cbx_prod
this.cbx_cal=create cbx_cal
this.cbx_var=create cbx_var
this.dw_2=create dw_2
this.st_4=create st_4
this.st_3=create st_3
this.sle_mensa=create sle_mensa
this.em_fzarpe=create em_fzarpe
this.sle_nave=create sle_nave
this.dw_1=create dw_1
this.st_5=create st_5
this.dw_3=create dw_3
this.pb_salir=create pb_salir
this.pb_grabar=create pb_grabar
this.st_1=create st_1
this.dw_4=create dw_4
this.st_6=create st_6
this.st_2=create st_2
this.Control[]={this.uo_selplantas,&
this.uo_selcliente,&
this.dw_6,&
this.dw_5,&
this.cb_oper,&
this.em_operacion,&
this.st_9,&
this.st_8,&
this.st_11,&
this.ddlb_1,&
this.em_planilla,&
this.dw_guia,&
this.cbx_pak,&
this.cbx_guia,&
this.st_7,&
this.em_despacho,&
this.cbx_prod,&
this.cbx_cal,&
this.cbx_var,&
this.dw_2,&
this.st_4,&
this.st_3,&
this.sle_mensa,&
this.em_fzarpe,&
this.sle_nave,&
this.dw_1,&
this.st_5,&
this.dw_3,&
this.pb_salir,&
this.pb_grabar,&
this.st_1,&
this.dw_4,&
this.st_6,&
this.st_2}
end on

on w_gene_archivo_saam_nuevo.destroy
destroy(this.uo_selplantas)
destroy(this.uo_selcliente)
destroy(this.dw_6)
destroy(this.dw_5)
destroy(this.cb_oper)
destroy(this.em_operacion)
destroy(this.st_9)
destroy(this.st_8)
destroy(this.st_11)
destroy(this.ddlb_1)
destroy(this.em_planilla)
destroy(this.dw_guia)
destroy(this.cbx_pak)
destroy(this.cbx_guia)
destroy(this.st_7)
destroy(this.em_despacho)
destroy(this.cbx_prod)
destroy(this.cbx_cal)
destroy(this.cbx_var)
destroy(this.dw_2)
destroy(this.st_4)
destroy(this.st_3)
destroy(this.sle_mensa)
destroy(this.em_fzarpe)
destroy(this.sle_nave)
destroy(this.dw_1)
destroy(this.st_5)
destroy(this.dw_3)
destroy(this.pb_salir)
destroy(this.pb_grabar)
destroy(this.st_1)
destroy(this.dw_4)
destroy(this.st_6)
destroy(this.st_2)
end on

event open;Boolean lb_Cerrar 

If IsNull(uo_SelCliente.Codigo) Then lb_Cerrar = True

If lb_Cerrar Then
	Close(This)
Else
	uo_SelCliente.Seleccion(False, False)
	uo_SelPlantas.Seleccion(False, False)
	uo_SelCliente.Inicia(gi_CodExport)
	uo_SelPlantas.Inicia(gi_CodPlanta)
	
	istr_mant.argumento[1]	=	String(uo_SelCliente.Codigo)
	istr_mant.argumento[2]	=	String(uo_SelPlantas.Codigo)
	
	dw_5.SetTransObject(Sqlca)
	
	//ii_var = 1
	IF gi_vari_rotulada = 1 THEN
		cbx_var.Checked	=	True
		cbx_var.Enabled	=	False
	ELSE
		cbx_var.Checked	= 	False
		cbx_var.Enabled	=	True
	END IF	
	
	IF gi_prod_rotulado = 1 THEN
		cbx_prod.Checked	=	True
		cbx_prod.Enabled	=	False
	ELSE
		cbx_prod.Checked	= 	False
		cbx_prod.Enabled	=	True
	END IF
	
	IF gi_cali_rotulado = 1 THEN
		cbx_cal.Checked	=	True
		cbx_cal.Enabled	=	False
	ELSE
		cbx_cal.Checked	= 	False
		cbx_cal.Enabled	=	True
	END IF
	
	IF gi_pack_rotulado = 1 THEN
		cbx_pak.Checked	=	True
		cbx_pak.Enabled	=	False
	ELSE
		cbx_pak.Checked	= 	False
		cbx_pak.Enabled	=	True
	END IF
	
	ddlb_1.SelectItem(Integer(1))
	
	is_tipoplanilla = '1'
	
	ii_var			= gi_vari_rotulada
	ii_prod 		= gi_prod_rotulado
	ii_cal 			= gi_cali_rotulado
	ii_packing	= gi_pack_rotulado
	
	GrabaAccesoAplicacion(True, id_FechaAcceso, it_HoraAcceso, This.Title, "Acceso a Aplicación", 1)
End If
end event

event close;GrabaAccesoAplicacion(False, id_FechaAcceso, it_HoraAcceso, "", "", 0)

end event

type uo_selplantas from uo_seleccion_plantas within w_gene_archivo_saam_nuevo
event destroy ( )
integer x = 626
integer y = 356
integer height = 92
integer taborder = 50
end type

on uo_selplantas.destroy
call uo_seleccion_plantas::destroy
end on

event ue_cambio;call super::ue_cambio;istr_mant.argumento[2]	=	String(This.Codigo)
end event

type uo_selcliente from uo_seleccion_clientesprod within w_gene_archivo_saam_nuevo
event destroy ( )
integer x = 626
integer y = 232
integer height = 92
integer taborder = 30
end type

on uo_selcliente.destroy
call uo_seleccion_clientesprod::destroy
end on

event ue_cambio;call super::ue_cambio;istr_mant.argumento[1]	=	String(This.Codigo)
end event

type dw_6 from datawindow within w_gene_archivo_saam_nuevo
boolean visible = false
integer x = 2359
integer y = 72
integer width = 146
integer height = 128
integer taborder = 50
string title = "none"
string dataobject = "dw_palleteliminados_despacho"
borderstyle borderstyle = stylelowered!
end type

type dw_5 from datawindow within w_gene_archivo_saam_nuevo
boolean visible = false
integer x = 2359
integer y = 256
integer width = 146
integer height = 128
integer taborder = 110
string title = "none"
string dataobject = "dw_controla_prediocuartel"
borderstyle borderstyle = stylelowered!
end type

type cb_oper from uo_buscar within w_gene_archivo_saam_nuevo
integer x = 910
integer y = 472
integer width = 96
integer height = 88
integer taborder = 40
boolean bringtotop = true
end type

event clicked;Str_busqueda	lstr_busq
String	ls_embarque

lstr_busq.argum[1]	=	istr_mant.argumento[1]

OpenWithParm(w_busc_embarques, lstr_busq)

istr_busq	= Message.PowerObjectParm

IF istr_busq.argum[1] = "" THEN
	em_operacion.SetFocus()
ELSE
	
	ls_embarque = Mid(istr_busq.argum[1],5,1)

	IF ls_embarque = 'M' OR ls_embarque = 'A' OR ls_embarque = 'T'  THEN
		MessageBox("Atención", "Intructivo debe ser Distinto a Maritmo, Aereo o Terrestre.~r~rIngrese otro Embarque.", &
							Exclamation!, Ok!)
		This.SetFocus()
		Return
	END IF	

	em_operacion.Text			= istr_busq.argum[1]
	sle_nave.text				= istr_busq.argum[2]
	em_fzarpe.text				= istr_busq.argum[3]
	id_fecha						= Date(istr_busq.argum[3])
	pb_grabar.Enabled			= True
	IF ExisteOperacion(em_operacion.Text) THEN
	END IF
END IF



end event

type em_operacion from editmask within w_gene_archivo_saam_nuevo
integer x = 631
integer y = 468
integer width = 261
integer height = 92
integer taborder = 30
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long backcolor = 16777215
alignment alignment = center!
textcase textcase = upper!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = stringmask!
string displaydata = "$"
end type

event getfocus;

IF This.Text <> '' THEN
	IF ExisteOperacion(This.Text) THEN
	END IF	
END IF	
end event

event modified;String	ls_embarque

ls_embarque = Mid(This.Text,5,1)

IF ls_embarque = 'M' OR ls_embarque = 'A' OR ls_embarque = 'T'  THEN
	MessageBox("Atención", "Intructivo debe ser Distinto a Maritmo, Aereo o Terrestre.~r~rIngrese otro Embarque.", &
						Exclamation!, Ok!)
	This.Text = ''
	This.SetFocus()
	Return
END IF	

IF ExisteOperacion(This.Text) = False THEN
	This.SetFocus()
END IF



 
	
end event

type st_9 from statictext within w_gene_archivo_saam_nuevo
integer x = 178
integer y = 488
integer width = 443
integer height = 84
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
boolean enabled = false
string text = "Nro Embarque"
boolean focusrectangle = false
end type

type st_8 from statictext within w_gene_archivo_saam_nuevo
integer x = 178
integer y = 612
integer width = 448
integer height = 84
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
boolean enabled = false
string text = "Planilla S.A.G."
boolean focusrectangle = false
end type

type st_11 from statictext within w_gene_archivo_saam_nuevo
integer x = 178
integer y = 728
integer width = 448
integer height = 84
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
boolean enabled = false
string text = "Tipo Planilla"
boolean focusrectangle = false
end type

type ddlb_1 from dropdownlistbox within w_gene_archivo_saam_nuevo
integer x = 626
integer y = 720
integer width = 1403
integer height = 400
integer taborder = 60
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
boolean hscrollbar = true
boolean vscrollbar = true
string item[] = {"1. Productos Agríc. de Export. Certificados","2. Productos Agr.Export. Cert. (USDA)","3. Fruta a ser Fumigada en U.S.A.","4. Fumigados","5. Fruta a ser Fumigada en México"}
borderstyle borderstyle = stylelowered!
end type

event selectionchanged;is_tipoplanilla	=	String(index)
end event

type em_planilla from editmask within w_gene_archivo_saam_nuevo
event getfocus pbm_ensetfocus
integer x = 635
integer y = 600
integer width = 443
integer height = 92
integer taborder = 50
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long backcolor = 16777215
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = stringmask!
string displaydata = "$"
end type

event modified;IF ExistePlanilla(Long(This.Text)) = False THEN
	This.SetFocus()
END IF
end event

type dw_guia from datawindow within w_gene_archivo_saam_nuevo
boolean visible = false
integer x = 2190
integer y = 72
integer width = 146
integer height = 128
integer taborder = 80
string title = "none"
string dataobject = "dw_info_guia_despacho_cal_nuevo"
borderstyle borderstyle = stylelowered!
end type

type cbx_pak from checkbox within w_gene_archivo_saam_nuevo
integer x = 727
integer y = 1200
integer width = 530
integer height = 80
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Packing Rot."
end type

event clicked;if this.checked = true then
	ii_packing = 1
else
	ii_packing = 0
end if	
end event

type cbx_guia from checkbox within w_gene_archivo_saam_nuevo
integer x = 923
integer y = 884
integer width = 357
integer height = 80
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Todas"
boolean checked = true
end type

event clicked;IF This.Checked THEN
	em_despacho.Enabled		=	False
	em_despacho.text = ""
	pb_grabar.Enabled = TRUE
ELSE
	em_despacho.Enabled		=	True
	em_despacho.SetFocus()
END IF	
end event

type st_7 from statictext within w_gene_archivo_saam_nuevo
integer x = 178
integer y = 884
integer width = 402
integer height = 64
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Nro Guía"
boolean focusrectangle = false
end type

type em_despacho from editmask within w_gene_archivo_saam_nuevo
integer x = 626
integer y = 876
integer width = 261
integer height = 92
integer taborder = 70
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long backcolor = 16777215
boolean enabled = false
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = stringmask!
string displaydata = "$"
end type

event getfocus;//IF ExisteOperacion(This.Text) 
end event

event modified;IF This.Text <> '' THEN
	IF NOT ExisteGuia(uo_SelCliente.Codigo,uo_SelPlantas.Codigo,&
		Long(This.Text),Long(em_planilla.text),is_tipoplanilla)  THEN
		This.Text = ""
		This.SetFocus()
	END IF
END IF	



 
	
end event

type cbx_prod from checkbox within w_gene_archivo_saam_nuevo
integer x = 183
integer y = 1200
integer width = 530
integer height = 80
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Productor Rot."
boolean checked = true
end type

event clicked;IF THIS.checked = true then
	ii_prod = 1
ELSE
	ii_prod = 0
END IF	
end event

type cbx_cal from checkbox within w_gene_archivo_saam_nuevo
integer x = 727
integer y = 1112
integer width = 439
integer height = 80
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Calidad Rot."
end type

event clicked;if this.checked = true then
	ii_cal = 1
else
	ii_cal = 0
end if	
end event

type cbx_var from checkbox within w_gene_archivo_saam_nuevo
integer x = 183
integer y = 1104
integer width = 480
integer height = 80
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Variedad Rot."
boolean checked = true
end type

event clicked;if this.checked = true then
	ii_var = 1
else
	ii_var = 0
end if	
end event

type dw_2 from datawindow within w_gene_archivo_saam_nuevo
boolean visible = false
integer x = 2528
integer y = 72
integer width = 146
integer height = 128
integer taborder = 120
string title = "none"
string dataobject = "dw_info_archivo_saam_packing_nuevo"
borderstyle borderstyle = stylelowered!
end type

type st_4 from statictext within w_gene_archivo_saam_nuevo
integer x = 178
integer y = 368
integer width = 402
integer height = 64
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Planta"
boolean focusrectangle = false
end type

type st_3 from statictext within w_gene_archivo_saam_nuevo
integer x = 178
integer y = 244
integer width = 402
integer height = 64
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Cliente"
boolean focusrectangle = false
end type

type sle_mensa from singlelineedit within w_gene_archivo_saam_nuevo
integer x = 160
integer y = 1372
integer width = 1888
integer height = 92
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = script!
string facename = "Comic Sans MS"
long textcolor = 65535
long backcolor = 553648127
boolean autohscroll = false
boolean displayonly = true
borderstyle borderstyle = stylelowered!
end type

type em_fzarpe from editmask within w_gene_archivo_saam_nuevo
integer x = 1499
integer y = 984
integer width = 421
integer height = 92
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long backcolor = 16777215
alignment alignment = center!
boolean displayonly = true
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
string displaydata = "@"
end type

type sle_nave from singlelineedit within w_gene_archivo_saam_nuevo
integer x = 178
integer y = 984
integer width = 1285
integer height = 92
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long backcolor = 16777215
boolean autohscroll = false
boolean displayonly = true
borderstyle borderstyle = stylelowered!
end type

type dw_1 from datawindow within w_gene_archivo_saam_nuevo
boolean visible = false
integer x = 2007
integer y = 1812
integer width = 549
integer height = 400
string dataobject = "dw_gene_archivo_saam_plano_nuevo"
boolean livescroll = true
end type

type st_5 from statictext within w_gene_archivo_saam_nuevo
integer x = 78
integer y = 68
integer width = 2043
integer height = 92
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 16711680
boolean enabled = false
string text = "Genera Archivo Plano Puerto"
alignment alignment = center!
boolean border = true
borderstyle borderstyle = stylelowered!
boolean focusrectangle = false
end type

type dw_3 from datawindow within w_gene_archivo_saam_nuevo
boolean visible = false
integer x = 2190
integer y = 256
integer width = 146
integer height = 128
integer taborder = 110
string dataobject = "dw_gene_archivo_saam_nuevo"
borderstyle borderstyle = stylelowered!
end type

event clicked;This.Print()
end event

type pb_salir from picturebutton within w_gene_archivo_saam_nuevo
integer x = 2185
integer y = 1268
integer width = 302
integer height = 244
integer taborder = 100
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean cancel = true
string picturename = "\Desarrollo 17\Imagenes\Botones\Apagar.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\Apagar-bn.png"
alignment htextalign = left!
end type

event clicked;Close(Parent)
end event

type pb_grabar from picturebutton within w_gene_archivo_saam_nuevo
integer x = 2190
integer y = 916
integer width = 302
integer height = 244
integer taborder = 90
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
boolean default = true
string picturename = "\Desarrollo 17\Imagenes\Botones\Aceptar base datos.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\Aceptar base datos-bn.png"
alignment htextalign = left!
end type

event clicked;Parent.TriggerEvent("ue_guardar")
end event

type st_1 from statictext within w_gene_archivo_saam_nuevo
integer x = 78
integer y = 184
integer width = 2043
integer height = 672
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 16711680
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type dw_4 from datawindow within w_gene_archivo_saam_nuevo
boolean visible = false
integer x = 2528
integer y = 256
integer width = 146
integer height = 128
integer taborder = 130
string dataobject = "dw_gene_archivo_saam_estado_nuevo"
borderstyle borderstyle = stylelowered!
end type

type st_6 from statictext within w_gene_archivo_saam_nuevo
integer x = 78
integer y = 1304
integer width = 2043
integer height = 224
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 16711680
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_2 from statictext within w_gene_archivo_saam_nuevo
integer x = 78
integer y = 856
integer width = 2043
integer height = 448
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 16711680
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

