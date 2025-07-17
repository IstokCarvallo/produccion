$PBExportHeader$w_maed_ctlcalplanillaespecies.srw
$PBExportComments$Ingresador de antecedentes para Planilla Cuantitativa por especies.
forward
global type w_maed_ctlcalplanillaespecies from w_mant_encab_deta_csd
end type
type tab_1 from tab within w_maed_ctlcalplanillaespecies
end type
type tabpage_2 from userobject within tab_1
end type
type dw_inspeccion from uo_dw within tabpage_2
end type
type tabpage_2 from userobject within tab_1
dw_inspeccion dw_inspeccion
end type
type tabpage_5 from userobject within tab_1
end type
type dw_inspecpromedio from datawindow within tabpage_5
end type
type tabpage_5 from userobject within tab_1
dw_inspecpromedio dw_inspecpromedio
end type
type tabpage_1 from userobject within tab_1
end type
type dw_evaluacion from uo_dw within tabpage_1
end type
type tabpage_1 from userobject within tab_1
dw_evaluacion dw_evaluacion
end type
type tabpage_4 from userobject within tab_1
end type
type dw_madurez from datawindow within tabpage_4
end type
type tabpage_4 from userobject within tab_1
dw_madurez dw_madurez
end type
type tabpage_3 from userobject within tab_1
end type
type dw_distribucion from uo_dw within tabpage_3
end type
type tabpage_3 from userobject within tab_1
dw_distribucion dw_distribucion
end type
type tabpage_6 from userobject within tab_1
end type
type dw_firmeza from datawindow within tabpage_6
end type
type tabpage_6 from userobject within tab_1
dw_firmeza dw_firmeza
end type
type tabpage_7 from userobject within tab_1
end type
type dw_catguarda from datawindow within tabpage_7
end type
type tabpage_7 from userobject within tab_1
dw_catguarda dw_catguarda
end type
type tabpage_8 from userobject within tab_1
end type
type dw_botrytis from datawindow within tabpage_8
end type
type tabpage_8 from userobject within tab_1
dw_botrytis dw_botrytis
end type
type tabpage_9 from userobject within tab_1
end type
type mle_observacion from multilineedit within tabpage_9
end type
type tabpage_9 from userobject within tab_1
mle_observacion mle_observacion
end type
type tabpage_10 from userobject within tab_1
end type
type dw_materiaseca from uo_dw within tabpage_10
end type
type tabpage_10 from userobject within tab_1
dw_materiaseca dw_materiaseca
end type
type tabpage_11 from userobject within tab_1
end type
type rb_ac2 from radiobutton within tabpage_11
end type
type rb_fc2 from radiobutton within tabpage_11
end type
type rb_ac1 from radiobutton within tabpage_11
end type
type rb_fc1 from radiobutton within tabpage_11
end type
type gb_3 from groupbox within tabpage_11
end type
type tabpage_11 from userobject within tab_1
rb_ac2 rb_ac2
rb_fc2 rb_fc2
rb_ac1 rb_ac1
rb_fc1 rb_fc1
gb_3 gb_3
end type
type tabpage_12 from userobject within tab_1
end type
type dw_alternaria from uo_dw within tabpage_12
end type
type tabpage_12 from userobject within tab_1
dw_alternaria dw_alternaria
end type
type tabpage_13 from userobject within tab_1
end type
type dw_segregacion from uo_dw within tabpage_13
end type
type tabpage_13 from userobject within tab_1
dw_segregacion dw_segregacion
end type
type tab_1 from tab within w_maed_ctlcalplanillaespecies
tabpage_2 tabpage_2
tabpage_5 tabpage_5
tabpage_1 tabpage_1
tabpage_4 tabpage_4
tabpage_3 tabpage_3
tabpage_6 tabpage_6
tabpage_7 tabpage_7
tabpage_8 tabpage_8
tabpage_9 tabpage_9
tabpage_10 tabpage_10
tabpage_11 tabpage_11
tabpage_12 tabpage_12
tabpage_13 tabpage_13
end type
end forward

global type w_maed_ctlcalplanillaespecies from w_mant_encab_deta_csd
integer width = 5079
integer height = 2320
string title = "PLANILLA RECEPCION CEREZAS"
string menuname = ""
windowstate windowstate = maximized!
event ue_validapassword ( )
tab_1 tab_1
end type
global w_maed_ctlcalplanillaespecies w_maed_ctlcalplanillaespecies

type variables
DataWindowChild idwc_zonas,idwc_plantas,idwc_productores,idwc_especies, &
						idwc_variedades,idwc_tecnicos,idwc_inspectores,idwc_predio,idwc_cuartel

Datawindow 	dw_3,dw_4,dw_5,dw_6,dw_7,dw_8,dw_9,dw_10, dw_11, dw_12, dw_13
Datastore     	ds_porespecie, ds_firmeza

uo_zonas             		iuo_zonas
uo_plantadesp        	iuo_plantas
uo_variedades        	iuo_variedades
uo_productores       	iuo_productor
uo_ctlcalinspectores	iuo_inspectores
uo_especie         	 	iuo_especies
uo_prodcuarteles     	iuo_prodcuarteles
uo_prodpredio			iuo_prodpredio

Integer	ii_especie, ii_zona, ii_sw //este sw controla si la planilla existe con otra especie
Long		il_lote1, il_lote2, il_lote3, il_lote4, il_lote5
String 	is_calibre
Date		id_fechaproc





end variables

forward prototypes
protected function integer wf_modifica ()
public subroutine habilitaencab (boolean habilita)
public subroutine habilitaingreso (string ls_columna)
protected function boolean wf_actualiza_db (boolean borrando)
public function boolean duplicado (string data)
public function integer valida_frutosevaluados ()
public function long recupera_inspeccion (integer ai_cliente, integer ai_planta, long al_numero, integer ai_especie)
public subroutine buscalote ()
public subroutine bloquea_columnas ()
public function long recupera_evaluacion (integer ai_cliente, integer ai_planta, long al_numero, integer ai_especie)
public function boolean existelote (integer ai_planta, integer ai_especie, integer al_lote)
public subroutine calcula_porexport ()
public subroutine porexpor_final ()
public function boolean existemovimiento ()
public function boolean existe_lote (string as_columna, integer as_valor)
public function long recupera_madurez (integer ai_cliente, integer ai_planta, long al_numero, integer ai_especie)
public function integer valida_muestras ()
public function long recupera_inspromedio (integer ai_cliente, integer ai_planta, long al_numero, integer ai_especie)
public subroutine calcula_promedio (long al_fila, string as_columna)
public subroutine calcula_rango (long al_fila, string as_columna)
public function boolean trae_planilla (long al_planilla, integer ai_planta, integer ai_especie)
public subroutine deshabilita_porexp (integer ai_especie)
public subroutine habilita (boolean ab_columna)
public function long recupera_firmeza (integer ai_cliente, integer ai_planta, long al_numero, integer ai_especie)
public function boolean existeloteoriginal (integer ai_lote)
public subroutine calcula_promedio_aceites (long al_fila, string as_columna)
public subroutine recupera_botrytis (integer ai_cliente, integer ai_planta, long al_numero, integer ai_especie)
public subroutine calculo_profir (long al_fila, string as_columna)
public function integer wf_valida_evaluacioncolor ()
public subroutine wf_calcula_minmax (long al_fila, string as_columna, integer tipo, datawindow adw, boolean minimo)
public function integer recupera_catguarda (integer ai_cliente, integer ai_planta, integer ai_especie, long al_numero, integer ai_selector)
protected function long recupera_distribucion (integer ai_cliente, integer ai_planta, long al_numero, integer ai_especie, integer ai_variedad, integer tipo)
public function boolean wf_validaporcentaje (string as_columna, string as_valor, integer flag)
public function long recupera_materiaseca (integer ai_cliente, integer ai_planta, long al_numero, integer ai_especie)
public subroutine desviacion_estandar (long al_fila, string as_columna, integer ai_tipo, datawindow adw)
public subroutine calcula_media (long al_fila, string as_columna, integer ai_tipo, datawindow adw)
public function integer recupera_alternaria (integer ai_cliente, integer ai_planta, integer ai_especie, long al_numero, integer ai_selector)
public function integer recupera_segregacion (integer ai_cliente, integer ai_planta, integer ai_especie, long al_numero, integer ai_selector)
public subroutine wf_calcula_promedio_fb (long al_fila, string as_columna)
public subroutine calcula_relacion (long al_fila, decimal ad_valor, string as_columna)
public function byte wf_validacategoria (string as_columna, string as_valor, datawindow adw, long row)
end prototypes

event ue_validapassword();istr_mant.Argumento[1]	=	"Control de Calidad"
istr_mant.Argumento[2]	=	gstr_parlote.paswor

OpenWithParm(w_password, istr_mant)

istr_mant	=	Message.PowerObjectParm

IF istr_mant.Respuesta = 0 THEN Close(This)
end event

protected function integer wf_modifica ();RETURN 1
end function

public subroutine habilitaencab (boolean habilita);If Habilita Then
	dw_2.Object.ccre_numero.Protect		=	0
	dw_2.Object.zona_codigo.Protect		=	0
	dw_2.Object.plde_codigo.Protect		=	0
	dw_2.Object.vari_codigo.Protect		=	0
	dw_2.Object.prod_codigo.Protect		=	0
	dw_2.Object.prbr_codpre.Protect		=	0
	dw_2.Object.ccre_feccos.Protect		=	0
	dw_2.Object.ccre_fecrec.Protect		=	0
	dw_2.Object.ccre_hordes.Protect		=	0
	dw_2.Object.ccre_horing.Protect		=	0
	dw_2.Object.ccre_temper.Protect		=	0
	dw_2.Object.prcc_codigo.Protect		=	0
	dw_2.Object.ccre_noguia.Protect		=	0
	dw_2.Object.lote_codigo.Protect		=	0
	dw_2.Object.ccre_tamlot.Protect		=	0
	dw_2.Object.ccin_codigo.Protect		=	0
	dw_2.Object.ccre_tipcal.Protect		=	0
	dw_2.Object.ccre_horins.Protect		=	0
	dw_2.Object.ccre_fecins.Protect		=	0
	
	dw_3.Object.ccec_mues01.Protect		=	0
	dw_3.Object.ccec_mues02.Protect		=	0
	dw_3.Object.ccec_mues03.Protect		=	0
	dw_3.Object.ccec_mues04.Protect		=	0
	dw_3.Object.ccec_mues05.Protect		=	0
	dw_3.Object.ccec_mues06.Protect		=	0
	dw_3.Object.ccec_mues07.Protect		=	0
	dw_3.Object.ccec_mues08.Protect		=	0
	dw_3.Object.ccec_mues09.Protect		=	0
	dw_3.Object.ccec_mues10.Protect		=	0
	dw_3.Object.ccec_mues11.Protect		=	0
	dw_3.Object.ccec_mues12.Protect		=	0
	dw_3.Object.ccec_mues13.Protect		=	0
	dw_3.Object.ccec_mues14.Protect		=	0
	dw_3.Object.ccec_mues15.Protect		=	0
	dw_3.Object.ccec_mues16.Protect		=	0
	dw_3.Object.ccec_mues17.Protect		=	0
	dw_3.Object.ccec_mues18.Protect		=	0
	dw_3.Object.ccec_mues19.Protect		=	0
	dw_3.Object.ccec_mues20.Protect		=	0
	dw_3.Object.ccec_mues21.Protect		=	0
	dw_3.Object.ccec_mues22.Protect		=	0
	dw_3.Object.ccec_mues23.Protect		=	0
	dw_3.Object.ccec_mues24.Protect		=	0
	dw_3.Object.ccec_mues25.Protect		=	0
	dw_3.Object.ccec_clasif.Protect			=	0
	dw_4.Object.ccic_cantid.Protect     		=  0
	dw_4.Object.ccic_totfru.Protect        		=  0
	dw_5.Object.cidc_cantid.Protect			=  0
	
	dw_2.Object.ccre_numero.Color=	0
	dw_2.Object.zona_codigo.Color=	0
	dw_2.Object.plde_codigo.Color	=	0
	dw_2.Object.vari_codigo.Color	=	0
	dw_2.Object.prod_codigo.Color=	0
	dw_2.Object.prbr_codpre.Color=	0
	dw_2.Object.ccre_feccos.Color	=	0
	dw_2.Object.ccre_fecrec.Color	=	0
	dw_2.Object.ccre_hordes.Color=	0
	dw_2.Object.ccre_horing.Color	=	0
	dw_2.Object.ccre_temper.Color=	0
	dw_2.Object.prcc_codigo.Color	=	0
	dw_2.Object.ccre_noguia.Color	=	0
	dw_2.Object.lote_codigo.Color	=	0
	dw_2.Object.ccre_tamlot.Color	=	0
	dw_2.Object.ccin_codigo.Color	=	0
	dw_2.Object.ccre_tipcal.Color	=	0
	dw_2.Object.ccre_horins.Color	=	0
	dw_2.Object.ccre_fecins.Color	=	0	
	
	dw_2.Object.ccre_numero.BackGround.Color	=	RGB(255,255,255)
	dw_2.Object.zona_codigo.BackGround.Color=	RGB(255,255,255)
	dw_2.Object.plde_codigo.BackGround.Color	=	RGB(255,255,255)
	dw_2.Object.vari_codigo.BackGround.Color	=	RGB(255,255,255)
	dw_2.Object.prod_codigo.BackGround.Color	=	RGB(255,255,255)
	dw_2.Object.prbr_codpre.BackGround.Color	=	RGB(255,255,255)
	dw_2.Object.ccre_feccos.BackGround.Color	=	RGB(255,255,255)
	dw_2.Object.ccre_fecrec.BackGround.Color	=	RGB(255,255,255)
	dw_2.Object.ccre_hordes.BackGround.Color	=	RGB(255,255,255)
	dw_2.Object.ccre_horing.BackGround.Color	=	RGB(255,255,255)
	dw_2.Object.ccre_temper.BackGround.Color=	RGB(255,255,255)
	dw_2.Object.prcc_codigo.BackGround.Color	=	RGB(255,255,255)
	dw_2.Object.ccre_noguia.BackGround.Color	=	RGB(255,255,255)
	dw_2.Object.lote_codigo.BackGround.Color	=	RGB(255,255,255)
	dw_2.Object.ccre_tamlot.BackGround.Color	=	RGB(255,255,255)
	dw_2.Object.ccin_codigo.BackGround.Color	=	RGB(255,255,255)
	dw_2.Object.ccre_tipcal.BackGround.Color	=	RGB(255,255,255)
	dw_2.Object.ccre_horins.BackGround.Color	=	RGB(255,255,255)
	dw_2.Object.ccre_fecins.BackGround.Color	=	RGB(255,255,255)
  	pb_buscar.Enabled   = True		
Else
	dw_2.Object.ccre_numero.Protect			=	1
	dw_2.Object.zona_codigo.Protect			=	1
	dw_2.Object.plde_codigo.Protect				=	1
	dw_2.Object.vari_codigo.Protect				=	1
	dw_2.Object.prod_codigo.Protect				=	1
	dw_2.Object.prbr_codpre.Protect				=	1
	dw_2.Object.ccre_feccos.Protect				=	1
	dw_2.Object.ccre_fecrec.Protect				=	1
	dw_2.Object.ccre_hordes.Protect				=	1
	dw_2.Object.ccre_horing.Protect				=	1
	dw_2.Object.ccre_temper.Protect			=	1
	dw_2.Object.prcc_codigo.Protect				=	1
	dw_2.Object.ccre_noguia.Protect				=	1
	dw_2.Object.lote_codigo.Protect				=	1
	dw_2.Object.ccre_tamlot.Protect				=	1
	dw_2.Object.ccin_codigo.Protect				=	1
	dw_2.Object.ccre_tipcal.Protect				=	1
	dw_2.Object.ccre_horins.Protect				=	1
	dw_2.Object.ccre_fecins.Protect				=	1

	dw_3.Object.ccec_mues01.Protect			=	1
	dw_3.Object.ccec_mues02.Protect			=	1
	dw_3.Object.ccec_mues03.Protect			=	1
	dw_3.Object.ccec_mues04.Protect			=	1
	dw_3.Object.ccec_mues05.Protect			=	1
	dw_3.Object.ccec_mues06.Protect			=	1
	dw_3.Object.ccec_mues07.Protect			=	1
	dw_3.Object.ccec_mues08.Protect			=	1
	dw_3.Object.ccec_mues09.Protect			=	1
	dw_3.Object.ccec_mues10.Protect			=	1
	dw_3.Object.ccec_mues11.Protect			=	1
	dw_3.Object.ccec_mues12.Protect			=	1
	dw_3.Object.ccec_mues13.Protect			=	1
	dw_3.Object.ccec_mues14.Protect			=	1
	dw_3.Object.ccec_mues15.Protect			=	1
	dw_3.Object.ccec_mues16.Protect			=	1
	dw_3.Object.ccec_mues17.Protect			=	1
	dw_3.Object.ccec_mues18.Protect			=	1
	dw_3.Object.ccec_mues19.Protect			=	1
	dw_3.Object.ccec_mues20.Protect			=	1
	dw_3.Object.ccec_mues21.Protect			=	1
	dw_3.Object.ccec_mues22.Protect			=	1
	dw_3.Object.ccec_mues23.Protect			=	1
	dw_3.Object.ccec_mues24.Protect			=	1
	dw_3.Object.ccec_mues25.Protect			=	1
	dw_3.Object.ccec_clasif.Protect				=	1
	dw_4.Object.ccic_cantid.Protect           		=  1
	dw_4.Object.ccic_totfru.Protect           		=  1
	dw_5.Object.cidc_cantid.Protect           		=  1
	
	dw_2.Object.ccre_numero.Color=	RGB(255,255,255)
	dw_2.Object.zona_codigo.Color=	RGB(255,255,255)
	dw_2.Object.plde_codigo.Color	=	RGB(255,255,255)
	dw_2.Object.vari_codigo.Color	=	RGB(255,255,255)
	dw_2.Object.prod_codigo.Color=	RGB(255,255,255)
	dw_2.Object.prbr_codpre.Color=	RGB(255,255,255)
	dw_2.Object.ccre_feccos.Color	=	RGB(255,255,255)
	dw_2.Object.ccre_fecrec.Color	=	RGB(255,255,255)
	dw_2.Object.ccre_hordes.Color=	RGB(255,255,255)
	dw_2.Object.ccre_horing.Color	=	RGB(255,255,255)
	dw_2.Object.ccre_temper.Color=	RGB(255,255,255)
	dw_2.Object.prcc_codigo.Color	=	RGB(255,255,255)
	dw_2.Object.ccre_noguia.Color	=	RGB(255,255,255)
	dw_2.Object.lote_codigo.Color	=	RGB(255,255,255)
	dw_2.Object.ccre_tamlot.Color	=	RGB(255,255,255)
	dw_2.Object.ccin_codigo.Color	=	RGB(255,255,255)
	dw_2.Object.ccre_tipcal.Color	=	RGB(255,255,255)
	dw_2.Object.ccre_horins.Color	=	RGB(255,255,255)
	dw_2.Object.ccre_fecins.Color	=	RGB(255,255,255)
	
	dw_2.Object.ccre_numero.BackGround.Color	=	553648127
	dw_2.Object.zona_codigo.BackGround.Color	=	553648127
	dw_2.Object.plde_codigo.BackGround.Color		=	553648127
	dw_2.Object.vari_codigo.BackGround.Color		=	553648127
	dw_2.Object.prod_codigo.BackGround.Color		=	553648127
	dw_2.Object.prbr_codpre.BackGround.Color		=	553648127
	dw_2.Object.ccre_feccos.BackGround.Color		=	553648127
	dw_2.Object.ccre_fecrec.BackGround.Color		=	553648127
	dw_2.Object.ccre_hordes.BackGround.Color		=	553648127
	dw_2.Object.ccre_horing.BackGround.Color		=	553648127
	dw_2.Object.ccre_temper.BackGround.Color	=	553648127
	dw_2.Object.prcc_codigo.BackGround.Color		=	553648127
	dw_2.Object.ccre_noguia.BackGround.Color		=	553648127
	dw_2.Object.lote_codigo.BackGround.Color		=	553648127
	dw_2.Object.ccre_tamlot.BackGround.Color		=	553648127
	dw_2.Object.ccin_codigo.BackGround.Color		=	553648127
	dw_2.Object.ccre_tipcal.BackGround.Color		=	553648127
	dw_2.Object.ccre_horins.BackGround.Color		=	553648127
	dw_2.Object.ccre_fecins.BackGround.Color		=	553648127
	pb_buscar.Enabled   = False	
End If
end subroutine

public subroutine habilitaingreso (string ls_columna);Boolean	lb_Estado = True

dw_2.AcceptText()

IF ls_Columna <> "zona_codigo" AND &
	(dw_2.Object.zona_codigo[1]) = 0 OR IsNull(dw_2.Object.zona_codigo[1]) THEN
	lb_Estado	=	False
END IF

IF ls_Columna <> "plde_codigo" AND &
	(dw_2.Object.plde_codigo[1]) = 0 OR IsNull(dw_2.Object.plde_codigo[1]) THEN
	lb_Estado	=	False
END IF

IF ls_Columna <> "vari_codigo" AND &
	(dw_2.Object.vari_codigo[1]) = 0 OR IsNull(dw_2.Object.vari_codigo[1]) THEN
	lb_Estado	=	False
END IF
	
IF ls_Columna <> "prod_codigo" AND &
	(dw_2.Object.prod_codigo[1]) = 0 OR IsNull(dw_2.Object.prod_codigo[1]) THEN
	lb_Estado	=	False
END IF

IF ls_Columna <> "ccin_codigo" AND &
	(dw_2.Object.ccin_codigo[1]) = 0 OR IsNull(dw_2.Object.ccin_codigo[1]) THEN
	lb_Estado	=	False
END IF

IF ls_Columna <> "ccre_numero" AND &
	(dw_2.Object.ccre_numero[1]) = 0 OR IsNull(dw_2.Object.ccre_numero[1])  THEN
	lb_Estado	=	False
END IF

IF ls_Columna <> "lote_codigo" AND &
	(dw_2.Object.lote_codigo[1]) = 0 OR IsNull(dw_2.Object.lote_codigo[1]) OR ii_sw = 1 THEN
	lb_Estado	=	False
END IF

IF lb_Estado = TRUE THEN
	IF ii_especie = 21 OR ii_especie = 41 THEN
		IF dw_3.RowCount() = 0 OR dw_4.RowCount() = 0 OR dw_5.RowCount() = 0 THEN
			lb_Estado	=	FALSE
		ELSE
			lb_Estado	=	TRUE
		END IF
	END IF
END IF

pb_grabar.Enabled = lb_Estado
end subroutine

protected function boolean wf_actualiza_db (boolean borrando);Boolean	lb_AutoCommit, lb_Retorno

If Not dw_2.uf_check_required(0) Then Return False

lb_AutoCommit		=	sqlca.AutoCommit
sqlca.AutoCommit	=	False

If Borrando Then
	If dw_13.Update(True, False) = 1 Then	
		If dw_12.Update(True, False) = 1 Then	
			If dw_11.Update(True, False) = 1 Then	
				If dw_10.Update(True, False) = 1 Then	
				  If dw_9.Update(True, False) = 1 Then	
					 If dw_8.Update(True, False) = 1 Then
						If dw_7.Update(True, False) = 1 Then
							If dw_6.Update(True, False) = 1 Then
								If dw_5.Update(True, False) = 1 Then
									If dw_4.Update(True,False) =	1 Then
										If dw_3.Update(True, False) = 1 Then
											If dw_2.Update(True, False) = 1 Then
												Commit;
												
												If sqlca.SQLCode <> 0 Then
													F_ErrorBaseDatos(sqlca, This.Title)
												Else
													lb_Retorno	=	True
													
													dw_13.ResetUpdate()
													dw_12.ResetUpdate()
													dw_11.ResetUpdate()
													dw_10.ResetUpdate()
													dw_9.ResetUpdate()
													dw_8.ResetUpdate()
													dw_7.ResetUpdate()
													dw_6.ResetUpdate()
													dw_5.ResetUpdate()
													dw_4.ResetUpdate()
													dw_3.ResetUpdate()
													dw_2.ResetUpdate()
												End If
											Else
												F_ErrorBaseDatos(sqlca, This.Title)
												RollBack;
											End If
										Else
											F_ErrorBaseDatos(sqlca, This.Title)
											RollBack;
										End If
									Else
										F_ErrorBaseDatos(sqlca, This.Title)
										RollBack;
									End If
								Else
								  F_ErrorBaseDatos(sqlca, This.Title)
								  RollBack;
								End If
							Else
								F_ErrorBaseDatos(sqlca, This.Title)
								RollBack;
							End If
						Else
							F_ErrorBaseDatos(sqlca, This.Title)
							RollBack;
						End If
					Else
						F_ErrorBaseDatos(sqlca, This.Title)
						RollBack;
					End If
				Else
					F_ErrorBaseDatos(sqlca, This.Title)
					RollBack;
				End If
			Else
				F_ErrorBaseDatos(sqlca, This.Title)
				RollBack;
			End If
		Else
			F_ErrorBaseDatos(sqlca, This.Title)
			RollBack;
		End If
	Else
		F_ErrorBaseDatos(sqlca, This.Title)
		RollBack;
	End If
Else
	F_ErrorBaseDatos(sqlca, This.Title)
	RollBack;
End If
Else
  If dw_2.Update(True, False) = 1 Then
	  If dw_3.Update(True, False) = 1 Then
	     If dw_4.Update(True, False) = 1 Then
		     If dw_5.Update(True,False) =	1	Then
				  If dw_6.Update(True,False) =	1	Then
					  If dw_7.Update(True,False) =	1	Then
						   If dw_8.Update(True,False) =	1	Then
								If dw_9.Update(True,False) =	1	Then	
									If dw_10.Update(True,False) =	1	Then
										If dw_11.Update(True,False) =	1	Then
											If dw_12.Update(True,False) =	1	Then
												If dw_13.Update(True,False) =	1	Then
													Commit;
											
													If sqlca.SQLCode <> 0 Then
														F_ErrorBaseDatos(sqlca, This.Title)
													Else
														lb_Retorno	=	True
														
														dw_2.ResetUpdate()
														dw_3.ResetUpdate()
														dw_4.ResetUpdate()
														dw_5.ResetUpdate()
														dw_6.ResetUpdate()
														dw_7.ResetUpdate()
														dw_8.ResetUpdate()
														dw_9.ResetUpdate()
														dw_10.ResetUpdate()
														dw_11.ResetUpdate()
														dw_12.ResetUpdate()
														dw_13.ResetUpdate()
													End If
												Else
												  F_ErrorBaseDatos(sqlca, This.Title)
												  RollBack;
												End If	
											Else
											  F_ErrorBaseDatos(sqlca, This.Title)
											  RollBack;
											End If	
										Else
										  F_ErrorBaseDatos(sqlca, This.Title)
										  RollBack;
										End If	
									Else
									  F_ErrorBaseDatos(sqlca, This.Title)
									  RollBack;
									End If	
								Else
								  F_ErrorBaseDatos(sqlca, This.Title)
								  RollBack;
								End If
							Else
							  F_ErrorBaseDatos(sqlca, This.Title)
							  RollBack;
							End If	
						Else
							F_ErrorBaseDatos(sqlca, This.Title)
						   RollBack;
						End If
					 Else
						F_ErrorBaseDatos(sqlca, This.Title)
						RollBack;
					 End If
				 Else
					F_ErrorBaseDatos(sqlca, This.Title)
					RollBack;
				 End If
			Else
				F_ErrorBaseDatos(sqlca, This.Title)
				RollBack;
			End If
		Else
			F_ErrorBaseDatos(sqlca, This.Title)
			RollBack;
		End If
   Else
		F_ErrorBaseDatos(sqlca, This.Title)
		RollBack;
	End If	
End If

sqlca.AutoCommit	=	lb_AutoCommit

Return lb_Retorno
end function

public function boolean duplicado (string data);return true
end function

public function integer valida_frutosevaluados ();Long    ll_fila
Integer li_cont

If dw_3.RowCount() > 0 Then
	For ll_fila = 1 To dw_3.RowCount()
		 If IsNull(dw_3.Object.ccec_mues01[ll_fila]) And (dw_3.Object.ccev_codigo[ll_fila] <> 5 ) Then li_cont++
   Next
End If
	
Return li_cont
end function

public function long recupera_inspeccion (integer ai_cliente, integer ai_planta, long al_numero, integer ai_especie);Long    ll_fila, ll_fild, ll_exisFil, ll_New, ll_null
String  ls_familia, ls_secuenc
Integer li_familia, li_secuenc
setnull(ll_null)

dw_4.Retrieve(ai_cliente,ai_planta,al_numero,ai_especie)

ds_porespecie.Dataobject =	'dw_mues_ctlcaldefectos'
ds_porespecie.SetTransObject(SQLca)

ll_fila	=	ds_porespecie.Retrieve(0,ai_especie)

For ll_fila = 1 To ds_porespecie.RowCount()
	 li_familia = ds_porespecie.Object.ccfa_codigo[ll_fila]
	 li_secuenc = ds_porespecie.Object.ccde_codigo[ll_fila]
	 ls_secuenc = ds_porespecie.Object.ccde_descri[ll_fila]
	 
	 ll_fild	= dw_4.Find("ccfa_codigo = " + String(li_familia) + " AND ccde_codigo = " + String(li_secuenc), 1, dw_4.RowCount())
	 	 				
  	 If ll_fild = 0 Then
		 ll_New = dw_4.InsertRow(0)
		  dw_4.Object.espe_codigo[ll_New]	= ai_especie
		 dw_4.Object.ccfa_codigo[ll_New]		= li_familia
		 dw_4.Object.ccde_codigo[ll_New]	= li_secuenc
		 dw_4.Object.ccde_descri[ll_New]		= ls_secuenc
		 
		 If ai_especie = 82 Then
			 dw_4.Object.ccic_canti2[ll_New]		= ll_null
			 dw_4.Object.ccic_cantid[ll_New]		= ll_null
			 dw_4.Object.ccic_cancom[ll_New]	= ll_null
		End If
	 End If
Next

ll_exisFil = dw_4.RowCount()
	 
Return ll_exisFil
end function

public subroutine buscalote ();Integer	li_Planta, li_Especie, li_Zona, li_Variedad
Long		ll_Fila, ll_Productor, ll_Fila1

If (Isnull(dw_2.Object.plde_codigo[1]) OR dw_2.Object.plde_codigo[1] = 0) Or &
(Isnull(dw_2.Object.espe_codigo[1]) OR dw_2.Object.espe_codigo[1] = 0) Or &
(Isnull(dw_2.Object.zona_codigo[1]) OR dw_2.Object.zona_codigo[1] = 0)Then 
	Messagebox("Atención","Debe Seleccionar Zona, Planta y Especie")
	dw_2.setfocus()
	Return
Else
	If IsNull(dw_2.Object.vari_codigo[1]) 	Then
		li_variedad = -1 // TODAS
	Else
		li_variedad = dw_2.Object.vari_codigo[1]
	End If
	
	If IsNull(dw_2.Object.prod_codigo[1]) 	Then
		ll_productor = -1
	Else
		ll_productor = dw_2.Object.prod_codigo[1]
	End If
	
	istr_busq.argum[1] = String(dw_2.object.plde_codigo[1])
	istr_busq.argum[2] = String(dw_2.object.espe_codigo[1])
	istr_busq.argum[4] =	String(dw_2.Object.zona_codigo[1])
	istr_busq.argum[5] =	'RECEPCION'
	istr_busq.argum[6] =	String(li_variedad)
	istr_busq.argum[7] =	String(ll_productor)
	istr_busq.argum[8] = String(dw_2.Object.clie_codigo[1])
	istr_busq.argum[20] = String(dw_2.Object.ccre_tipore[1])
	
	OpenWithParm(w_busc_lotefrutagranel, istr_busq)
	
	istr_busq	= Message.PowerObjectParm
	
	If istr_busq.argum[3] <> "" Then
		
		
		dw_2.GetChild("prod_codigo", idwc_productores) 
		idwc_productores.SetTransObject(sqlca)
		idwc_productores.Retrieve(dw_2.Object.zona_codigo[1])	
		
		dw_2.GetChild("prbr_codpre", idwc_predio)
		idwc_predio.SetTransObject(sqlca)
		idwc_predio.Retrieve(Long(istr_busq.argum[5]))
		
		dw_2.GetChild("prcc_codigo", idwc_cuartel)
		idwc_cuartel.SetTransObject(sqlca)
		idwc_cuartel.Retrieve(Long(istr_busq.argum[5]), Integer(istr_busq.argum[8]))
		
		
		dw_2.Object.lote_codigo[1] =	Integer(istr_busq.argum[2])
		dw_2.Object.plde_codigo[1] =	Integer(istr_busq.argum[3])
		dw_2.Object.prod_codigo[1] =	Long(istr_busq.argum[5])
		dw_2.Object.vari_codigo[1] =	Integer(istr_busq.argum[6])
		dw_2.Object.prbr_codpre[1] =	Integer(istr_busq.argum[8])
		dw_2.Object.prcc_codigo[1] =	Integer(istr_busq.argum[9])
		dw_2.Object.ccre_tamlot[1] =	Integer(istr_busq.argum[10])
		dw_2.Object.ccre_fecrec[1] =	Date(istr_busq.argum[11])
		dw_2.Object.ccre_noguia[1] =	Long(istr_busq.argum[12])
		
		dw_2.Object.lote_totnet[1] = dec(istr_busq.argum[24])
		
		IF ii_especie = 21 THEN dw_2.Object.ccre_hidroc[1] =	Long(istr_busq.argum[22])
		dw_2.Object.clie_codigo[1] =	Long(istr_busq.argum[23])
		/*Procedimento nunca trae fecha de cosecha, se podría traer de tabla de pesaje,
		pero puede existir fruta con distinta fecha de cosecha, por lo tanto, opto por la digitacion*/
		//dw_2.Object.ccre_feccos[1] =	Date(istr_busq.argum[13])
		dw_2.Object.espe_codigo[1] =	Integer(istr_busq.argum[14])
		dw_2.AcceptText()

//		If IsNull(dw_2.Object.ccre_feccos[1]) Then
//			If IsNull(istr_busq.argum[13]) Then dw_2.Object.ccre_feccos[1] = Today()
//		End If
		
//		14/11/2007 ModIf. V.Costa
//li_planta = Integer(istr_busq.argum[3]) 
//		
//		SELECT zona_codigo  
//        INTO :li_zona  
//        FROM dba.plantadesp  
//       WHERE plde_codigo = :li_planta;  
//					
//		dw_2.Object.zona_codigo[1] = 	li_zona 
		ll_fila = Recupera_Distribucion(dw_2.Object.clie_codigo[1],Integer(istr_busq.argum[3]),0,Integer(istr_busq.argum[14]),Integer(istr_busq.argum[6]), 1)
		ll_fila1 = recupera_catguarda(dw_2.Object.clie_codigo[1],Integer(istr_busq.argum[3]),ii_especie,0,0)		
		
		//ai_cliente,ai_planta,al_numero,ai_especie,ai_variedad
		
		If ll_fila = 0 Then
			Messagebox("Atención","Falta Ingresar Calibre en tabla respectiva", StopSign!, Ok!)
			dw_2.SetFocus()
			RETURN
		End If
		
		bloquea_columnas()
		If dw_2.Object.ccre_tipore[1] = 2 Then
			If existeloteoriginal(Integer(istr_busq.argum[2])) Then
				If il_lote1 <> 0 Then
					dw_2.Object.ccre_loteo1[1]			= il_lote1 
				End If	
				
				If il_lote2 <> 0 Then
					dw_2.Object.ccre_loteo2[1]			= il_lote2
				End If
				
				If il_lote3 <> 0 Then
					dw_2.Object.ccre_loteo3[1] 		= il_lote3
				End If
				
				If il_lote4 <> 0 Then
					dw_2.Object.ccre_loteo4[1]			= il_lote4
				End If
				
				If il_lote5 <> 0 Then
					dw_2.Object.ccre_loteo5[1]			= il_lote5
				End If	
				
				dw_2.Object.ccre_fecpre[1]			=	Date(istr_busq.argum[11])
				dw_2.Object.ccre_colorl[1]			=	istr_busq.argum[21]
			End If	
		End If			
   End If
End If

end subroutine

public subroutine bloquea_columnas ();dw_2.Object.zona_codigo.Protect			=	1
dw_2.Object.plde_codigo.Protect			=	1
dw_2.Object.vari_codigo.Protect			=	1
dw_2.Object.prod_codigo.Protect			=	1
dw_2.Object.prbr_codpre.Protect			=	1
dw_2.Object.ccre_fecrec.Protect			=	1
dw_2.Object.prcc_codigo.Protect			=	1
dw_2.Object.ccre_noguia.Protect			=	1
dw_2.Object.lote_codigo.Protect			=	1
dw_2.Object.ccre_tamlot.Protect			=	1

IF ii_especie = 21 THEN dw_2.Object.ccre_hidroc.Protect	=	1
//	dw_2.Object.ccre_fecins.Protect			=	1
//	dw_2.Object.ccre_porexp.Protect		=	1

dw_2.Object.zona_codigo.Color=	RGB(255,255,255)
dw_2.Object.plde_codigo.Color	=	RGB(255,255,255)
dw_2.Object.vari_codigo.Color	=	RGB(255,255,255)
dw_2.Object.prod_codigo.Color=	RGB(255,255,255)
dw_2.Object.prbr_codpre.Color=	RGB(255,255,255)
dw_2.Object.ccre_fecrec.Color	=	RGB(255,255,255)
dw_2.Object.prcc_codigo.Color	=	RGB(255,255,255)
dw_2.Object.ccre_noguia.Color	=	RGB(255,255,255)
dw_2.Object.lote_codigo.Color	=	RGB(255,255,255)
dw_2.Object.ccre_tamlot.Color	=	RGB(255,255,255)

		
dw_2.Object.zona_codigo.BackGround.Color=	553648127
dw_2.Object.plde_codigo.BackGround.Color	=	553648127
dw_2.Object.vari_codigo.BackGround.Color	=	553648127
dw_2.Object.prod_codigo.BackGround.Color	=	553648127
dw_2.Object.prbr_codpre.BackGround.Color	=	553648127
dw_2.Object.ccre_fecrec.BackGround.Color	=	553648127
dw_2.Object.prcc_codigo.BackGround.Color	=	553648127
dw_2.Object.ccre_noguia.BackGround.Color	=	553648127
dw_2.Object.lote_codigo.BackGround.Color	=	553648127
dw_2.Object.ccre_tamlot.BackGround.Color	=	553648127

IF ii_especie = 41 THEN
	dw_2.Object.lote_totnet.Protect				=	1
	dw_2.Object.lote_totnet.Color					=	Rgb(255,255,255)
	dw_2.Object.lote_totnet.BackGround.Color	=	553648127
END IF

	
If ii_especie = 21 Then dw_2.Object.ccre_hidroc.BackGround.Color	=	553648127
	//dw_2.Object.ccre_fecins.BackGround.Color	=	553648127
	//dw_2.Object.ccre_porexp.Protect =	553648127
		
IF (dw_2.Object.zona_codigo[1] > 0  AND NOT IsNull(dw_2.Object.zona_codigo[1])) AND &
	(dw_2.Object.plde_codigo[1] > 0  AND NOT IsNull(dw_2.Object.plde_codigo[1])) AND &
	(dw_2.Object.vari_codigo[1] > 0  AND NOT IsNull(dw_2.Object.vari_codigo[1])) AND &
	(dw_2.Object.prod_codigo[1] > 0  AND NOT IsNull(dw_2.Object.prod_codigo[1])) AND &
	(dw_2.Object.ccag_codigo[1] > 0  AND NOT IsNull(dw_2.Object.ccag_codigo[1])) AND &
	(dw_2.Object.ccre_numero[1] > 0  AND NOT IsNull(dw_2.Object.ccre_numero[1])) AND &
	(dw_2.Object.lote_codigo[1] > 0  AND NOT IsNull(dw_2.Object.lote_codigo[1])) AND &
	(dw_3.RowCount() > 0 AND dw_4.RowCount() > 0 AND dw_5.RowCount() > 0) THEN
			
	   dw_3.Object.ccec_mues01.Protect	=	0
	   dw_3.Object.ccec_mues02.Protect	=	0
	   dw_3.Object.ccec_mues03.Protect	=	0
	   dw_3.Object.ccec_mues04.Protect	=	0
	   dw_3.Object.ccec_mues05.Protect	=	0
	   dw_3.Object.ccec_mues06.Protect	=	0
	   dw_3.Object.ccec_mues07.Protect	=	0
	   dw_3.Object.ccec_mues08.Protect	=	0
	   dw_3.Object.ccec_mues09.Protect	=	0
	   dw_3.Object.ccec_mues10.Protect	=	0
	   dw_3.Object.ccec_mues11.Protect	=	0
	   dw_3.Object.ccec_mues12.Protect	=	0
	   dw_3.Object.ccec_mues13.Protect	=	0
	   dw_3.Object.ccec_mues14.Protect	=	0
	   dw_3.Object.ccec_mues15.Protect	=	0
	   dw_3.Object.ccec_mues16.Protect	=	0
	   dw_3.Object.ccec_mues17.Protect	=	0
	   dw_3.Object.ccec_mues18.Protect	=	0
	   dw_3.Object.ccec_mues19.Protect	=	0
	   dw_3.Object.ccec_mues20.Protect	=	0
	   dw_3.Object.ccec_mues21.Protect	=	0
	   dw_3.Object.ccec_mues22.Protect	=	0
	   dw_3.Object.ccec_mues23.Protect	=	0
	   dw_3.Object.ccec_mues24.Protect	=	0
	   dw_3.Object.ccec_mues25.Protect	=	0
	   dw_3.Object.ccec_clasif.Protect		=	0
	   dw_4.Object.ccic_cantid.Protect     	=  0
	   dw_4.Object.ccic_totfru.Protect		=  0
	   dw_5.Object.cidc_cantid.Protect     =  0
		
	pb_grabar.Enabled = True
End If
end subroutine

public function long recupera_evaluacion (integer ai_cliente, integer ai_planta, long al_numero, integer ai_especie);Long    ll_fila, ll_fild, ll_exisFil
String  ls_ccev, ls_Columna
Integer li_ccev,li_cand
DwObject dwo

dw_3.Retrieve(ai_cliente,ai_planta,al_numero,ai_especie)

ds_porespecie.Dataobject =	'dw_mues_ctlcalevaluacion'
ds_porespecie.SetTransObject(SQLca)

ll_fila	=	ds_porespecie.Retrieve(ai_especie)

FOR ll_fila = 1 to ds_porespecie.RowCount()
	 li_ccev = ds_porespecie.Object.ccev_codigo[ll_fila]
	 ls_ccev = ds_porespecie.Object.ccev_descri[ll_fila]
	 li_cand = ds_porespecie.Object.ccev_candat[ll_fila]
	 	 
	 ll_fild	= dw_3.Find("ccev_codigo = " + String(li_ccev)  ,1, dw_3.RowCount())
	 	 				
  	 IF ll_fild = 0 THEN
		 dw_3.InsertRow(0)
		 dw_3.Object.ccev_codigo[dw_3.RowCount()] = li_ccev
		 dw_3.Object.ccev_descri[dw_3.RowCount()] = ls_ccev
		 dw_3.Object.ccev_candat[dw_3.RowCount()] = li_cand
	 END IF
NEXT

IF ii_Especie = 41 THEN
//	dw_3.Object.ccec_mues11.Visible	=	False
//	dw_3.Object.ccec_mues12.Visible	=	False
//	dw_3.Object.ccec_mues13.Visible	=	False
//	dw_3.Object.ccec_mues14.Visible	=	False
//	dw_3.Object.ccec_mues15.Visible	=	False
//	dw_3.Object.ccec_mues16.Visible	=	False
//	dw_3.Object.ccec_mues17.Visible	=	False
//	dw_3.Object.ccec_mues18.Visible	=	False
//	dw_3.Object.ccec_mues19.Visible	=	False
//	dw_3.Object.ccec_mues20.Visible	=	False
//	dw_3.Object.ccec_mues21.Visible	=	False
//	dw_3.Object.ccec_mues22.Visible	=	False
//	dw_3.Object.ccec_mues23.Visible	=	False
//	dw_3.Object.ccec_mues24.Visible	=	False
//	dw_3.Object.ccec_mues25.Visible	=	False
//	dw_3.Object.ccec_rangos.Visible  	=  False
//	dw_3.Object.rangos.Visible       	=  False
//	dw_3.Object.ccec_desest.Visible	=	False
//	dw_3.Object.ccec_clasif.Visible		=	False
//	dw_3.Object.clasificacion.Visible	=	False
//	dw_3.Object.desviacion.Visible		=	False
//	dw_3.Object.t_11.Visible				=	False
//	dw_3.Object.t_12.Visible				=	False
//	dw_3.Object.t_13.Visible				=	False
//	dw_3.Object.t_14.Visible				=	False
//	dw_3.Object.t_15.Visible				=	False
//	dw_3.Object.t_16.Visible				=	False
//	dw_3.Object.t_17.Visible				=	False
//	dw_3.Object.t_18.Visible				=	False
//	dw_3.Object.t_19.Visible				=	False
//	dw_3.Object.t_20.Visible				=	False
//	dw_3.Object.t_21.Visible				=	False
//	dw_3.Object.t_22.Visible				=	False
//	dw_3.Object.t_23.Visible				=	False
//	dw_3.Object.t_24.Visible				=	False
//	dw_3.Object.t_25.Visible				=	False
//	dw_3.Object.ccec_media.x			=	1682
//	dw_3.Object.media.x					=	1691
//	dw_3.Object.muestras.Width		=	1225
ELSEIF ii_Especie = 26 OR ii_Especie = 27 OR ii_Especie = 78 OR ii_Especie = 82 OR ii_Especie = 21 OR  ii_especie = 36 OR  ii_especie = 10 THEN
	dw_3.Object.ccec_mues11.Visible	=	False
	dw_3.Object.ccec_mues12.Visible	=	False
	dw_3.Object.ccec_mues13.Visible	=	False
	dw_3.Object.ccec_mues14.Visible	=	False
	dw_3.Object.ccec_mues15.Visible	=	False
	dw_3.Object.ccec_mues16.Visible	=	False
	dw_3.Object.ccec_mues17.Visible	=	False
	dw_3.Object.ccec_mues18.Visible	=	False
	dw_3.Object.ccec_mues19.Visible	=	False
	dw_3.Object.ccec_mues20.Visible	=	False
	dw_3.Object.ccec_mues21.Visible	=	False
	dw_3.Object.ccec_mues22.Visible	=	False
	dw_3.Object.ccec_mues23.Visible	=	False
	dw_3.Object.ccec_mues24.Visible	=	False
	dw_3.Object.ccec_mues25.Visible	=	False
	dw_3.Object.ccec_media.Visible	=	False
	dw_3.Object.media.Visible	      	=	False
	dw_3.Object.ccec_desest.Visible	=	False
	dw_3.Object.ccec_clasif.Visible	=	False
	dw_3.Object.clasificacion.Visible	=	False
	dw_3.Object.desviacion.Visible		=	False
	dw_3.Object.t_11.Visible			=	False
	dw_3.Object.t_12.Visible			=	False
	dw_3.Object.t_13.Visible			=	False
	dw_3.Object.t_14.Visible			=	False
	dw_3.Object.t_15.Visible			=	False
	dw_3.Object.t_16.Visible			=	False
	dw_3.Object.t_17.Visible			=	False
	dw_3.Object.t_18.Visible			=	False
	dw_3.Object.t_19.Visible			=	False
	dw_3.Object.t_20.Visible			=	False
	dw_3.Object.t_21.Visible			=	False
	dw_3.Object.t_22.Visible			=	False
	dw_3.Object.t_23.Visible			=	False
	dw_3.Object.t_24.Visible			=	False
	dw_3.Object.t_25.Visible			=	False
		
	dw_3.Object.t_02.Visible			=	False
	dw_3.Object.t_03.Visible			=	False
	dw_3.Object.t_04.Visible			=	False
	dw_3.Object.t_05.Visible			=	False
	dw_3.Object.t_06.Visible			=	False
	dw_3.Object.t_07.Visible			=	False
	dw_3.Object.t_08.Visible			=	False
	dw_3.Object.t_09.Visible			=	False
	dw_3.Object.t_10.Visible			=	False
	dw_3.Object.ccec_mues02.Visible	=	False
	dw_3.Object.ccec_mues03.Visible	=	False
	dw_3.Object.ccec_mues04.Visible	=	False
	dw_3.Object.ccec_mues05.Visible	=	False
	dw_3.Object.ccec_mues06.Visible	=	False
	dw_3.Object.ccec_mues07.Visible	=	False
	dw_3.Object.ccec_mues08.Visible	=	False
	dw_3.Object.ccec_mues09.Visible	=	False
	dw_3.Object.ccec_mues10.Visible	=	False
	dw_3.Object.ccec_rangos.x		=	713
	dw_3.Object.rangos.x				=	713
	dw_3.Object.Muestras.Width		=	267
	dw_3.Object.Muestras.Height		=  132
	dw_3.Object.ccec_mues01.Width	=	267
	dw_3.Object.t_01.Width			=	242
	
	dw_3.Object.rangos.Visible	      	=	False
	dw_3.Object.t_1.Visible	      		=	True
	dw_3.Object.t_1.x					=	713
	dw_3.Object.t_01.Visible			=	False

	dw_3.Object.Frutos.Width			=	267
	dw_3.Object.compute_1.x			=	713
	dw_3.Object.Frutos.Visible	   		=	True
	dw_3.Object.compute_1.Visible	=	True
	IF ii_Especie = 82 or ii_Especie = 26 or  ii_especie = 36 or ii_Especie = 27 OR  ii_especie = 10 or ii_Especie = 78 THEN
		dw_3.Object.Frutos.x								=	626
		dw_3.Object.Frutos.Width						=	320
		dw_3.Object.Frutos.Font.Height				=	-8
		dw_3.Object.Frutos.Height						=	64
		dw_3.Object.Muestras.x							=	626
		dw_3.Object.Muestras.Width					=	320
		dw_3.Object.Muestras.Height					=  132
		dw_3.Object.muestras.Font.Height			= 	-8
		dw_3.Object.media.x								=	965
		dw_3.Object.media.y								=	12
		dw_3.Object.media.width						=	320
		dw_3.Object.media.height						=	132
		dw_3.Object.media.visible						= 	TRUE
		dw_3.Object.media.Font.Height				=	-8
		dw_3.Object.compute_1.Visible				=	FALSE
		dw_3.Object.ccec_mues01.x					=	626
		dw_3.Object.ccec_mues01.Width				=	320
		dw_3.Object.ccec_mues01.Font.Height		=	-8
		dw_3.Object.ccec_mues01.Height				=	64
		dw_3.Object.ccev_descri.x						=	9
		dw_3.Object.ccev_descri.width					=	599
		dw_3.Object.ccev_descri.Font.Height			=	-8
		dw_3.Object.ccev_descri.Height				=	64
		dw_3.Object.ctlcalevaluacion_ccev_descri_t.x				=	9
		dw_3.Object.ctlcalevaluacion_ccev_descri_t.width			=	599
		dw_3.Object.ctlcalevaluacion_ccev_descri_t.Font.Height	=	-8
		dw_3.Object.DataWindow.Detail.Height		=	84
		dw_3.Object.DataWindow.Summary.Height	=	84
	END IF
ELSE
	dw_3.Object.ccec_rangos.Visible	=	False
	dw_3.Object.rangos.Visible      	=	False
	dw_3.Object.Frutos.Visible	   		=	False
	dw_3.Object.compute_1.Visible	=	False
END IF

ll_exisFil = dw_3.RowCount()
	 
RETURN ll_exisFil
end function

public function boolean existelote (integer ai_planta, integer ai_especie, integer al_lote);Integer li_lote, li_variedad,  li_predio, li_cuartel, li_tamlot, li_zona, &
		  li_planta, li_tipo, li_tipore
Long    ll_guia, ll_productor, ll_fila, ll_Cliente
Date    ld_feccos, ld_fecrec

li_tipore	=	dw_2.Object.ccre_tipore[1]
ll_Cliente	=	dw_2.Object.clie_codigo[1]

SELECT Distinct lot.lote_codigo,lot.vari_codigo,lot.prod_codigo,
		lot.prbr_codpre,lot.prcc_codigo,lot.lote_guisii,
		lot.fgcc_feccos,lot.fgcc_fecrec,lot.lote_totbul,1,lot.lote_calibr,
		lot.fgcc_fecrec
 INTO :li_lote,:li_variedad,:lL_productor,
		:li_predio,:li_cuartel,:ll_guia,
		:ld_feccos,:ld_fecrec,:li_tamlot,:li_tipo,:is_calibre,
		:id_fechaproc
 FROM dbo.spro_lotesfrutagranel as lot,dbo.spro_movtofrutagrandeta as mod
	WHERE	mod.clie_codigo = :ll_Cliente
	AND mod.lote_espcod 	 = :ii_especie
	AND mod.lote_codigo 	 = lot.lote_codigo
   AND mod.lote_pltcod   = :ai_planta 
 	AND lot.lote_espcod   = :ai_especie 
  	AND lot.lote_codigo   = :al_lote
  	AND (isnull(lot.lote_calibr,'') = '' OR
		lot.lote_calibr = '');

SELECT Distinct lot.lote_codigo,lot.vari_codigo,lot.prod_codigo,
		lot.prbr_codpre,lot.prcc_codigo,lot.lote_guisii,
		lot.fgcc_feccos,lot.fgcc_fecrec,lot.lote_totbul,2,lot.lote_calibr,
		lot.fgcc_fecrec
 INTO :li_lote,:li_variedad,:lL_productor,
		:li_predio,:li_cuartel,:ll_guia,
		:ld_feccos,:ld_fecrec,:li_tamlot,:li_tipo,:is_calibre,
		:id_fechaproc
 FROM dbo.spro_lotesfrutagranel as lot,dbo.spro_movtofrutagrandeta as mod
	WHERE	mod.clie_codigo = :ll_Cliente
	AND mod.lote_espcod 	 = :ii_especie
	AND mod.lote_codigo 	 = lot.lote_codigo
   AND mod.lote_pltcod   = :ai_planta 
 	AND lot.lote_espcod   = :ai_especie 
  	AND lot.lote_codigo   = :al_lote
  AND (isnull(lot.lote_calibr,'') <> '' or
		lot.lote_calibr <> '');

IF li_tipore = li_tipo THEN

	IF sqlca.SQLCode = -1 THEN
		F_errorbasedatos(sqlca,"Lectura tabla spro_lotesfrutagranel ")
		RETURN FALSE
	
	ELSEIF sqlca.sqlcode	=	0 THEN
		
		dw_2.SetItem(1, "vari_codigo", li_variedad)
		dw_2.SetItem(1, "prod_codigo", ll_productor)		
		dw_2.SetItem(1, "prbr_codpre", li_predio)		
		dw_2.SetItem(1, "prcc_codigo", li_cuartel)		
		dw_2.SetItem(1, "ccre_noguia", ll_guia)	
		dw_2.SetItem(1, "ccre_feccos", ld_feccos)	
		dw_2.SetItem(1, "ccre_fecrec", ld_fecrec)
		dw_2.SetItem(1, "ccre_tamlot", li_tamlot)
		dw_2.SetItem(1, "lote_codigo", li_lote)
		IF IsNull(ld_feccos) THEN dw_2.SetItem(1, "ccre_feccos", Today())
		
	  SELECT zona_codigo  
		 INTO :li_zona  
		 FROM dbo.plantadesp  
		WHERE plde_codigo = :li_planta ;
		
		dw_2.SetItem(1, "zona_codigo", li_zona)
		dw_2.GetChild("plde_codigo", idwc_plantas)
		idwc_plantas.SetTransObject(sqlca)
		idwc_plantas.Retrieve(1, li_zona)	
			
		ll_fila = Recupera_Distribucion(ll_Cliente,ai_planta,0,ai_especie,li_variedad, 1)
					
			IF ll_fila = 0 THEN
				Messagebox("Atención","Falta Ingresar Calibre en tabla respectiva", StopSign!, Ok!)
				dw_2.SetFocus()
			END IF
		
		RETURN TRUE
	ELSE	
		RETURN FALSE
	END IF
ELSE	
	//Messagebox("Error de Consistencia","Tipo de Recepción no coincide con Lote Ingresado", StopSign!, Ok!)
	Return False
END IF	
end function

public subroutine calcula_porexport ();Long		ll_filexp 
Decimal{2} ld_valexpor, suma_porc = 0, ld_Valor

IF ii_especie = 21 THEN
	FOR ll_filexp = 1 TO dw_5.RowCount() - 2		
		IF Not IsNull(dw_5.Object.calc_porce[ll_filexp]) THEN
			ld_Valor =	dw_5.Object.calc_porce[ll_filexp]
			suma_porc = suma_porc + ld_Valor
		END IF		
	NEXT
	
	ld_valexpor =  ((100 - (dw_4.Object.tot_porce[1])) *  suma_porc)/100
	
	dw_2.Object.ccre_porexp[1] = ld_valexpor
ELSEIF ii_especie = 41 OR ii_especie = 27  OR ii_especie = 26 OR ii_especie = 36 OR ii_especie = 81 THEN
	FOR ll_filexp = 1 TO dw_5.RowCount() - 3
		IF Not IsNull(dw_5.Object.calc_porce[ll_filexp]) THEN
			ld_Valor =	dw_5.Object.calc_porce[ll_filexp]
			suma_porc = suma_porc + ld_Valor
		END IF		
	NEXT
	
	ld_valexpor =  ((100 - (dw_4.Object.tot_porce[1])) *  suma_porc)/100
	
	dw_2.Object.ccre_porexp[1] = ld_valexpor
END IF
end subroutine

public subroutine porexpor_final ();Long		ll_filexp 
Decimal{2} ld_valexpor, suma_porc = 0,ld_Valor

IF ii_especie = 21 THEN
	FOR ll_filexp = 1 TO dw_5.RowCount() - 1
		IF Not IsNull(dw_5.Object.calc_porce[ll_filexp]) THEN
			ld_Valor =	dw_5.Object.calc_porce[ll_filexp]
			suma_porc = suma_porc + ld_Valor
		END IF
	NEXT
	ld_valexpor =  ((100 - (dw_4.Object.tot_porce[1])) *  suma_porc)/100
	dw_2.Object.ccre_porexp[1] = ld_valexpor

ELSEIF ii_especie = 41  OR ii_especie = 27 OR ii_especie = 26 OR ii_especie = 81 OR ii_especie = 36 THEN
		FOR ll_filexp = 1 TO dw_5.RowCount() - 2
			IF Not IsNull(dw_5.Object.calc_porce[ll_filexp]) THEN
				ld_Valor =	dw_5.Object.calc_porce[ll_filexp]
				suma_porc = suma_porc + ld_Valor
			END IF
		NEXT
		ld_valexpor =  ((100 - (dw_4.Object.tot_porce[1])) *  suma_porc)/100
		dw_2.Object.ccre_porexp[1] = ld_valexpor
END IF
end subroutine

public function boolean existemovimiento ();Long	ll_Movto

SELECT	Count(*)
	INTO	:ll_Movto
	FROM	dbo.spro_movtofrutagranenca
	WHERE	espe_codigo = :ii_especie;

IF sqlca.sqlcode = -1 THEN
	F_ErrorBaseDatos(sqlca,"No se pudo leer la tabla Movimiento Granel")
	RETURN FALSE
ELSEIF ll_Movto = 0 THEN
	RETURN FALSE
ELSE	
	RETURN TRUE	
END IF
end function

public function boolean existe_lote (string as_columna, integer as_valor);Integer	li_lote, li_planta, li_especie
Long     ll_numero

li_Planta		=	dw_2.Object.plde_codigo[1]
li_Especie	=	dw_2.Object.espe_codigo[1]
li_lote			=	dw_2.Object.lote_codigo[1]

CHOOSE CASE as_columna
		
	CASE "plde_codigo"
		li_Planta  		=	Integer(as_valor)
		
	CASE "espe_codigo"
		li_especie		=	Long(as_valor)

	CASE "lote_codigo"
		li_lote		   =	Long(as_valor)
		
END CHOOSE
  SELECT ccre_numero
    INTO :ll_numero
    FROM dbo.ctlcalrecepcionfrutasenca  
   WHERE plde_codigo =  :li_planta
    AND  espe_codigo =  :li_especie   
    AND  lote_codigo =  :li_lote;
	
IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla ctlcalrecepcionfrutasenca")
	RETURN True
ELSEIF sqlca.sqlcode	=	0 OR ll_numero > 0 THEN
	Messagebox("Atención","Número de Lote ya Fué Recepcionado, Por Favor Ingrese otro",Exclamation!)
	RETURN True
ELSE	
	RETURN False
END IF


end function

public function long recupera_madurez (integer ai_cliente, integer ai_planta, long al_numero, integer ai_especie);Long    ll_fila, ll_fild, ll_exisFil
String  ls_descri, ls_Columna
Integer li_codigo
DwObject dwo

dw_6.Retrieve(ai_cliente,ai_planta,al_numero,ai_especie)

ds_porespecie.Dataobject =	'dw_mues_ctlcalevalumaducolor'
ds_porespecie.SetTransObject(SQLca)

ll_fila	=	ds_porespecie.Retrieve(ai_especie)

FOR ll_fila = 1 to ds_porespecie.RowCount()
	 li_codigo = ds_porespecie.Object.evmc_codigo[ll_fila]
	 ls_descri = ds_porespecie.Object.evmc_descri[ll_fila]
	 
	 ll_fild	= dw_6.Find("evmc_codigo = " + String(li_codigo),1, dw_6.RowCount())
	 	 				
  	 IF ll_fild = 0 THEN
		 dw_6.InsertRow(0)
		 dw_6.Object.evmc_codigo[dw_6.RowCount()] = li_codigo
		 dw_6.Object.evmc_descri[dw_6.RowCount()] = ls_descri
	 END IF
NEXT

ll_exisFil = dw_6.RowCount()
	 
RETURN ll_exisFil
end function

public function integer valida_muestras ();Long		ll_fila
Integer	li_cont

If dw_7.RowCount() > 0 Then
	For ll_fila = 1 To dw_7.RowCount()
		 If IsNull(dw_7.Object.iccd_mues01[ll_fila]) And (dw_7.Object.incc_codigo[ll_fila] <> 2 ) Then li_cont++
   Next
End If
	
Return li_cont

end function

public function long recupera_inspromedio (integer ai_cliente, integer ai_planta, long al_numero, integer ai_especie);Long    ll_fila, ll_fild, ll_exisFil, li_cand , li_valida
String  ls_descri, ls_Columna
Integer li_codigo

dw_7.SetTransObject(sqlca)

dw_7.Retrieve(ai_cliente,ai_planta,al_numero,ai_especie)

ds_porespecie.Dataobject =	'dw_mues_ctlcalinspecalicondi'
ds_porespecie.SetTransObject(SQLca)

ll_fila	=	ds_porespecie.Retrieve(ai_especie)

FOR ll_fila = 1 to ds_porespecie.RowCount()
	 li_codigo = ds_porespecie.Object.incc_codigo[ll_fila]
	 ls_descri = ds_porespecie.Object.incc_descri[ll_fila]
	 li_cand = ds_porespecie.Object.incc_candat[ll_fila]
	 
	 If ii_Especie = 21 Then li_valida = ds_porespecie.Object.incc_valida[ll_fila]
	 
	 
	 ll_fild	= dw_7.Find("incc_codigo = " + String(li_codigo),1, dw_7.RowCount())
	 	 				
  	 IF ll_fild = 0 THEN
		 dw_7.InsertRow(0)
		 dw_7.Object.incc_codigo[dw_7.RowCount()] = li_codigo
		 dw_7.Object.incc_descri[dw_7.RowCount()] = ls_descri
		 dw_7.Object.incc_candat[dw_7.RowCount()] = li_cand
		 dw_7.Object.incc_valida[dw_7.RowCount()] = li_Valida
	 END IF
NEXT

ll_exisFil = dw_7.RowCount()
	 
RETURN ll_exisFil
end function

public subroutine calcula_promedio (long al_fila, string as_columna);Decimal{2}	ld_valor,ld_media,ld_suma, ld_null
String			ls_columna
Long			ll_fila
Integer		ai_columna, li_Cuenta, li_Div

dw_7.AcceptText()

SetNull(ld_null)

ai_columna	=	Integer(as_columna)

IF Integer(istr_mant.argumento[3]) = al_fila THEN
	IF Integer(istr_mant.argumento[6]) < ai_columna THEN
		istr_mant.argumento[6]	=	String(ai_columna)
	ELSE
		ai_columna = Integer(istr_mant.argumento[6])
	END IF
ELSE
	istr_mant.argumento[6] = '0'
	istr_mant.argumento[3] = String(al_fila)
	IF Integer(istr_mant.argumento[6]) < ai_columna THEN
		istr_mant.argumento[6]	=	String(ai_columna)
	ELSE
		ai_columna = Integer(istr_mant.argumento[6])
	END IF
END IF

FOR ll_fila= 1 TO 30
	IF ll_fila >= 10 THEN
		ls_columna	=	"iccd_mues" +  String(ll_fila)
	ELSE
		ls_columna	=	"iccd_mues" + String(0) + String(ll_fila)
	END IF
	
	IF Not IsNull(dw_7.GetitemNumber(al_fila,ls_columna)) THEN li_cuenta++
	
   	ld_valor   =  dw_7.GetitemNumber(al_fila,ls_columna)
		
	If IsNull(ld_valor) Then ld_valor = 0

   	IF IsNull(ld_valor) AND li_cuenta < ai_columna THEN
	   	Messagebox("Atención","Muestra" +"  " + String(ll_fila)+ " " + "no puede ser Nula",exclamation!)
	     dw_7.SetItem(al_fila,"iccd_media",ld_null)
		dw_7.SetColumn(ls_columna)
		dw_7.SetFocus()
	ELSE
//		IF li_cuenta > ai_columna THEN
//			li_Div = li_cuenta 
//		ELSE
//			li_Div = ai_columna 
//		END IF
		If li_Cuenta < 2 Then
			li_Div = 1 
		Else
			li_Div = li_Cuenta
		End If
		
		IF IsNull(ld_valor) THEN ld_valor = 0 
		
		ld_suma  = ld_suma + ld_valor 
		ld_media = ld_suma / li_Div
		dw_7.SetItem(al_fila,"iccd_media",ld_media)
	END IF
NEXT
end subroutine

public subroutine calcula_rango (long al_fila, string as_columna);Decimal{2} ld_valor,ld_menor, ld_mayor
String	ls_columna
Long		ll_fila, ll_Nfrutos, ll_muestra, ll_null
Integer	ai_columna

dw_3.AcceptText()
SetNull(ll_null)
ll_muestra = dw_3.Object.ccec_mues01[al_fila]
ll_Nfrutos = dw_4.Object.ccic_totfru[1]

If IsNull(ll_Nfrutos) Then
	Messagebox("Atención","Falta Ingresar el Número Total de Frutos", StopSign!, Ok!)
	dw_3.SetItem(al_fila,"ccec_mues01", Integer(ll_null))
	Message.DoubleParm = -1
	Return
End If	


//If dw_2.Object.vari_codigo[1] = 17  Then
//	If dw_3.Object.ccev_codigo[al_fila] > 5 Then
//		dw_3.Object.ccec_rangos[al_fila] = String((ll_muestra * 100) / ll_Nfrutos)
//	Else
//		Messagebox("Atención","Item que esta ingresando no pertenece a la Variedad Reinier", StopSign!, Ok!)
//		dw_3.SetItem(al_fila,"ccec_mues01", Integer(ll_null))
//		Message.DoubleParm = -1
//		Return
//	End If
//Else
//	If dw_3.Object.ccev_codigo[al_fila] <= 5 Then
//		dw_3.Object.ccec_rangos[al_fila] = String((ll_muestra * 100) / ll_Nfrutos)
//	Else
//		Messagebox("Atención","Item que esta ingresando no pertenece a la Variedad", StopSign!, Ok!)
//		dw_3.SetItem(al_fila,"ccec_mues01", Integer(ll_null))
//		Message.DoubleParm = -1
//		Return
//	End If
//End If


//ll_Nfrutos = dw_4.Object.ccic_totfru[1]

//FOR ll_fila= 10 TO 1
//	If ll_fila >= 10 Then
//		ls_columna	=	"ccec_mues" +  String(ll_fila)
//	Else
//		ls_columna	=	"ccec_mues" + String(0) + String(ll_fila)
//	End If
//	ld_valor   =  dw_3.GetitemNumber(al_fila,ls_columna)
//	If NOT IsNull(ld_valor) Then
//		If ll_fila = 1 Then	
//			ld_menor   =  dw_3.GetitemNumber(al_fila,ls_columna)
//			ld_mayor   =  dw_3.GetitemNumber(al_fila,ls_columna)
//		End If4
//		If ld_menor > ld_valor 	Then
//			ld_menor = ld_valor
//		End If
//		If ld_mayor < ld_valor Then
//			ld_mayor = ld_valor
//		End If
//      dw_3.SetItem(al_fila,"ccec_rangos",String(ld_menor,'##0') + '-' + String(ld_mayor,'##0'))
//   End If	
//NEXT	
end subroutine

public function boolean trae_planilla (long al_planilla, integer ai_planta, integer ai_especie); Long    ll_existe, ll_productor, ll_Cliente, Cliente
 Integer li_variedad, li_especie, li_zona, li_predio
 
 ii_sw = 0
 ll_Cliente = dw_2.Object.clie_codigo[1]
 
 SELECT ccre_numero, vari_codigo, espe_codigo, 
        prod_codigo, zona_codigo, prbr_codpre,clie_codigo
   INTO :ll_existe, :li_variedad, :li_especie,
	     :ll_productor,:li_zona, :li_predio, :Cliente
    FROM dbo.ctlcalrecepcionfrutasenca  
   WHERE clie_codigo = :ll_Cliente
	  AND plde_codigo = :ai_planta 
	  AND ccre_numero = :al_planilla
	  AND espe_codigo = :ai_especie;
	  
 If sqlca.SQLCode = -1 Then
	 F_errorbasedatos(sqlca,"Lectura tabla ctlcalrecepcionfrutasenca  ")
	 Return False
ElseIf sqlca.sqlcode	=	0 Then
	If li_especie = dw_2.Object.espe_codigo[1] Then
		istr_busq.argum[1] = String(al_planilla)
		istr_busq.argum[3] = String(ai_planta)
		istr_busq.argum[4] = String(li_zona)
		istr_busq.argum[5] = String(ll_productor)
		istr_busq.argum[8] = String(li_predio)
		istr_busq.argum[9] = String(Cliente)
		
	   ii_sw = 0
	Else
		ii_sw = 1
	End If
	Return True
Else
	Return False
End If
end function

public subroutine deshabilita_porexp (integer ai_especie);IF /*ai_especie = 21 OR*/ ai_especie = 41 THEN
	dw_2.Object.ccre_porexp.protect = 1
END IF
end subroutine

public subroutine habilita (boolean ab_columna);Integer li_fila

IF ab_columna THEN
	dw_3.Object.ccec_mues01.Protect				=	0
	dw_3.Object.ccec_mues02.Protect				=	0
	dw_3.Object.ccec_mues03.Protect				=	0
	dw_3.Object.ccec_mues04.Protect				=	0
	dw_3.Object.ccec_mues05.Protect				=	0
	dw_3.Object.ccec_mues06.Protect				=	0
	dw_3.Object.ccec_mues07.Protect				=	0
	dw_3.Object.ccec_mues08.Protect				=	0
	dw_3.Object.ccec_mues09.Protect				=	0
	dw_3.Object.ccec_mues10.Protect				=	0
	dw_3.Object.ccec_mues11.Protect				=	0
	dw_3.Object.ccec_mues12.Protect				=	0
	dw_3.Object.ccec_mues13.Protect				=	0
	dw_3.Object.ccec_mues14.Protect				=	0
	dw_3.Object.ccec_mues15.Protect				=	0
	dw_3.Object.ccec_mues16.Protect				=	0
	dw_3.Object.ccec_mues17.Protect				=	0
	dw_3.Object.ccec_mues18.Protect				=	0
	dw_3.Object.ccec_mues19.Protect				=	0
	dw_3.Object.ccec_mues20.Protect				=	0
	dw_3.Object.ccec_mues21.Protect				=	0
	dw_3.Object.ccec_mues22.Protect				=	0
	dw_3.Object.ccec_mues23.Protect				=	0
	dw_3.Object.ccec_mues24.Protect				=	0
	dw_3.Object.ccec_mues25.Protect				=	0
ELSE
	dw_3.Object.ccec_mues01.Protect				=	1
	dw_3.Object.ccec_mues02.Protect				=	1
	dw_3.Object.ccec_mues03.Protect				=	1
	dw_3.Object.ccec_mues04.Protect				=	1
	dw_3.Object.ccec_mues05.Protect				=	1
	dw_3.Object.ccec_mues06.Protect				=	1
	dw_3.Object.ccec_mues07.Protect				=	1
	dw_3.Object.ccec_mues08.Protect				=	1
	dw_3.Object.ccec_mues09.Protect				=	1
	dw_3.Object.ccec_mues10.Protect				=	1
	dw_3.Object.ccec_mues11.Protect				=	1
	dw_3.Object.ccec_mues12.Protect				=	1
	dw_3.Object.ccec_mues13.Protect				=	1
	dw_3.Object.ccec_mues14.Protect				=	1
	dw_3.Object.ccec_mues15.Protect				=	1
	dw_3.Object.ccec_mues16.Protect				=	1
	dw_3.Object.ccec_mues17.Protect				=	1
	dw_3.Object.ccec_mues18.Protect				=	1
	dw_3.Object.ccec_mues19.Protect				=	1
	dw_3.Object.ccec_mues20.Protect				=	1
	dw_3.Object.ccec_mues21.Protect				=	1
	dw_3.Object.ccec_mues22.Protect				=	1
	dw_3.Object.ccec_mues23.Protect				=	1
	dw_3.Object.ccec_mues24.Protect				=	1
	dw_3.Object.ccec_mues25.Protect				=	1
	
END IF
end subroutine

public function long recupera_firmeza (integer ai_cliente, integer ai_planta, long al_numero, integer ai_especie);Long    ll_fila, ll_fild, ll_exisFil, ll_New
String  ls_ccef
Integer li_ccef,li_cand
DwObject dwo

dw_8.Retrieve(ai_cliente,ai_planta,al_numero,ai_especie)

ds_firmeza.Dataobject =	'dw_mues_ctlcalevaluafirmeza'
ds_firmeza.SetTransObject(SQLca)

ll_fila	=	ds_firmeza.Retrieve(ai_especie)

FOR ll_fila = 1 to ds_firmeza.RowCount()
	 li_ccef = ds_firmeza.Object.ccef_codigo[ll_fila]
	 ls_ccef = ds_firmeza.Object.ccef_descri[ll_fila]
	 li_cand = ds_firmeza.Object.ccef_candat[ll_fila]
	 	 
	 ll_fild	= dw_8.Find("ccef_codigo = " + String(li_ccef)  ,1, dw_8.RowCount())
	 	 				
  	 IF ll_fild = 0 THEN
		 ll_New = dw_8.InsertRow(0)
		 dw_8.Object.ccef_codigo[ll_New]	= li_ccef
		 dw_8.Object.ccef_descri[ll_New] 	= ls_ccef
		 dw_8.Object.ccef_candat[ll_New]	= li_cand
	 END IF	 
NEXT

ll_exisFil = dw_8.RowCount()
	 
RETURN ll_exisFil
end function

public function boolean existeloteoriginal (integer ai_lote);Long		ll_Movto, ll_numero, ll_docrel, ll_lote, ll_Cliente
Integer	li_cliente, li_tipdoc, li_planta

ll_Cliente = dw_2.Object.clie_codigo[1]

SELECT mfge_numero 
INTO	:ll_numero
FROM	dbo.spro_movtofrutagrandeta
	WHERE	clie_codigo = :ll_Cliente
	AND lote_espcod 	= :ii_especie
	AND lote_codigo 	= :ai_lote
   AND tpmv_codigo 	= 1;

IF sqlca.sqlcode = -1 THEN
	F_ErrorBaseDatos(sqlca,"No se pudo leer la tabla Detalle Granel")
	RETURN FALSE
END IF

SELECT clie_codigo,mfge_numero,plde_codigo,defg_tipdoc,defg_docrel 
INTO :li_cliente,:ll_numero,:li_planta,:li_tipdoc,:ll_docrel
FROM dbo.spro_movtofrutagranenca
	WHERE	clie_codigo = :ll_Cliente
   AND espe_codigo 	= :ii_especie
	AND mfge_numero 	= :ll_numero
	AND tpmv_codigo 	= 1
   AND defg_tipdoc 	= 8;
	
IF sqlca.sqlcode = -1 THEN
	F_ErrorBaseDatos(sqlca,"No se pudo leer la tabla Detalle Granel")
	Return False
END IF

DECLARE Lote_original PROCEDURE FOR dbo.CtlCal_lotesorigilimones
				@cliente = :ll_Cliente,
				@numero  = :ll_docrel,
				@planta  = :li_planta;
EXECUTE Lote_original;

IF sqlca.sqlcode = -1 THEN
	F_ErrorBaseDatos(sqlca,"No se pudo ejecutar Lote_original")
	Return False
END IF
			
IF SQLCA.SQLCode = 0 THEN
	FETCH Lote_original INTO :il_lote1, :il_lote2, :il_lote3, :il_lote4, :il_lote5;
	
	CLOSE Lote_original;
END IF

Return True


end function

public subroutine calcula_promedio_aceites (long al_fila, string as_columna);Decimal{2} ld_valor,ld_media,ld_suma, ld_null
String	ls_columna
Long		ll_fila
Integer	ai_columna, li_Cuenta, li_Div

dw_6.AcceptText()

SetNull(ld_null)

ai_columna	=	Integer(as_columna)

IF Integer(istr_mant.argumento[3]) = al_fila THEN
	IF Integer(istr_mant.argumento[6]) < ai_columna THEN
		istr_mant.argumento[6]	=	String(ai_columna)
	ELSE
		ai_columna = Integer(istr_mant.argumento[6])
	END IF
ELSE
	istr_mant.argumento[6] = '0'
	istr_mant.argumento[3] = String(al_fila)
	IF Integer(istr_mant.argumento[6]) < ai_columna THEN
		istr_mant.argumento[6]	=	String(ai_columna)
	ELSE
		ai_columna = Integer(istr_mant.argumento[6])
	END IF
END IF

FOR ll_fila= 1 TO 10
	IF ll_fila >= 10 THEN
		ls_columna	=	"evmc_mues" +  String(ll_fila)
	ELSE
		ls_columna	=	"evmc_mues" + String(0) + String(ll_fila)
	END IF
	
	IF ls_columna	=	"evmc_mues01" THEN
		ls_columna	=	"evmc_muestr"
	END IF	
	
	IF Not IsNull(dw_6.GetitemDecimal(al_fila,ls_columna)) THEN li_cuenta++
	
   ld_valor   =  dw_6.GetitemDecimal(al_fila,ls_columna)

   IF IsNull(ld_valor) AND li_cuenta < ai_columna THEN
	   Messagebox("Atención","Muestra" +"  " + String(ll_fila)+ " " + "no puede ser Nula",exclamation!)
	   dw_6.SetItem(al_fila,"evmc_promed",ld_null)
		dw_6.SetColumn(ls_columna)
		dw_6.SetFocus()
	ELSE
		IF li_cuenta > ai_columna THEN
			li_Div = li_cuenta 
		ELSE 
			li_Div = ai_columna 
		END IF
		
		IF IsNull(ld_valor) THEN ld_valor = 0 
		
		ld_suma  = ld_suma + ld_valor 
		ld_media = ld_suma / li_Div
		dw_6.SetItem(al_fila,"evmc_promed",ld_media)
	END IF
NEXT
end subroutine

public subroutine recupera_botrytis (integer ai_cliente, integer ai_planta, long al_numero, integer ai_especie);Long    ll_fila, ll_fila1
String  ls_descri, ls_Columna
Integer li_codigo
DwObject dwo

ll_fila1 = dw_10.Retrieve(ai_cliente,ai_planta,al_numero,ai_especie)

IF ll_fila1 = 0 THEN
	FOR ll_fila = 1 to 5
		
		dw_10.InsertRow(0)
		dw_10.Object.ctbo_secuen[ll_fila] = ll_fila
	
	NEXT
END IF	

end subroutine

public subroutine calculo_profir (long al_fila, string as_columna);Decimal{2} ld_valor,ld_media,ld_suma, ld_null
String	ls_columna
Long		ll_fila
Integer	ai_columna, li_Cuenta, li_Div

dw_8.AcceptText()

SetNull(ld_null)

ai_columna	=	Integer(as_columna)

IF Integer(istr_mant.argumento[3]) = al_fila THEN
	IF Integer(istr_mant.argumento[6]) < ai_columna THEN
		istr_mant.argumento[6]	=	String(ai_columna)
	ELSE
		ai_columna = Integer(istr_mant.argumento[6])
	END IF
ELSE
	istr_mant.argumento[6] = '0'
	istr_mant.argumento[3] = String(al_fila)
	IF Integer(istr_mant.argumento[6]) < ai_columna THEN
		istr_mant.argumento[6]	=	String(ai_columna)
	ELSE
		ai_columna = Integer(istr_mant.argumento[6])
	END IF
END IF

FOR ll_fila= 1 TO ai_columna
	IF ll_fila <= 9 THEN
		ls_columna	=	"ccef_mue00" +  String(ll_fila)
	ELSEIF ll_fila = 100 THEN
		ls_columna	=	"ccef_mue" + String(ll_fila)
	ELSE
		ls_columna	=	"ccef_mue0" + String(ll_fila)
	END IF
	
	IF Not IsNull(dw_8.GetitemNumber(al_fila,ls_columna)) THEN li_cuenta++
	
   ld_valor   =  dw_8.GetitemNumber(al_fila,ls_columna)

   IF IsNull(ld_valor) AND li_cuenta < ai_columna THEN
	   Messagebox("Atención","Muestra" +"  " + String(ll_fila)+ " " + "no puede ser Nula",exclamation!)
	   	dw_8.SetItem(ll_fila,"ccef_media",ld_null)
		dw_8.SetFocus()
		dw_8.SetColumn(ll_fila)
		EXIT
	ELSE
		IF li_cuenta > ai_columna THEN
			li_Div = li_cuenta 
		ELSE 
			li_Div = ai_columna 
		END IF
		
		IF IsNull(ld_valor) THEN ld_valor = 0 
		
		ld_suma  = ld_suma + ld_valor 
		ld_media = ld_suma / li_Div
		dw_8.SetItem(al_fila,"ccef_media",ld_media)
	END IF
NEXT
end subroutine

public function integer wf_valida_evaluacioncolor ();Dec{2}	ld_valor, ld_total, ld_totrei
Long    	ll_fila
Integer 	li_cont

If dw_3.RowCount() > 0 Then
	For ll_fila = 1 To dw_3.RowCount()
		If dw_2.Object.vari_codigo[1] = 17 And dw_3.Object.ccev_codigo[ll_Fila] > 4 Then
			ld_valor = dw_3.Object.ccec_mues01[ll_fila]
			If IsNull(ld_valor)  Then ld_totrei = 0		
			ld_totrei += ld_Valor
		Else
			ld_valor = dw_3.Object.ccec_mues01[ll_fila]
			If IsNull(ld_valor)  Then ld_valor = 0		
			
			ld_total += ld_Valor
		End If
   Next
End If

If dw_2.Object.vari_codigo[1] = 17 Then
	If ld_totrei <> dw_4.Object.ccic_Totfru[1] Then 	li_cont = 1
Else
	If ld_total <> dw_4.Object.ccic_Totfru[1] Then 	li_cont = 1
End If

Return li_cont
end function

public subroutine wf_calcula_minmax (long al_fila, string as_columna, integer tipo, datawindow adw, boolean minimo);Decimal{2}	ld_valor, ld_Minimo
String			ls_columna, ls_Prefijo
Long			ll_fila, ll_Tope
Integer  		ai_Columna

adw.AcceptText()
SetNull(ld_Minimo)

ai_columna	=	Integer(as_columna)

ll_Tope = 10

If Tipo = 1 Then
	ls_Prefijo = 'iccd'
ElseIf Tipo = 2 Then
	ls_Prefijo = 'evmc'
Else
	ls_Prefijo = 'ccms'
End If	

For ll_Fila= 1 to ai_columna
	If ll_fila >= 10 Then
		ls_columna	=	ls_Prefijo + "_mues"  + String(ll_Fila)
	Else
		ls_columna	=	ls_Prefijo + "_mues" + String(0) + String(ll_Fila)
		If ii_Especie = 81 And ll_Fila = 1 Then
			ls_Columna = 'evmc_muestr'
		End If
	End If
		
	ld_valor   =  adw.GetitemNumber(al_fila,ls_columna)
	
	If IsNull(ld_Minimo) Then 
		ld_Minimo = ld_Valor
	Else
		If Minimo Then
			If ld_Valor <=  ld_Minimo Then ld_Minimo = ld_Valor 
		Else
			If ld_Valor >=  ld_Minimo Then ld_Minimo = ld_Valor 
		End If
	End If

Next

If Minimo Then
	adw.SetItem(al_Fila, ls_Prefijo + '_nromin', ld_Minimo)
Else
	adw.SetItem(al_Fila, ls_Prefijo + '_nromax', ld_Minimo)
End If
end subroutine

public function integer recupera_catguarda (integer ai_cliente, integer ai_planta, integer ai_especie, long al_numero, integer ai_selector);Long   	ll_fila, ll_fild, ll_exisFil, ll_contador, ll_filnueva, ll_fila1
String  	ls_categoria, ls_secuenc, ls_NomCat
Decimal 	ld_maxdef, ld_mindef, ld_maxsol, ld_minsol, ld_minfir, ld_maxfir

dw_9.Retrieve(ai_cliente,ai_planta,al_numero,ai_especie)

ds_porespecie.Dataobject =	'dw_mues_categoriaguarda'
ds_porespecie.SetTransObject(SQLca)

ll_fila	=	ds_porespecie.Retrieve(ai_especie)

For ll_fila = 1 To ds_porespecie.RowCount()
	ls_categoria 	=	ds_porespecie.Object.ctcg_codigo[ll_fila]     
	ls_NomCat		=	ds_porespecie.Object.ctcg_nombre[ll_fila]
	ld_mindef 		= 	ds_porespecie.Object.ctcg_mindef[ll_fila]          
	ld_maxdef		= 	ds_porespecie.Object.ctcg_maxdef[ll_fila]          
	ld_minsol		= 	ds_porespecie.Object.ctcg_minsso[ll_fila]          
	ld_maxsol		= 	ds_porespecie.Object.ctcg_maxsso[ll_fila]          
	ld_minfir			=	ds_porespecie.Object.ctcg_minfir[ll_fila]          
	ld_maxfir			= 	ds_porespecie.Object.ctcg_maxfir[ll_fila]
	
	ll_fild	= dw_9.Find("cccg_codigo = '" + ls_categoria + "'",1, dw_9.RowCount())
	 	 				
	If ll_fild = 0 Then
		ll_filnueva = dw_9.InsertRow(0)
				 
		dw_9.Object.clie_codigo[ll_filnueva] 		=	ai_cliente         
		dw_9.Object.plde_codigo[ll_filnueva] 		= 	ai_planta        
		dw_9.Object.ccre_numero[ll_filnueva] 	= 	al_numero         
		dw_9.Object.espe_codigo[ll_filnueva] 	= 	ai_especie        
		dw_9.Object.cccg_codigo[ll_filnueva] 	= 	ls_categoria        
		dw_9.Object.ctcg_nombre[ll_filnueva]	=	ls_NomCat      
		ll_contador = 0
	End If
Next

ll_exisFil = dw_9.RowCount()
	 
Return ll_exisFil	 

end function

protected function long recupera_distribucion (integer ai_cliente, integer ai_planta, long al_numero, integer ai_especie, integer ai_variedad, integer tipo);Long		ll_fila, ll_fild, ll_exisFil, ll_New, ll_Ordena
String 	ls_calibre, ls_gramos, ls_milime
Integer	ll_Secuen

dw_5.Reset()

If Tipo = 1 Then  dw_5.Retrieve(ai_cliente,ai_planta,al_numero,ai_especie,ai_variedad)

ds_porespecie.Dataobject =	'dw_mues_distribu_calibre'
ds_porespecie.SetTransObject(SQLca)

ll_Fila	=	ds_porespecie.Retrieve(ai_especie,ai_variedad)

For ll_fila = 1 To ds_porespecie.RowCount()
	ll_Ordena	=	ds_porespecie.Object.ccdc_ordena[ll_fila]
	ll_secuen		=	ds_porespecie.Object.ccdc_secuen[ll_fila]
	ls_calibre 	=	ds_porespecie.Object.ccdc_calibr[ll_fila]
	ls_gramos  	=	ds_porespecie.Object.ccdc_gramos[ll_fila]
	ls_milime  	=	ds_porespecie.Object.ccdc_milime[ll_fila]
	 	 
	 ll_fild	= dw_5.Find("ccdc_secuen = " + String(ll_secuen), 1, dw_5.RowCount())
	 	 				
  	 If ll_fild = 0 Then
		 ll_New = dw_5.InsertRow(0)
		 dw_5.Object.ccdc_secuen[ll_New]	= ll_secuen
		 dw_5.Object.ccdc_calibr[ll_New]		= ls_calibre
		 dw_5.Object.ccdc_gramos[ll_New]		= ls_gramos
	End If
Next

ll_exisFil = dw_5.RowCount()
	 
Return ll_exisFil	 
end function

public function boolean wf_validaporcentaje (string as_columna, string as_valor, integer flag);Boolean	lb_Retorno	=	True
Dec{2}	ld_Fancy, ld_Choice, ld_Comercial, ld_XFancy, ld_Valor

dw_2.AcceptText()

ld_Fancy			=	dw_2.Object.ccre_expack[1]
ld_Choice		=	dw_2.Object.ccre_porexp[1]
ld_Comercial	=	dw_2.Object.ccre_excome[1]
ld_XFancy 		=	dw_2.Object.ccre_porxfa[1]

Choose Case as_Columna
	Case "ccre_porxfa"	
		ld_XFancy		=	Dec(as_Valor)
		
	Case 'ccre_porexp'
		ld_Choice		=	Dec(as_Valor)
		
	Case 'ccre_expack'
		ld_Fancy			=	Dec(as_Valor)
		
	Case 'ccre_excome'
		ld_Comercial	=	Dec(as_Valor)
End Choose

If IsNull(ld_Choice) Then ld_Choice = 0
If IsNull(ld_Fancy) Then ld_Fancy = 0
If IsNull(ld_Comercial) Then ld_Comercial = 0
If IsNull(ld_XFancy) Then ld_XFancy = 0

ld_Valor = ld_Choice + ld_Fancy + ld_Comercial + ld_XFancy

If Flag = 1 Then
	If ld_Valor > 100 Then 
			lb_Retorno = False
	ElseIf ld_Valor < 100 Then 
		If Not IsNull(dw_2.Object.ccre_expack[1]) Or Not IsNull(dw_2.Object.ccre_porexp[1]) &
		Or Not IsNull(dw_2.Object.ccre_excome[1]) Or Not IsNull(dw_2.Object.ccre_porxfa[1])Then
			If ld_Valor > 100 Then 
				lb_Retorno = False
			End If
		End If
	End If
Else
	If ld_Valor <> 100 Then lb_Retorno = False
End If

Return lb_Retorno
end function

public function long recupera_materiaseca (integer ai_cliente, integer ai_planta, long al_numero, integer ai_especie);Long    ll_fila, ll_fild, ll_exisFil
String  ls_descri, ls_Columna
Integer li_codigo, li_tipo, li_cantidad
DwObject dwo

dw_11.Retrieve(ai_cliente,ai_planta,al_numero,ai_especie)

ds_porespecie.Dataobject =	'dw_mues_ctlcalmateriaseca'
ds_porespecie.SetTransObject(SQLca)

ll_fila	=	ds_porespecie.Retrieve(ai_especie, 2)

For ll_fila = 1 To ds_porespecie.RowCount()
	li_codigo 	=	ds_porespecie.Object.mase_codigo[ll_fila]
	li_tipo			=	ds_porespecie.Object.mase_tipoin[ll_fila]
	ls_descri 	=	ds_porespecie.Object.mase_nombre[ll_fila]
	li_cantidad	=	ds_porespecie.Object.mase_candig[ll_fila]
	 
	 ll_fild	= dw_11.Find("mase_codigo = " + String(li_codigo), 1, dw_11.RowCount())
	 	 				
  	 If ll_fild = 0 Then 
		 dw_11.InsertRow(0)
		 dw_11.Object.mase_codigo[dw_11.RowCount()]		= li_codigo
		 dw_11.Object.mase_nombre[dw_11.RowCount()]	= ls_descri
		 dw_11.Object.mase_tipoin[dw_11.RowCount()]		= li_tipo
		 dw_11.Object.mase_candig[dw_11.RowCount()]		= li_Cantidad
	End If
Next

ll_exisFil = dw_11.RowCount()
	 
Return ll_exisFil
end function

public subroutine desviacion_estandar (long al_fila, string as_columna, integer ai_tipo, datawindow adw);Decimal{2}	ld_valor, ld_null, ld_cuadra,ld_sumvar,ld_desvia, ld_sumvar1,var
String			ls_columna, ls_Prefijo
Long			ll_fila
Integer  		ai_columna

adw.AcceptText()

SetNull(ld_null)

ai_columna	=	Integer(as_columna)

If (ii_especie = 41 And ai_Tipo <> 3)And ii_especie <> 21 And ii_Especie <> 23 Then
	If Integer(istr_mant.argumento[2]) = al_fila Then
		If Integer(istr_mant.argumento[1]) < ai_columna Then
			istr_mant.argumento[1]	=	String(ai_columna)
		Else
			ai_columna = Integer(istr_mant.argumento[1])
		End If
	Else
		istr_mant.argumento[1] = '0'
		istr_mant.argumento[2] = String(al_fila)
		If Integer(istr_mant.argumento[1]) < ai_columna Then
			istr_mant.argumento[1]	=	String(ai_columna)
		Else
			ai_columna = Integer(istr_mant.argumento[1])
		End If
	End If
	
	If ai_Tipo = 1 Then
		ls_Prefijo = 'ccec'
	ElseIf ai_Tipo = 2 Then
		ls_Prefijo = 'ccms'
	End If	
	
	For ll_fila= 1 To ai_columna
		If ll_fila >= 10 Then
			ls_columna	=	ls_Prefijo + "_mues" +  String(ll_fila)
		Else
			ls_columna	=	ls_Prefijo + "_mues" + String(0) + String(ll_fila)
		End If
			
		ld_valor   =  adw.GetitemNumber(al_fila,ls_columna)
	
		ld_cuadra 	= ld_cuadra + ld_valor^2 // suma muestras al cuadrado
		ld_sumvar   = (ld_sumvar + ld_valor) // Suma Muestras
		ld_sumvar1  = ld_sumvar^2  // suma total de muestras al cuadrado
	Next
	
	If ai_columna > 1 Then
		var = ld_sumvar1/ai_columna
		ld_desvia   = sqrt(((ld_cuadra - var)/(ai_columna - 1)))
	End If
	
	If ai_Tipo = 2 Then
		adw.SetItem(al_Fila, ls_Prefijo + '_desstd', ld_desvia)
	Else
		adw.Object.ccec_desest[al_fila] = ld_desvia
	End If
	
ElseIf ii_Especie = 21 Or ii_Especie = 23 Then
		If Integer(istr_mant.argumento[2]) = al_fila Then
		If Integer(istr_mant.argumento[1]) < ai_columna Then
			istr_mant.argumento[1]	=	String(ai_columna)
		Else
			ai_columna = Integer(istr_mant.argumento[1])
		End If
	Else
		istr_mant.argumento[1] = '0'
		istr_mant.argumento[2] = String(al_fila)
		If Integer(istr_mant.argumento[1]) < ai_columna Then
			istr_mant.argumento[1]	=	String(ai_columna)
		Else
			ai_columna = Integer(istr_mant.argumento[1])
		End If
	End If
	
	For ll_fila= 1 To ai_columna
		If ll_fila >= 10 Then
			ls_columna	=	"iccd_mues" +  String(ll_fila)
		Else
			ls_columna	=	"iccd_mues" + String(0) + String(ll_fila)
		End If
			
		ld_valor   =  adw.GetitemNumber(al_fila,ls_columna)
	
		ld_cuadra 	= ld_cuadra + ld_valor^2 // suma muestras al cuadrado
		ld_sumvar   = (ld_sumvar + ld_valor) // Suma Muestras
		ld_sumvar1  = ld_sumvar^2  // suma total de muestras al cuadrado
	Next
	
	If ai_columna > 1 Then
		var = ld_sumvar1/ai_columna
		ld_desvia   = sqrt(((ld_cuadra - var)/(ai_columna - 1)))
	End If
	
	adw.Object.iccd_desest[al_fila] = ld_desvia
Else
	If Integer(istr_mant.argumento[2]) = al_fila Then
		If Integer(istr_mant.argumento[1]) < ai_columna Then
			istr_mant.argumento[1]	=	String(ai_columna)
		Else
			ai_columna = Integer(istr_mant.argumento[1])
		End If
	Else
		istr_mant.argumento[1] = '0'
		istr_mant.argumento[2] = String(al_fila)
		If Integer(istr_mant.argumento[1]) < ai_columna Then
			istr_mant.argumento[1]	=	String(ai_columna)
		Else
			ai_columna = Integer(istr_mant.argumento[1])
		End If
	End If
	
	For ll_fila= 1 To ai_columna
		If ll_fila = 100 Then
			ls_columna	=	"ccef_mue"  + String(ll_fila)
		ElseIf ll_fila >= 10 Then
			ls_columna	=	"ccef_mue0"  + String(ll_fila)
		Else
			ls_columna	=	"ccef_mue00" +  String(ll_fila)
		End If
			
		ld_valor   =  adw.GetitemNumber(al_fila,ls_columna)
	
		ld_cuadra 	= ld_cuadra + ld_valor^2 // suma muestras al cuadradod
		ld_sumvar   = (ld_sumvar + ld_valor) // Suma Muestras
		ld_sumvar1  = ld_sumvar^2  // suma total de muestras al cuadrado
	Next
	
	If ai_columna > 1 Then
		var = ld_sumvar1/ai_columna
		ld_desvia   = sqrt(((ld_cuadra - var)/(ai_columna - 1)))
	End If
	
	adw.Object.ccef_desstd[1] = ld_desvia
	
End If	
end subroutine

public subroutine calcula_media (long al_fila, string as_columna, integer ai_tipo, datawindow adw);Decimal{2}	ld_valor,ld_media,ld_suma, ld_null
String			ls_columna, ls_Prefijo
Long			ll_fila, ll_Div
Integer		ai_columna
Boolean		lb_boulean

adw.AcceptText()

SetNull(ld_null)

ai_columna	=	Integer(as_columna)

IF Integer(istr_mant.argumento[2]) = al_fila THEN
	IF Integer(istr_mant.argumento[1]) < ai_columna THEN
		istr_mant.argumento[1]	=	String(ai_columna) 
	ELSE
		ai_columna = Integer(istr_mant.argumento[1])
	END IF
ELSE
	istr_mant.argumento[1] = '0'
	istr_mant.argumento[2] = String(al_fila)
	IF Integer(istr_mant.argumento[1]) < ai_columna THEN
		istr_mant.argumento[1]	=	String(ai_columna)
	ELSE
		ai_columna = Integer(istr_mant.argumento[1])
	END IF
END IF

If ai_Tipo = 1 Then
	ls_Prefijo = 'ccec'
Else
	ls_Prefijo = 'ccms'
End If	

ll_Div = 0

For ll_fila= 1 To ai_columna
	If ll_fila >= 10 Then
		ls_columna	=	ls_Prefijo + "_mues" +  String(ll_fila)
	Else
		ls_columna	=	ls_Prefijo + "_mues" + String(0) + String(ll_fila)
	End If
		
	ld_valor   =  adw.GetitemNumber(al_fila,ls_columna)

	If IsNull(ld_valor) Then
		Messagebox("Atención","Muestra" +"  " + String(ll_fila)+ " " + "no puede ser Nula", Exclamation!)
		ld_valor	= 0
		adw.SetColumn(ls_columna)
		adw.SetFocus()
	Else
		ll_Div++
	End If
	
	ld_suma  = ld_suma + ld_valor 
	ld_media = ld_suma / ll_Div
	adw.SetItem(al_fila,ls_Prefijo + "_media", ld_media)
Next
end subroutine

public function integer recupera_alternaria (integer ai_cliente, integer ai_planta, integer ai_especie, long al_numero, integer ai_selector);Long   	ll_fila, ll_fild, ll_exisFil, ll_contador, ll_filnueva, ll_fila1, ll_Codigo
String  	ls_Nombre

dw_12.Retrieve(ai_cliente,ai_planta,al_numero,ai_especie)

ds_porespecie.Dataobject =	'dw_mues_alternaria'
ds_porespecie.SetTransObject(SQLca)

ll_fila	=	ds_porespecie.Retrieve(ai_especie)

For ll_fila = 1 To ds_porespecie.RowCount()
	ll_Codigo		=	ds_porespecie.Object.ctal_codigo[ll_fila]
	ls_Nombre	=	ds_porespecie.Object.ctal_nombre[ll_fila]
	
	ll_fild	= dw_12.Find("ccal_codigo = " + String(ll_Codigo), 1, dw_12.RowCount())
	 	 				
	If ll_fild = 0 Then
		ll_filnueva = dw_12.InsertRow(0)
				 
		dw_12.Object.clie_codigo[ll_filnueva] 	=	ai_cliente         
		dw_12.Object.plde_codigo[ll_filnueva] 	= 	ai_planta        
		dw_12.Object.ccre_numero[ll_filnueva] 	= 	al_numero         
		dw_12.Object.espe_codigo[ll_filnueva] 	= 	ai_especie        
		dw_12.Object.ccal_codigo[ll_filnueva] 	= 	ll_Codigo
		dw_12.Object.ctal_nombre[ll_filnueva]	=	ls_Nombre
		dw_12.Object.ccal_cantid[ll_filnueva]	=	0
		ll_contador = 0
	End If
Next

ll_exisFil = dw_12.RowCount()
	 
Return ll_exisFil	 

end function

public function integer recupera_segregacion (integer ai_cliente, integer ai_planta, integer ai_especie, long al_numero, integer ai_selector);Long   	ll_fila, ll_fild, ll_exisFil, ll_contador, ll_filnueva, ll_fila1, ll_Codigo
String  	ls_Nombre

dw_13.Retrieve(ai_cliente,ai_planta,al_numero,ai_especie)

ds_porespecie.Dataobject =	'dw_mues_segregacion'
ds_porespecie.SetTransObject(SQLca)

ll_fila	=	ds_porespecie.Retrieve(ai_especie)

For ll_fila = 1 To ds_porespecie.RowCount()
	ll_Codigo		=	ds_porespecie.Object.segr_codigo[ll_fila]
	ls_Nombre	=	ds_porespecie.Object.segr_nombre[ll_fila]
	
	ll_fild	= dw_13.Find("segr_codigo = " + String(ll_Codigo), 1, dw_13.RowCount())
	 	 				
	If ll_fild = 0 Then
		ll_filnueva = dw_13.InsertRow(0)
				 
	/*	dw_13.Object.clie_codigo[ll_filnueva] 	=	ai_cliente         
		dw_13.Object.plde_codigo[ll_filnueva] 	= 	ai_planta        
		dw_13.Object.ccre_numero[ll_filnueva] 	= 	al_numero         
		dw_13.Object.espe_codigo[ll_filnueva] 	= 	ai_especie     */   
		dw_13.Object.segr_codigo[ll_filnueva] 	= 	ll_Codigo
		dw_13.Object.segr_nombre[ll_filnueva]	=	ls_Nombre
		ll_contador = 0
	End If
Next

ll_exisFil = dw_13.RowCount()
	 
Return ll_exisFil	 
end function

public subroutine wf_calcula_promedio_fb (long al_fila, string as_columna);Decimal{2}	ld_valor,ld_media,ld_suma, ld_null
String			ls_columna
Long			ll_fila
Integer		ai_columna, li_Cuenta, li_Div

dw_7.AcceptText()

SetNull(ld_null)

ai_columna	=	Integer(as_columna)

IF Integer(istr_mant.argumento[3]) = al_fila THEN
	IF Integer(istr_mant.argumento[6]) < ai_columna THEN
		istr_mant.argumento[6]	=	String(ai_columna)
	ELSE
		ai_columna = Integer(istr_mant.argumento[6])
	END IF
ELSE
	istr_mant.argumento[6] = '0'
	istr_mant.argumento[3] = String(al_fila)
	IF Integer(istr_mant.argumento[6]) < ai_columna THEN
		istr_mant.argumento[6]	=	String(ai_columna)
	ELSE
		ai_columna = Integer(istr_mant.argumento[6])
	END IF
END IF

FOR ll_fila= 1 TO 30
	IF ll_fila >= 10 THEN
		ls_columna	=	"iccd_mues" +  String(ll_fila)
	ELSE
		ls_columna	=	"iccd_mues" + String(0) + String(ll_fila)
	END IF
	
	IF Not IsNull(dw_7.GetitemNumber(al_fila,ls_columna)) THEN li_cuenta++
	
   	ld_valor   =  dw_7.GetitemNumber(al_fila,ls_columna)
		
//	If IsNull(ld_valor) Then ld_valor = 0

   	IF IsNull(ld_valor) AND li_cuenta < ai_columna THEN
	   	Messagebox("Atención","Muestra" +"  " + String(ll_fila)+ " " + "no puede ser Nula",exclamation!)
	     dw_7.SetItem(al_fila,"iccd_media",ld_null)
		dw_7.SetColumn(ls_columna)
		dw_7.SetFocus()
	Else		
//		IF IsNull(ld_valor) THEN ld_valor = 0 
		
		If (ld_valor < 230) and Not IsNull(ld_valor) Then ld_suma++
		
		ld_media = (ld_suma / 25) * 100
		
		dw_2.SetItem(1,"ccre_frubla",ld_media)
	END IF
NEXT
end subroutine

public subroutine calcula_relacion (long al_fila, decimal ad_valor, string as_columna);Decimal{3}	ld_media, ld_valor, ld_valor2
Integer		li_fila

dw_6.AcceptText()

If al_fila = 1 Then
	ld_valor = Dec(ad_valor) 
	ld_valor2	=	dw_6.GetItemNumber(2, as_Columna)
ElseIf al_fila = 2 Then
	ld_valor2 = Dec(ad_valor) 
	ld_valor	=	dw_6.GetItemNumber(1, as_Columna)
End If

If Not IsNull(ld_valor2) And ld_valor2 <> 0 Then
	ld_media = ld_valor / ld_valor2
	dw_6.SetItem(3, as_Columna, ld_media)
End If
end subroutine

public function byte wf_validacategoria (string as_columna, string as_valor, datawindow adw, long row);Byte		lb_Retorno = 0
Decimal{2}	ld_Total, ld_Cat1, ld_Cat2, ld_Plano

ld_Cat1	=	adw.Object.ccre_pocat1[Row]
ld_Cat2	=	adw.Object.ccre_pocat2[Row]
ld_Plano	=	adw.Object.ccre_poplan[Row]

Choose Case as_Columna
	Case	'ccre_pocat1'
		ld_Cat1	=	Dec(as_Valor)
		
	Case	'ccre_pocat2'
		ld_Cat2	=	Dec(as_Valor)
		
	Case	'ccre_poplan'
		ld_Plano	=	Dec(as_Valor)
		
End Choose

If IsNull(ld_Cat1) Then ld_Cat1	= 0
If IsNull(ld_Cat2) Then ld_Cat2	= 0
If IsNull(ld_Plano) Then ld_Plano = 0

ld_Total	= ld_Cat1 + ld_Cat2 + ld_Plano

If ld_Total > 100.5 Then
	lb_Retorno = 1
ElseIf (ld_Cat1 > 100) Or (ld_Cat2 > 100) Or (ld_Plano > 100) Then
	lb_Retorno = 2
End If

Return lb_Retorno
end function

on w_maed_ctlcalplanillaespecies.create
int iCurrent
call super::create
this.tab_1=create tab_1
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.tab_1
end on

on w_maed_ctlcalplanillaespecies.destroy
call super::destroy
destroy(this.tab_1)
end on

event ue_seleccion;call super::ue_seleccion;String	ls_nula

SetNull(ls_nula)

istr_busq.argum[1]	=	String(dw_2.Object.plde_codigo[1])
istr_busq.argum[2]	=	String(ii_especie)
istr_busq.argum[4]	=	String(dw_2.Object.clie_codigo[1])

OpenWithParm(w_busc_ctlcalplanillaespecies, istr_busq)
istr_busq = Message.PowerObjectParm

If istr_busq.argum[3] <> "" Then
	This.TriggerEvent("ue_recuperadatos")
Else
	pb_buscar.SetFocus()
	HabilitaEncab(True)
	dw_2.Object.buscalote.visible = True
	dw_2.Object.ccre_numero[1] = Long(ls_nula)
	Recupera_Evaluacion(dw_2.Object.clie_codigo[1],0,0,ii_especie)
   	Recupera_Inspeccion(dw_2.Object.clie_codigo[1],0,0,ii_especie)
   	Recupera_Distribucion(dw_2.Object.clie_codigo[1],0,0,ii_especie,dw_2.Object.vari_codigo[1], 1)
	Recupera_Madurez(dw_2.Object.clie_codigo[1],0,0,ii_especie)
	Recupera_inspromedio(dw_2.Object.clie_codigo[1],0,0,ii_especie)
   	Recupera_firmeza(dw_2.Object.clie_codigo[1],0,0,ii_especie)
	Recupera_botrytis(dw_2.Object.clie_codigo[1],0,0,ii_especie)
	Recupera_materiaseca(dw_2.Object.clie_codigo[1],0,0,ii_especie)
	Recupera_segregacion(dw_2.Object.clie_codigo[1],0,ii_especie, 0, 0)
End If
	
		
		
end event

event ue_recuperadatos;Long 		ll_fila_e, ll_fildw3, ll_fildw4, ll_fildw5, ll_fildw6, ll_fildw7, respuesta, ll_fildw8, ll_fildw9, ll_fildw10, ll_fildw11, ll_fildw12

dw_2.SetTransObject(Sqlca)

Do
	dw_2.SetRedraw(False)
	dw_2.Reset()
	
	ll_fila_e	= dw_2.Retrieve(Long(istr_busq.argum[9]),Integer(istr_busq.argum[3]),Long(istr_busq.argum[1]),ii_especie)

	If ll_fila_e = -1 Then
		respuesta = MessageBox("Error en Base de Datos", "No es posible conectar la Base de Datos.", Information!, RetryCancel!)
	Else
		dw_2.Object.buscalote.visible = False
		
		dw_2.GetChild("plde_codigo", idwc_plantas)
		idwc_plantas.SetTransObject(sqlca)
		idwc_plantas.Retrieve(1,-1)
		
		dw_2.GetChild("vari_codigo", idwc_variedades)
		idwc_variedades.SetTransObject(sqlca)
		idwc_variedades.Retrieve(ii_especie)
		
		dw_2.GetChild("prod_codigo", idwc_productores) 
		idwc_productores.SetTransObject(sqlca)
		idwc_productores.Retrieve(dw_2.Object.zona_codigo[1])	
		
		dw_2.GetChild("prbr_codpre", idwc_predio)
		idwc_predio.SetTransObject(sqlca)
		idwc_predio.Retrieve(dw_2.Object.prod_codigo[1])
		
		dw_2.GetChild("prcc_codigo", idwc_cuartel)
		idwc_cuartel.SetTransObject(sqlca)
		idwc_cuartel.Retrieve(dw_2.Object.prod_codigo[1],dw_2.Object.prbr_codpre[1])
				
		If ll_fila_e > 0 Then
		   Do
				ll_fildw3 = Recupera_Evaluacion(dw_2.Object.clie_codigo[1],dw_2.Object.plde_codigo[1],dw_2.Object.ccre_numero[1],ii_especie)
				ll_fildw4 = Recupera_Inspeccion(dw_2.Object.clie_codigo[1],dw_2.Object.plde_codigo[1],dw_2.Object.ccre_numero[1],ii_especie)
				ll_fildw5 = Recupera_Distribucion(dw_2.Object.clie_codigo[1],dw_2.Object.plde_codigo[1],dw_2.Object.ccre_numero[1],ii_especie,dw_2.Object.vari_codigo[1], 1)
				ll_fildw6 = Recupera_Madurez(dw_2.Object.clie_codigo[1],dw_2.Object.plde_codigo[1],dw_2.Object.ccre_numero[1],ii_especie)
				ll_fildw7 = Recupera_inspromedio(dw_2.Object.clie_codigo[1],dw_2.Object.plde_codigo[1],dw_2.Object.ccre_numero[1],ii_especie)
				ll_fildw8 = Recupera_firmeza(dw_2.Object.clie_codigo[1],dw_2.Object.plde_codigo[1],dw_2.Object.ccre_numero[1],ii_especie)
				ll_fildw12 = Recupera_Segregacion(dw_2.Object.clie_codigo[1], dw_2.Object.plde_codigo[1],ii_especie,dw_2.Object.ccre_numero[1],0)
				
				If ii_especie = 41 Then
					Recupera_botrytis(dw_2.Object.clie_codigo[1],dw_2.Object.plde_codigo[1],dw_2.Object.ccre_numero[1],ii_especie)
					ll_fildw9	= Recupera_catguarda(dw_2.Object.clie_codigo[1], dw_2.Object.plde_codigo[1],ii_especie,dw_2.Object.ccre_numero[1],0)
					ll_fildw10	= Recupera_materiaseca(dw_2.Object.clie_codigo[1], dw_2.Object.plde_codigo[1], dw_2.Object.ccre_numero[1],ii_especie)
				Else
					ll_fildw9 = 0
					ll_fildw10 = 0 
				End If	
				
				If ii_especie = 82 Then 
					ll_fildw9		= Recupera_catguarda(dw_2.Object.clie_codigo[1], dw_2.Object.plde_codigo[1],ii_especie,dw_2.Object.ccre_numero[1],0)
					ll_fildw11		= Recupera_Alternaria(dw_2.Object.clie_codigo[1], dw_2.Object.plde_codigo[1],ii_especie,dw_2.Object.ccre_numero[1],0)
				End If
								
				If ll_fildw3 = -1 OR ll_fildw4 = -1 OR ll_fildw5 = -1 OR ll_fildw6 = -1 OR &
					ll_fildw7 = -1 OR ll_fildw8 = -1 OR ll_fildw9 = -1 Or ll_fildw10 = -1 Or ll_fildw11 = -1 Or ll_fildw12 = -1 Then	
					respuesta = MessageBox("Error en Base de Datos", "No es posible conectar la Base de Datos.", Information!, RetryCancel!)
				End If
				
				Tab_1.TabPage_9.mle_observacion.Text = dw_2.Object.ccre_observ[1]
				
				//ASIGNACION TIPO CAMARA
				If ii_especie = 41 Then
					If dw_2.Object.ccre_tipoat[1] =	'FC 1' Then
						Tab_1.TabPage_11.rb_fc1.Checked  = True	
					ElseIf dw_2.Object.ccre_tipoat[1] =	'AC 1' Then
						Tab_1.TabPage_11.rb_ac1.Checked  = True
					ElseIf dw_2.Object.ccre_tipoat[1] =	'FC 2' Then
						Tab_1.TabPage_11.rb_fc2.Checked  = True	
					ElseIf dw_2.Object.ccre_tipoat[1] =	'AC 2' Then
						Tab_1.TabPage_11.rb_ac2.Checked  = True
					End If
					
				Elseif ii_especie = 27 Or ii_especie = 36 Then
					If dw_2.Object.ccre_tipoat[1] =	'FC' Then
						Tab_1.TabPage_11.rb_fc1.Checked  = True
					elseif  dw_2.Object.ccre_tipoat[1] =	'AC' Then
						Tab_1.TabPage_11.rb_ac1.Checked  = True
					end if
				End If
				
				If ii_especie = 78 Then
					If dw_2.Object.ccre_tipore[1] = 3 Then
						dw_2.Object.t_13.Text				=	'% Fancy'
						dw_2.Object.t_19.Text				=	'% Choice / Stndr'
						dw_2.object.ccre_porxfa.Protect	= 0
						dw_2.object.ccre_porxfa.Color		=	0
						dw_2.object.ccre_porxfa.BackGround.Color	=	RGB(255,255,255)
					Else
						dw_2.Object.t_13.Text				=	'% Cat 1'
						dw_2.Object.t_19.Text				=	'% Cat 2'
						dw_2.object.ccre_porxfa.Protect	= 1
						dw_2.object.ccre_porxfa.Color	=	RGB(255,255,255)
						dw_2.object.ccre_porxfa.BackGround.Color	=	553648127
					End If
				End If
	
//				If ii_especie = 27 OR ii_especie = 26 OR ii_especie = 78 Then
//					If ll_fildw3 > 0 And ll_fildw4 > 0 And ll_fildw5 > 0 And ll_fildw6 > 0 And ll_fildw7 >0 And ll_fildw12 > 0 Then
				pb_imprimir.Enabled	= True
				pb_grabar.Enabled	= True
				pb_eliminar.Enabled	= True
				HabilitaEncab(True)
				
				dw_2.Object.ccre_numero.Protect				=	1
				dw_2.Object.plde_codigo.Protect				=	1
				dw_2.Object.zona_codigo.Protect				=	1
				dw_2.Object.prod_codigo.Protect				=	1
				dw_2.Object.vari_codigo.Protect				=	1
				
				dw_2.Object.ccre_numero.Color	=	RGB(255,255,255)
				dw_2.Object.plde_codigo.Color		=	RGB(255,255,255)
				dw_2.Object.zona_codigo.Color	=	RGB(255,255,255)
				dw_2.Object.prod_codigo.Color	=	RGB(255,255,255)
				dw_2.Object.vari_codigo.Color		=	RGB(255,255,255)
				
				dw_2.Object.ccre_numero.BackGround.Color	=	553648127
				dw_2.Object.plde_codigo.BackGround.Color		=	553648127
				dw_2.Object.zona_codigo.BackGround.Color	=	553648127
				dw_2.Object.prod_codigo.BackGround.Color		=	553648127
				dw_2.Object.vari_codigo.BackGround.Color		=	553648127
//					End If
//				ElseIf ii_especie = 81 Then
//					If ll_fildw4 > 0 And ll_fildw5 > 0 And ll_fildw6 > 0 Then	
//						pb_imprimir.Enabled	= True
//						pb_grabar.Enabled	= True
//						pb_eliminar.Enabled	= True
//						HabilitaEncab(True)
//						dw_2.Object.ccre_numero.Protect				=	1
//						dw_2.Object.plde_codigo.Protect					=	1
//						dw_2.Object.ccre_numero.BackGround.Color	=	553648127
//						dw_2.Object.plde_codigo.BackGround.Color	=	553648127
//					End If
//				ElseIf ii_especie = 41 Then
//					If ll_fildw3 > 0 And ll_fildw4 > 0 And ll_fildw5 > 0 And ll_fildw8 > 0 And ll_fildw9 > 0 Then	
//						pb_imprimir.Enabled	= True
//						pb_grabar.Enabled	= True
//						pb_eliminar.Enabled	= True
//						HabilitaEncab(True)
//						dw_2.Object.ccre_numero.Protect				=	1
//						dw_2.Object.plde_codigo.Protect					=	1
//						dw_2.Object.ccre_numero.BackGround.Color	=	553648127
//						dw_2.Object.plde_codigo.BackGround.Color	=	553648127
//					End If
//				ElseIf ii_especie = 21 Then
//						If ll_fildw3 > 0 And ll_fildw4 > 0 And ll_fildw5 > 0 And ll_fildw7 > 0 Then
//							pb_imprimir.Enabled	= True
//							pb_grabar.Enabled	= True
//							pb_eliminar.Enabled	= True
//							HabilitaEncab(True)
//							dw_2.Object.ccre_numero.Protect				=	1
//							dw_2.Object.plde_codigo.Protect					=	1
//							dw_2.Object.ccre_numero.BackGround.Color	=	553648127
//							dw_2.Object.plde_codigo.BackGround.Color	=	553648127
//						End If
//				ElseIf ii_especie = 23 Then
//					If ll_fildw4 > 0 And ll_fildw5 > 0 And ll_fildw7 > 0 Then	
//						pb_imprimir.Enabled	= True
//						pb_grabar.Enabled	= True
//						pb_eliminar.Enabled	= True
//						HabilitaEncab(True)
//						dw_2.Object.ccre_numero.Protect				=	1
//						dw_2.Object.plde_codigo.Protect					=	1
//						dw_2.Object.ccre_numero.BackGround.Color	=	553648127
//						dw_2.Object.plde_codigo.BackGround.Color	=	553648127
//					End If
//				ElseIf ii_especie = 82 Then
//					If ll_fildw4 > 0 And ll_fildw5 > 0 And ll_fildw7 > 0 And ll_fildw11 > 0 Then	
//						pb_imprimir.Enabled	= True
//						pb_grabar.Enabled	= True
//						pb_eliminar.Enabled	= True
//						HabilitaEncab(True)
//						dw_2.Object.ccre_numero.Protect				=	1
//						dw_2.Object.plde_codigo.Protect					=	1
//						dw_2.Object.ccre_numero.BackGround.Color	=	553648127
//						dw_2.Object.plde_codigo.BackGround.Color	=	553648127
//					End If					
//				Else					
//					If ll_fildw3 > 0 And ll_fildw4 > 0 And ll_fildw5 > 0 And ll_fildw8 > 0 Then	
//						pb_imprimir.Enabled	= True
//						pb_grabar.Enabled	= True
//						pb_eliminar.Enabled	= True
//						HabilitaEncab(True)
//						dw_2.Object.ccre_numero.Protect				=	1
//						dw_2.Object.plde_codigo.Protect					=	1
//						dw_2.Object.ccre_numero.BackGround.Color	=	553648127
//						dw_2.Object.plde_codigo.BackGround.Color	=	553648127
//					End If
//				End If
			Loop While respuesta = 1
	 End If
		dw_2.SetRedraw(True)
		If respuesta = 2 Then Close(This)
	End If
Loop While respuesta = 1

If respuesta = 2 Then Close(This)
end event

event ue_nuevo;Integer  	li_Grupo, li_null
Long		ll_modIf2, ll_modIf3, ll_modIf4, ll_modIf5, ll_modIf6, ll_modIf7, ll_modIf10, ll_modIf11, ll_modIf12, & 
       	  	ll_fildw3, ll_fildw4, ll_fildw5, ll_fildw6, ll_fildw7, ll_fildw8, ll_modIf8, ll_modIf13, &
			ll_modIf9, ll_fildw9, ll_fildw10, ll_fildw11, ll_fildw12

SetNull(li_null)
ib_ok	= True

istr_mant.Solo_Consulta	=	istr_mant.UsuarioSoloConsulta

If Not istr_mant.Solo_Consulta Then
	CHOOSE CASE wf_modIfica()
		CASE -1
			ib_ok = False
		CASE 0
			ll_modIf2	=	dw_2.GetNextModIfied(0, Primary!)
			ll_modIf3	=	dw_3.GetNextModIfied(0, Primary!)
			ll_modIf4	=	dw_4.GetNextModIfied(0, Primary!)	
			ll_modIf5	=	dw_5.GetNextModIfied(0, Primary!)		
			ll_modIf6	=	dw_6.GetNextModIfied(0, Primary!)
			ll_modIf7	=	dw_7.GetNextModIfied(0, Primary!)
			ll_modIf8	=	dw_8.GetNextModIfied(0, Primary!)
			ll_modIf10	=	dw_10.GetNextModIfied(0, Primary!)
			ll_modIf11	=	dw_11.GetNextModIfied(0, Primary!)
			ll_modIf12	=	dw_12.GetNextModIfied(0, Primary!)
			ll_modIf13	=	dw_13.GetNextModIfied(0, Primary!)
						
			If dw_3.RowCount() > 0 OR dw_4.RowCount() > 0 OR dw_5.RowCount() > 0 OR &
			    dw_6.RowCount() > 0 OR dw_7.RowCount() > 0 OR dw_8.RowCount() > 0 OR &
				dw_10.RowCount() > 0 Or dw_11.RowCount() > 0 Or dw_12.RowCount() > 0 Or &
				dw_13.RowCount() > 0 Then
				Choose Case MessageBox("Grabar registro(s)","Desea Grabar la información ?", Question!, YesNoCancel!)
					Case 1
						Message.DoubleParm = 0
						This.TriggerEvent("ue_guardar")
						If message.DoubleParm = -1 Then ib_ok = False
					Case 3
						ib_ok	= False
						Return 
				End Choose 
			End If
	End Choose 
End If

If Not ib_ok Then Return

dw_3.Reset()
dw_4.Reset()
dw_5.Reset()
dw_6.Reset()
dw_7.Reset()
dw_8.Reset()
dw_10.Reset()
dw_11.Reset()
dw_12.Reset()
dw_13.Reset()

dw_2.SetRedraw(False)
dw_2.Reset()
dw_2.InsertRow(0)
dw_2.SetRedraw(True)

Tab_1.TabPage_9.mle_Observacion.Text = ''

If ii_especie = 41 Then
	dw_9.Reset()
	dw_11.Reset()
End If

pb_grabar.Enabled		=	FALSE
pb_eliminar.Enabled		=	FALSE
pb_imprimir.Enabled		=	FALSE
dw_2.Enabled				=	TRUE
/*
Cambio hecha Por A.O. 04-04-2007
argumento y función
*/
istr_mant.argumento[5] = '0'
istr_mant.argumento[6] = '0'

If NOT ExisteMovimiento() AND gstr_parlote.codgen = 0 Then
	istr_mant.argumento[5] = '1'
End If

If gstr_parlote.codgen = 1 OR istr_mant.argumento[5] = '1' Then
	dw_2.Object.Buscalote.Visible = False
	 IF ii_especie = 41 OR ii_especie = 21 THEN
        dw_2.SetTabOrder ("lote_totnet", 235 ) 
     END IF
Else
	dw_2.Object.Buscalote.Visible = True
End If

dw_2.GetChild("vari_codigo", idwc_variedades)
idwc_variedades.SetTransObject(sqlca)
idwc_variedades.Retrieve(ii_especie)
idwc_variedades.SetSort("vari_nombre A")
idwc_variedades.Sort()

If ii_especie = 21 Then	
	dw_2.SetItem(1, "vari_codigo",li_null)
ElseIf ii_especie = 41 Then
	dw_2.Object.ccre_tipcal.values="45	1/42	2/" 	
	dw_2.SetItem(1, "vari_codigo",1)
ElseIf ii_especie = 27 Then
	dw_2.Object.ccre_tipcal.values="113	1/88	2/" 	
	dw_2.SetItem(1, "vari_codigo",1)
ElseIf ii_especie = 26 or  ii_especie = 36 or  ii_especie = 10 Then
	dw_2.Object.ccre_tipcal.values="5	1/4	2/" 	
	dw_2.SetItem(1, "vari_codigo",1)
ElseIf ii_especie = 81 Then
	dw_2.Object.ccre_tipcal.values="80	1/70	2/" 	
	dw_2.SetItem(1, "vari_codigo",1)
ElseIf ii_especie = 82 Then	
	dw_2.SetItem(1, "vari_codigo",1)
End If

dw_2.SetItem(1, "clie_codigo",gi_CodExport)
dw_2.SetItem(1, "zona_codigo",gi_codZona)

dw_2.GetChild("prod_codigo", idwc_productores)
idwc_productores.SetTransObject(sqlca)
idwc_productores.Retrieve(-1)//gi_codZona)
idwc_productores.SetSort("prod_nombre A")
idwc_productores.Sort()
dw_2.SetItem(1, "prod_codigo",li_null)
	
dw_2.GetChild("plde_codigo", idwc_plantas)
idwc_plantas.SetTransObject(sqlca)
idwc_plantas.Retrieve(1,-1)
idwc_plantas.SetSort("plde_nombre A")
idwc_plantas.Sort()
dw_2.SetItem(1, "plde_codigo",gi_codPlanta)

//Huerto
dw_2.GetChild("prbr_codpre", idwc_predio)
idwc_predio.SetTransObject(sqlca)
idwc_predio.Retrieve(-1)

//Cuartel
dw_2.GetChild("prcc_codigo", idwc_cuartel)
idwc_cuartel.SetTransObject(sqlca)
idwc_cuartel.Retrieve(-1,-1)

dw_2.GetChild("ccin_codigo", idwc_inspectores)
idwc_inspectores.SetTransObject(sqlca)
idwc_inspectores.Retrieve()
idwc_inspectores.SetSort("ccin_nombre A")
idwc_inspectores.Sort()

dw_2.Object.ccre_tipore[1] = 1
dw_2.Object.espe_codigo[1] = ii_especie

ll_fildw3 = Recupera_Evaluacion(dw_2.Object.clie_codigo[1],0,0,ii_especie)
ll_fildw4 = Recupera_Inspeccion(dw_2.Object.clie_codigo[1],0,0,ii_especie)
ll_fildw5 = Recupera_Distribucion(dw_2.Object.clie_codigo[1],0,0,ii_especie,-1, 1)
ll_fildw5 = Recupera_Distribucion(dw_2.Object.clie_codigo[1],0,0,ii_especie,dw_2.Object.vari_codigo[1], 0)
ll_fildw6 = Recupera_Madurez(dw_2.Object.clie_codigo[1],0,0,ii_especie)
ll_fildw7 = Recupera_inspromedio(dw_2.Object.clie_codigo[1],0,0,ii_especie)
ll_fildw8 = Recupera_firmeza(dw_2.Object.clie_codigo[1],0,0,ii_especie)

If ii_especie = 82 Then
	ll_fildw9		= Recupera_catguarda(dw_2.Object.clie_codigo[1],0,ii_especie,0,0)
	ll_fildw11	= Recupera_Alternaria(dw_2.Object.clie_codigo[1],0,ii_especie,0,0)
End If

If ii_especie = 41 Then
	Recupera_botrytis(dw_2.Object.clie_codigo[1],0,0,ii_especie)
	ll_fildw9		= Recupera_catguarda(dw_2.Object.clie_codigo[1],0,ii_especie,0,0)
	ll_fildw10	= Recupera_materiaseca(dw_2.Object.clie_codigo[1],0,0,ii_especie)
	
	Tab_1.TabPage_11.rb_fc1.Checked  = False
	Tab_1.TabPage_11.rb_ac1.Checked  = False
	Tab_1.TabPage_11.rb_fc2.Checked  = False
	Tab_1.TabPage_11.rb_ac2.Checked  = False
Else
	ll_fildw9 = 0
	ll_fildw10 = 0
End If

HabilitaEncab(True)

If ii_especie = 26 Or ii_especie = 27 Or ii_especie = 78 Or  ii_especie = 36 or  ii_especie = 10 Then
	ll_fildw12 = Recupera_Segregacion(dw_2.Object.clie_codigo[1],0,ii_especie,0,0)
	Tab_1.TabPage_11.rb_fc1.Checked  = False
	Tab_1.TabPage_11.rb_ac1.Checked  = False
	Tab_1.TabPage_11.rb_fc2.Checked  = False
	Tab_1.TabPage_11.rb_ac2.Checked  = False
	If ll_fildw3 = 0  Then
		Messagebox("Atención","Falta Ingresar Items de Evaluación en tabla respectiva", StopSign!, Ok!)
		dw_2.SetFocus()
		HabilitaEncab(True)
		Return
	ElseIf ll_fildw4 = 0 Then
		Messagebox("Atención","Falta Ingresar defectos de Calidad y Condición en tabla respectiva", StopSign!, Ok!)
		dw_2.SetFocus()
		Return
	End If
End If

dw_2.Object.ccre_numero.Protect					=	0
dw_2.Object.plde_codigo.Protect					=	0
dw_2.Object.ccre_numero.Color					=	0
dw_2.Object.plde_codigo.Color						=	0
dw_2.Object.ccre_numero.BackGround.Color	=	RGB(255,255,255)
dw_2.Object.plde_codigo.BackGround.Color		=	RGB(255,255,255)

istr_mant.argumento[1]	=	String(li_null)
istr_mant.argumento[2]	=	String(li_null)
//istr_mant.argumento[5]  =	String(li_null)
//istr_mant.argumento[6]  =	String(li_null)

li_Grupo = BuscaGrupo(Upper(Gstr_Us.Nombre))

If (li_Grupo > 2) Then
	pb_eliminar.Visible	=	False
	TriggerEvent('resize')
End If

dw_2.SetFocus()
dw_2.SetColumn("ccre_numero")
end event

event ue_nuevo_detalle;call super::ue_nuevo_detalle;//
end event

event open;/*
Falta Descripcion de Cada Argumento

Tipo de Recepción =====>		1	Huerto
						=====>		2	PreProceso
						=====>		3	Post Desverdizado
						=====>		4	Post Almacenaje	
*/

Integer li_null
SetNull(li_null)

x				= 0
y				= 0
This.Height	= 2520
im_menu		= m_principal

This.Icon										=	Gstr_apl.Icono
This.ParentWindow().ToolBarVisible	=	True
im_menu.Item[1].Item[6].Enabled		=	True
im_menu.Item[7].Visible					=	True

dw_3		=	Tab_1.TabPage_1.dw_evaluacion
dw_4		=	Tab_1.TabPage_2.dw_inspeccion
dw_5		=	Tab_1.TabPage_3.dw_distribucion
dw_6  	=  Tab_1.TabPage_4.dw_madurez
dw_7  	=  Tab_1.TabPage_5.dw_inspecpromedio
dw_8  	=  Tab_1.TabPage_6.dw_firmeza
dw_9  	=  Tab_1.TabPage_7.dw_catguarda
dw_10	=  Tab_1.TabPage_8.dw_botrytis
dw_11	=  Tab_1.TabPage_10.dw_materiaseca
dw_12	=  Tab_1.TabPage_12.dw_alternaria
dw_13	=  Tab_1.TabPage_13.dw_segregacion

dw_2.SetTransObject(sqlca)
dw_3.SetTransObject(sqlca)
dw_4.SetTransObject(sqlca)
dw_5.SetTransObject(sqlca)
dw_6.SetTransObject(sqlca)
dw_7.SetTransObject(sqlca)
dw_8.SettransObject(sqlca)
dw_9.SettransObject(sqlca)
dw_10.SettransObject(sqlca)
dw_11.SettransObject(sqlca)
dw_12.SettransObject(sqlca)
dw_13.SettransObject(sqlca)

dw_3.ModIfy("DataWindow.Footer.Height = 0")
dw_4.ModIfy("DataWindow.Footer.Height = 0")
dw_5.ModIfy("DataWindow.Footer.Height = 0")
dw_6.ModIfy("DataWindow.Footer.Height = 0")
dw_7.ModIfy("DataWindow.Footer.Height = 0")
dw_8.ModIfy("DataWindow.Footer.Height = 0")
dw_9.ModIfy("DataWindow.Footer.Height = 0")
dw_10.ModIfy("DataWindow.Footer.Height = 0")
dw_11.ModIfy("DataWindow.Footer.Height = 0")
dw_12.ModIfy("DataWindow.Footer.Height = 0")
dw_13.ModIfy("DataWindow.Footer.Height = 0")

ii_especie = Integer(Message.StringParm)

dw_2.SetRedraw(False)

Choose Case ii_especie
	Case 21
		This.Title	= "PLANILLA RECEPCION FRUTA A PROCESO : CEREZAS"
		dw_2.Object.ccre_camref.visible 		= True
		dw_2.Object.ccre_hrsdes.visible		= False
		dw_2.Object.HoraDespacho.visible 	= True
		dw_2.Object.HoraIngreso.visible 		= True
		dw_2.Object.ccre_hordes.visible		= True
		dw_2.Object.ccre_horing.visible		= True
		dw_2.object.ccre_reslot.visible 		= True
		dw_2.Object.t_13.visible 				= False
		dw_2.Object.ccre_porexp.visible 		= True
		dw_2.Object.ccre_expack.visible 		= False
		dw_2.Object.ccre_hoinfi.visible 		= True
		dw_2.Object.t_19.visible 				= True
		dw_2.Object.gb_1.visible 				= True
		dw_2.Object.lote1.visible 				= True
		dw_2.Object.horainspeccion.Visible	=	False
		dw_2.Object.horadespacho.Visible		=	True
		dw_2.Object.horaingreso.Visible		=	True
		dw_2.Object.llegacamion.Visible		=	False
		dw_2.Object.ccre_hordes.Visible		=	True
		dw_2.Object.ccre_horing.Visible		=	True
		dw_2.Object.ccre_expack.Visible		=	False
		dw_2.Object.t_14.Alignment			=	1
		dw_2.Object.ccre_tipore.visible 		=	True
		dw_2.Object.tipore.visible 				=	True
		dw_2.Object.ccre_hidroc.visible 		=	True
		dw_2.Object.t_12.visible 				=	True
		dw_2.Object.t_12.Text					=	'HidroCooler'
		dw_2.Object.t_4.Text						=	'Nº Bandejas'
		Tab_1.TabPage_5.Visible				=	True
		Tab_1.TabPage_5.dw_inspecpromedio.DataObject	= "dw_mues_ctlcalicalcondet"
		Tab_1.TabPage_5.Text					=	"P. internos"
		Tab_1.TabPage_3.Text					=	"Calibre"
		Tab_1.MoveTab(3,8)
		Tab_1.MoveTab(2,8)
		dw_2.SetTabOrder('ccre_horins', 121)
		dw_2.SetTabOrder('ccre_hoinfi', 122)
		dw_2.Object.t_25.visible 				=	False
		dw_2.Object.ccre_velpro.visible 		=	False
		dw_2.Object.temperatura.y 				=	376
		dw_2.Object.ccre_temper.y				=	376
		dw_2.Object.temperatura.x 				=	dw_2.Object.t_kgnetos.x
		dw_2.Object.ccre_temper.x				=	dw_2.Object.lote_totnet.x
		dw_2.Object.t_kgnetos.y 				=	296
		dw_2.Object.lote_totnet.y				=	296
		dw_2.Object.t_kgnetos.visible			=	True
		dw_2.Object.t_27.visible			         =	False
		dw_2.Object.lote_totnet.visible			=	True
		dw_2.Object.ccre_apguar.visible		=	False
		dw_2.Object.t_10.Text					=	'Zona Origen'
		dw_2.Object.t_8.Text						=	'Planta'
		dw_2.Object.t_7.Text						=	'Productor'
		dw_2.Object.t_2.Text						=	'Variedad'
		dw_2.Object.zona_codigo.y				=	216
		dw_2.Object.plde_codigo.y				=	136
		dw_2.Object.prod_codigo.y				=	296
		dw_2.Object.vari_codigo.y				=	366
		
		dw_2.Object.ccre_frubla.Visible 		=	True
		dw_2.Object.ccre_resflo.Visible 		=	True
		dw_2.Object.ccre_peduni.Visible 		=	True
		dw_2.Object.t_28.Visible			         =	True
		dw_2.Object.t_29.Visible			         =	True
		dw_2.Object.t_30.Visible			         =	True
		dw_2.Object.gb_2.Visible			   	=	True
		dw_2.Object.ccre_frubla.Protect 		=	0
		dw_2.Object.ccre_resflo.Protect 		=	0
		dw_2.Object.ccre_peduni.Protect 		=	0
		
		dw_2.Object.prbr_codpre.width		=	963
		dw_2.Object.prbr_codpre.y				=	442
		
		dw_2.SetTabOrder('ccre_numero', 10)
		dw_2.SetTabOrder('zona_codigo', 30)
		dw_2.SetTabOrder('plde_codigo', 20)
		dw_2.SetTabOrder('vari_codigo', 50)
		dw_2.SetTabOrder('prod_codigo', 40)
		dw_2.SetTabOrder('prbr_codpre', 60)
		dw_2.SetTabOrder('prcc_codigo', 70)
		dw_2.SetTabOrder('ccre_tipore', 80)
		dw_2.SetTabOrder('ccre_feccos', 90)
		dw_2.SetTabOrder('ccre_fecrec', 100)
		dw_2.SetTabOrder('ccre_hordes', 110)
		dw_2.SetTabOrder('ccre_horing', 120)
		dw_2.SetTabOrder('ccre_fecins', 130)
		dw_2.SetTabOrder('ccre_hoinfi', 140)
		dw_2.SetTabOrder('ccin_codigo', 150)
		dw_2.SetTabOrder('ccre_camref', 160)
		dw_2.SetTabOrder('lote_codigo', 170)
		dw_2.SetTabOrder('ccre_hidroc', 180)
		dw_2.SetTabOrder('ccre_tamlot', 190)
		dw_2.SetTabOrder('ccre_temper', 210)
	
	Case 41
		This.Title	= "PLANILLA RECEPCION FRUTA A PROCESO : KIWIS"
		dw_2.Object.lote_totnet.Visible  = True
		dw_2.Object.t_kgnetos.Visible    = True
		dw_2.Object.t_1.text = 'Nº Lote Granel'
		Tab_1.TabPage_1.Text 				= 'Brix'
		Tab_1.TabPage_6.Visible			= True
		Tab_1.TabPage_7.Visible			= True
		Tab_1.TabPage_8.Visible 			= True
		Tab_1.TabPage_10.Visible			= True
//		Tab_1.TabPage_11.Visible			= True		
		dw_8.visible 							= True
		dw_2.Object.ccre_hrsdes.visible 	= True
		dw_2.Object.t_14.visible 			= True
		dw_2.Object.ccre_porexp.visible 	= True
		dw_2.Object.t_13.visible 			= False
		dw_2.Object.ccre_expack.visible 	= False
		dw_2.Object.t_19.visible 			= True
		dw_2.Object.gb_1.visible 			= True
		dw_2.Object.ccre_reslot.visible 	= True
		dw_2.Object.t_15.visible 			=	False
		dw_2.Object.t_18.visible 			=	False
		dw_2.Object.t_22.visible 			=	False
		dw_2.Object.ccre_lluvia.Visible		=	False
		dw_2.Object.ccre_funpre.Visible	=	False
		dw_2.Object.ccre_aplhor.Visible	=	False
		
		dw_2.SetTabOrder('ccre_hrsdes', 119)
		dw_2.SetTabOrder('ccre_temper', 120)
		dw_2.SetTabOrder('ccre_lluvia', 0)
		dw_2.SetTabOrder('ccre_funpre', 0)
		dw_2.SetTabOrder('ccre_aplhor', 0)		
		
		dw_2.Object.temperatura.y 				=	Long(dw_2.Object.vari_codigo.y) + 10
		dw_2.Object.ccre_temper.y				=	Long(dw_2.Object.vari_codigo.y) + 10
		dw_2.Object.temperatura.x 				=	dw_2.Object.t_9.x
		dw_2.Object.ccre_temper.x				=	dw_2.Object.ccre_feccos.x
		
		//Tab_1.MoveTab(3,5)
		Tab_1.MoveTab(5,2)	//Calibres
		Tab_1.MoveTab(10,3)	//Materia Seca
		Tab_1.MoveTab(11,8)	// Categoria Guarda Huerto
		Tab_1.MoveTab(7,4)	// Firmeza
	Case 27
		This.Title	= "PLANILLA RECEPCION FRUTA A PROCESO : NARANJAS"
		dw_2.DataObject 						= 'dw_maed_ctlcalplanillaespecies_citricos'
		dw_4.DataObject						= 'dw_mues_ctlcalinspecalidadet_granados'
		dw_4.SetTransObject(sqlca)
		dw_4.Object.t_4.Text				=	'Choice'
		Tab_1.TabPage_4.Visible 			= True
		dw_6.visible 							= True
		Tab_1.TabPage_5.Visible 			= True
		dw_7.Visible 							= True
		Tab_1.TabPage_11.Visible 			= True
		Tab_1.TabPage_13.Visible 			= True
		dw_2.Object.t_13.Text				=	'% Fancy'
		dw_2.Object.t_19.Text				=	'% Choice'
		Tab_1.TabPage_11.gb_3.Text 		= "BRIX M. ESP"
		Tab_1.TabPage_11.rb_Ac1.Text 	= "No cumple"
		Tab_1.TabPage_11.rb_Fc1.Text 	= "Cumple"
		Tab_1.TabPage_11.text 				=	"BRIX M.~rESP"
		Tab_1.TabPage_11.rb_Fc1.y		=	Tab_1.TabPage_11.rb_Fc1.y + 80
		Tab_1.TabPage_11.rb_Ac1.y		=	Tab_1.TabPage_11.rb_Ac1.y + 80
		Tab_1.TabPage_11.rb_Fc2.Visible=	FALSE
		Tab_1.TabPage_11.rb_Ac2.Visible=	FALSE
		Tab_1.MoveTab(5,3)
		Tab_1.MoveTab(13,4)
		Tab_1.MoveTab(12,5)
		dw_3.Object.ctlcalevaluacion_ccev_descri_t.text = '~nColor'
		dw_3.Object.media.text				=	'~nPorcentaje'
		dw_3.Object.ccec_mues01.Format	=	'0'
		dw_3.Object.frutos.Format				=	'0'
		dw_3.Object.calc_porce.visible			= True
		dw_3.Object.tota_porc.visible			= True
	Case 78
		This.Title	= "PLANILLA RECEPCION FRUTA A PROCESO : LIMONES"
		dw_2.DataObject = 'dw_maed_ctlcalplanillaespecies_citricos'		
		dw_4.DataObject	=	'dw_mues_ctlcalinspecalidadet_granados'
		dw_4.SetTransObject(sqlca)
		dw_4.Object.t_4.Text				=	'Choice'
		Tab_1.TabPage_4.Visible 			= True
		dw_6.visible 							= True
		Tab_1.TabPage_5.Visible 			= True
		dw_7.Visible 							= True
		Tab_1.TabPage_13.Visible 			= True
		dw_2.Object.t_13.Text				=	'% Cat 1'
		dw_2.Object.t_19.Text				=	'% Cat 2'
		dw_2.object.ccre_porxfa.Protect	= 1
		dw_2.object.ccre_porxfa.BackGround.Color	=	553648127
		Tab_1.MoveTab(5,3)
		Tab_1.MoveTab(13,4)
		dw_3.Object.ctlcalevaluacion_ccev_descri_t.text = '~nColor'
		dw_3.Object.media.text				=	'~nPorcentaje'
		dw_3.Object.ccec_mues01.Format	=	'0'
		dw_3.Object.frutos.Format				=	'0'
		dw_3.Object.calc_porce.visible			= True
		dw_3.Object.tota_porc.visible			= True
	Case 10
		This.Title	= "PLANILLA RECEPCION FRUTA A PROCESO : LIMAS"		
		dw_2.DataObject = 'dw_maed_ctlcalplanillaespecies_citricos'
		dw_3.Object.ctlcalevaluacion_ccev_descri_t.text = '~nColor'
		Tab_1.TabPage_4.Text					=	"% Jugo"
		
		dw_3.Object.media.text					=	'~nPorcentaje'
		dw_3.Object.ccec_mues01.Format	=	'0'
		dw_3.Object.frutos.Format				=	'0'
		dw_2.Object.ccre_hrsdes.visible		= False
		dw_2.Object.t_14.visible 				= False
		dw_3.Object.calc_porce.visible			= True
		dw_3.Object.tota_porc.visible			= True
		Tab_1.TabPage_4.Visible 				= True
		dw_6.visible 								= True
		Tab_1.TabPage_5.Visible 				= True
		dw_7.Visible 								= True
		Tab_1.TabPage_13.Visible 				= True
		Tab_1.TabPage_1.Visible 				= False
		Tab_1.MoveTab(5,3)
		Tab_1.MoveTab(13,4)
	Case 26
		This.Title	= "PLANILLA RECEPCION FRUTA A PROCESO : CLEMENTINAS"		
		dw_2.DataObject = 'dw_maed_ctlcalplanillaespecies_citricos'
		dw_3.Object.ctlcalevaluacion_ccev_descri_t.text = '~nColor'
		dw_3.Object.media.text					=	'~nPorcentaje'
		dw_3.Object.ccec_mues01.Format	=	'0'
		dw_3.Object.frutos.Format				=	'0'
		dw_3.Object.calc_porce.visible			= True
		dw_3.Object.tota_porc.visible			= True
		Tab_1.TabPage_4.Visible 				= True
		dw_6.visible 								= True
		Tab_1.TabPage_5.Visible 				= True
		dw_7.Visible 								= True
		Tab_1.TabPage_13.Visible 				= True
		Tab_1.MoveTab(5,3)
		Tab_1.MoveTab(13,4)
	Case 36
		This.Title	= "PLANILLA RECEPCION FRUTA A PROCESO : MANDARINAS"		
		dw_2.DataObject = 'dw_maed_ctlcalplanillaespecies_citricos'
		dw_3.Object.ctlcalevaluacion_ccev_descri_t.text = '~nColor'
		dw_3.Object.media.text					=	'~nPorcentaje'
		dw_3.Object.ccec_mues01.Format	=	'0'
		dw_3.Object.frutos.Format				=	'0'
		Tab_1.TabPage_11.Visible 				= True
		dw_3.Object.calc_porce.visible			= True
		dw_3.Object.tota_porc.visible			= True
		Tab_1.TabPage_4.Visible 				= True
		dw_6.visible 								= True
		Tab_1.TabPage_5.Visible 				= True
		dw_7.Visible 								= True
		Tab_1.TabPage_13.Visible 				= True
		Tab_1.MoveTab(5,3)
		Tab_1.MoveTab(13,4)
		Tab_1.MoveTab(12,5)
		Tab_1.TabPage_11.gb_3.Text 		= "BRIX M. ESP"
		Tab_1.TabPage_11.rb_Ac1.Text 	= "No cumple"
		Tab_1.TabPage_11.rb_Fc1.Text 	= "Cumple"
		Tab_1.TabPage_11.text 				=	"BRIX M.~rESP"
		Tab_1.TabPage_11.rb_Fc1.y		=	Tab_1.TabPage_11.rb_Fc1.y + 80
		Tab_1.TabPage_11.rb_Ac1.y		=	Tab_1.TabPage_11.rb_Ac1.y + 80
		Tab_1.TabPage_11.rb_Fc2.Visible=	FALSE
		Tab_1.TabPage_11.rb_Ac2.Visible=	FALSE
	Case 81
		This.Title	= "PLANILLA RECEPCION FRUTA A PROCESO : PALTAS"	
		Tab_1.TabPage_5.Visible 			=	False
		Tab_1.TabPage_1.Visible 			=	False
		Tab_1.TabPage_4.Visible 			=	True
		dw_2.Object.ccre_hrsdes.visible 	=	False
		dw_2.Object.t_13.Visible 			=	True
		dw_2.Object.ccre_expack.visible 	=	True
		dw_2.Object.t_14.visible 			=	False
		dw_2.Object.ccre_fecins.visible 	=	True
		dw_2.Object.t_24.visible 			=	True
		dw_2.Object.ccre_porexp.visible 	=	True
		dw_2.Object.t_19.visible 			=	True
		dw_6.Visible 							=	True		
		dw_2.Object.ccre_reslot.Visible	=	True
		dw_2.Object.gb_1.Visible			=	True
		Tab_1.TabPage_4.text 				=	"Calculo Porcentaje~r Aceite"
		dw_6.DataObject 						=	'dw_mues_ctlcalevalumaducolor_det_pal'
		dw_6.SetTransObject(sqlca)
		
	Case 82
		This.Title	= "PLANILLA RECEPCION FRUTA A PROCESO : GRANADOS"
		
		dw_4.DataObject	=	'dw_mues_ctlcalinspecalidadet_granados'
		dw_4.SetTransObject(Sqlca)
		dw_3.Object.calc_porce.visible	= True
		dw_3.Object.tota_porc.visible	= True
		dw_2.Object.ccre_hrsdes.visible 	= False
		dw_2.Object.t_14.visible 			= True
		/* Modificado 2012-04-20 solicitado por VCosta Correo 09-04-2012
		dw_2.Object.ccre_porexp.visible 	= False		
		dw_2.Object.t_19.visible 			= False
		dw_2.Object.t_19.Text				=	'% Cat 1'		
		dw_2.Object.ccre_expack.visible 	= False
		dw_2.Object.t_13.visible 			= False
		dw_2.Object.t_13.Text				=	'% Cat 2'
		dw_2.Object.ccre_excome.visible = False
		dw_2.Object.fechapre.visible 		= False
		dw_2.Object.fechapre.Text		=	'% Comercial'
		*/
		dw_3.Object.ccec_mues01.Format	=	'0'
		dw_3.Object.frutos.Format				=	'0'
		Tab_1.TabPage_1.text 				= "Color"
		//Tab_1.TabPage_2.Text 			= 'Calidad y ~nCondicion'
		Tab_1.TabPage_3.Text 				= "Calibre"
		Tab_1.TabPage_5.Text 				= "Evaluación de~nMadurez"
		Tab_1.TabPage_7.Text 				= "Clasificación~nCategorias"
		dw_3.Object.media.text				=	'~nPorcentaje'
		Tab_1.TabPage_5.Visible			= True
		Tab_1.TabPage_6.Visible			= False
		Tab_1.TabPage_7.Visible 			= True
		Tab_1.TabPage_12.Visible 			= True
		dw_8.visible 							= True
		dw_2.Object.gb_1.visible 			= True
		dw_2.Object.ccre_reslot.visible 	= True
		Tab_1.MoveTab(2,8)
		Tab_1.MoveTab(4,2)
		Tab_1.MoveTab(7,5)
		Tab_1.MoveTab(7,5)
		Tab_1.MoveTab(12,8)
		
	Case 23
		This.Title	= "PLANILLA RECEPCION FRUTA A PROCESO : CIRUELAS"
//		dw_4.DataObject	=	'dw_mues_ctlcalinspecalidadet_ciruelas'
//		dw_4.SetTransObject(Sqlca)
		dw_7.DataObject	=	'dw_mues_ctlcalinspecalicondi_det_ciruelas'
		dw_7.SetTransObject(Sqlca)
		
		Tab_1.TabPage_1.Visible 			= False
		Tab_1.TabPage_2.Visible 			= True
		Tab_1.TabPage_3.Visible 			= True
		Tab_1.TabPage_4.Visible 			= False
		Tab_1.TabPage_5.Visible 			= True
		Tab_1.TabPage_6.Visible 			= False
		Tab_1.TabPage_7.Visible 			= False
		Tab_1.TabPage_8.Visible 			= False
		
		dw_2.Object.t_13.Visible 			= True
		dw_2.Object.t_19.Visible 			= True
		dw_2.Object.fechapre.Visible 		= True
		dw_2.Object.ccre_expack.Visible	= True
		dw_2.Object.ccre_porexp.Visible	= True
		dw_2.Object.ccre_excome.Visible	= True
		
		dw_2.Object.t_19.x					= dw_2.Object.temperatura.x
		dw_2.Object.t_19.y					= dw_2.Object.temperatura.y
		dw_2.Object.ccre_porexp.Width	= dw_2.Object.ccre_temper.Width
		dw_2.Object.ccre_porexp.x			= dw_2.Object.ccre_temper.x
		dw_2.Object.ccre_porexp.y			= dw_2.Object.ccre_temper.y

//		dw_2.Object.t_19.Text				= '% Cat 1'
//		dw_2.Object.t_13.Text 				= '% Cat 2'
//		dw_2.Object.fechapre.Text			= '% Comercial'

		dw_2.Object.gb_2.Visible				=	True
		dw_2.Object.gb_2.Text					=	"Color de Cubrimiento"
		dw_2.Object.gb_2.Width					=	1700
		
		dw_2.Object.ccre_pocafe.Visible		=	True
		dw_2.Object.ccre_pocat1.Visible		=	True
		dw_2.Object.ccre_pocat2.Visible		=	True
		dw_2.Object.ccre_poplan.Visible		=	True
		
		dw_2.Object.t_31.Visible		=	True
		dw_2.Object.t_32.Visible		=	True
		dw_2.Object.t_33.Visible		=	True
		dw_2.Object.t_34.Visible		=	True
		
		dw_2.Object.t_31.Text		=	"<40%"
		dw_2.Object.t_32.Text		=	"40-60%"
		dw_2.Object.t_33.Text		=	"60-80%"
		dw_2.Object.t_34.Text		=	">80%"

		dw_2.Object.t_13.Visible				=	False
		dw_2.Object.fechapre.Visible		=	False
		dw_2.Object.ccre_expack.Visible	=	False
		dw_2.Object.ccre_excome.Visible	=	False
		
		dw_2.SetTabOrder('ccre_porexp', 221)
		
		dw_2.Object.t_14.visible 			= False
		dw_2.Object.gb_1.Visible			= True
		dw_2.Object.ccre_reslot.visible 	= True
		dw_2.Object.ccre_hrsdes.visible 	= False
		dw_2.Object.temperatura.visible 	= False
		dw_2.Object.ccre_temper.visible 	= False
		Tab_1.TabPage_5.text				= "Firmeza y Brix"
		Tab_1.MoveTab(5,2)					
	End Choose	

dw_2.SetRedraw(True)

iuo_zonas       		= Create  uo_zonas
iuo_plantas     		= Create  uo_plantadesp
iuo_variedades  	= Create  uo_variedades 
iuo_productor   	= Create  uo_productores
iuo_inspectores  	= Create  uo_ctlcalinspectores
iuo_especies   		= Create  uo_especie
iuo_prodcuarteles	= Create  uo_prodcuarteles
iuo_prodpredio    	= Create  uo_prodpredio

ds_porespecie	   	= Create  DataStore
ds_firmeza     	  	= Create  DataStore

istr_mant.UsuarioSoloConsulta	=	OpcionSoloConsulta()

pb_nuevo.PostEvent(Clicked!)

GrabaAccesoAplicacion(True, id_FechaAcceso, it_HoraAcceso, This.Title, "Acceso a Aplicación", 1)

//Zona
dw_2.GetChild("zona_codigo", idwc_zonas)
idwc_zonas.SetTransObject(sqlca)
If idwc_zonas.Retrieve() = 0 Then
	idwc_zonas.InsertRow(0)
End If

If ii_especie = 41 Then
	dw_2.Object.horadespacho.Visible	=	False
	dw_2.Object.horaingreso.Visible		=	False
	dw_2.Object.ccre_hordes.Visible		=	False
	dw_2.Object.ccre_horing.Visible		=	False
	dw_2.Object.horainspeccion.Visible	=	False
	dw_2.Object.llegacamion.Visible		=	False
	
	dw_2.Object.gb_2.Visible				=	True
	dw_2.Object.gb_2.Width					=	2075
	
	dw_2.Object.ccre_pocafe.Visible		=	True
	dw_2.Object.ccre_pocat1.Visible		=	True
	dw_2.Object.ccre_pocat2.Visible		=	True
	dw_2.Object.ccre_poplan.Visible		=	True
	dw_2.Object.ccre_acidez.Visible		=	True
	
	dw_2.Object.t_31.Visible		=	True
	dw_2.Object.t_32.Visible		=	True
	dw_2.Object.t_33.Visible		=	True
	dw_2.Object.t_34.Visible		=	True
	dw_2.Object.t_35.Visible		=	True
	
ElseIf ii_especie = 81 Then
	dw_2.Object.ccre_hordes.Visible		=	True
	dw_2.Object.ccre_horing.Visible		=	True
	dw_2.Object.horainspeccion.Visible	=	True
	dw_2.Object.horadespacho.Visible	=	False
	dw_2.Object.llegacamion.Visible		=	True
	dw_2.Object.horaingreso.Visible		=	False
	dw_2.Object.t_22.Visible				=	False
	dw_2.Height								=	700
ElseIf ii_Especie = 82 Then 
	dw_2.Object.horaingreso.Text			=	'Hora Recepción'
	dw_2.Object.horaingreso.Visible		=	True
	dw_2.Object.ccre_horing.Visible		=	True
	dw_2.Object.horaingreso.y				=	216
	dw_2.Object.ccre_horing.y				=	216
	
	dw_2.Object.t_24.Visible				=	True
	dw_2.Object.ccre_fecins.Visible		=	True
	dw_2.Object.t_24.y						=	296
	dw_2.Object.ccre_fecins.y				=	296
	
	dw_2.Object.horainspeccion.Visible	=	True
	dw_2.Object.horainspeccion.Text		= 'Hora Inspección'
	dw_2.Object.horainspeccion.y			=	376
	dw_2.Object.ccre_horins.Visible		=	True
	
	dw_2.Object.t_14.Visible				=	False
	dw_2.Object.horadespacho.Visible	=	False
	dw_2.Object.ccre_hordes.Visible		=	False
	dw_2.Object.llegacamion.Visible		=	False
	
	dw_2.SetTabOrder('ccre_horing', 111)
	dw_2.SetTabOrder('ccre_hordes', 112)
	
ElseIf ii_Especie = 27  Or ii_Especie = 26 Or ii_Especie = 36 Or ii_Especie = 10 Then 
	
ElseIf ii_Especie = 78 Then 
	dw_2.Object.lote1.Visible				=	True
	dw_2.Object.ccre_loteo1.Visible		=	True
	dw_2.Object.ccre_loteo2.Visible		=	True
	dw_2.Object.ccre_loteo3.Visible		=	True
	dw_2.Object.ccre_loteo4.Visible		=	True
	dw_2.Object.ccre_loteo5.Visible		=	True
	dw_2.Object.ccre_colorl.Visible		=	True
Elseif ii_Especie <> 21 Then 
	dw_2.Object.horadespacho.Visible	=	False
	dw_2.Object.horaingreso.Visible		=	False
	dw_2.Object.ccre_hordes.Visible		=	False
	dw_2.Object.ccre_horing.Visible		=	False
	dw_2.Object.horainspeccion.Visible	=	False
	dw_2.Object.llegacamion.Visible		=	False
End If

idwc_zonas.SetSort("zona_nombre A")
idwc_zonas.Sort()
dw_2.SetItem(1, "zona_codigo",gi_codZona)

//Planta
dw_2.GetChild("plde_codigo", idwc_plantas)
idwc_plantas.SetTransObject(sqlca)
/*No filtra planta por zona, solicitado por V.C.*/
idwc_plantas.Retrieve(1,-1)
//idwc_plantas.Retrieve(1,gi_codzona)
idwc_plantas.SetSort("plde_nombre A")
idwc_plantas.Sort()
dw_2.SetItem(1, "plde_codigo",gi_codPlanta)	
	
//Productor
dw_2.GetChild("prod_codigo", idwc_productores)
idwc_productores.SetTransObject(sqlca)
idwc_productores.Retrieve(-1)
idwc_productores.SetSort("prod_nombre A")
idwc_productores.Sort()
dw_2.SetItem(1, "prod_codigo",li_null)
	
//Huerto
dw_2.GetChild("prbr_codpre", idwc_predio)
idwc_predio.SetTransObject(sqlca)
If idwc_predio.Retrieve(0) = 0 Then
	idwc_predio.InsertRow(0)
End If
dw_2.SetItem(1, "prbr_codpre",li_null)

//Cuartel
dw_2.GetChild("prcc_codigo", idwc_cuartel)
idwc_cuartel.SetTransObject(sqlca)
If idwc_cuartel.Retrieve(0,0) = 0 Then
	idwc_cuartel.InsertRow(0)
End If
dw_2.SetItem(1, "prcc_codigo",li_null)

//Especie
dw_2.GetChild("espe_codigo", idwc_especies)
idwc_especies.SetTransObject(sqlca)
idwc_especies.Retrieve()

If ii_especie = 26 Then
	idwc_especies.SetFilter("Left(espe_nombre,2) = 'CL' " + " OR  Left(espe_nombre,2) ='LI' " )
   	idwc_especies.Filter() 
	dw_2.SetItem(1, "espe_codigo",li_null)
ElseIf ii_especie = 25 Then
	idwc_especies.SetFilter("Left(espe_nombre,2) = 'DU' " + " OR  Left(espe_nombre,2) ='NE' " + " OR  left(espe_nombre,2) ='CI'")
	idwc_especies.Filter()
	dw_2.SetItem(1, "espe_codigo",li_null)
Else
	dw_2.SetItem(1, "espe_codigo",ii_especie)
	dw_2.Object.espe_codigo.Protect					=	1
	dw_2.Object.espe_codigo.Color					=	RGB(255,255,255)
	dw_2.Object.espe_codigo.BackGround.Color	=	553648127
End If

idwc_especies.SetSort("espe_nombre A")
idwc_especies.Sort()

//Variedad
dw_2.GetChild("vari_codigo", idwc_variedades)
idwc_variedades.SetTransObject(sqlca)
idwc_variedades.Retrieve(ii_especie)
idwc_variedades.SetSort("vari_nombre A")
idwc_variedades.Sort()
dw_2.SetItem(1, "vari_codigo",li_null)

//Inspectores
dw_2.GetChild("ccin_codigo", idwc_inspectores)
idwc_inspectores.SetTransObject(sqlca)
idwc_inspectores.Retrieve()
idwc_inspectores.SetSort("ccin_nombre A")
idwc_inspectores.Sort()
idwc_inspectores.InsertRow(0)

istr_mant.argumento[1] = '0'
istr_mant.argumento[2] = '0'
istr_mant.argumento[3] = '0'
istr_mant.argumento[4] = '0'
istr_mant.argumento[5] = '0'
istr_mant.argumento[6] = '0'

If NOT ExisteMovimiento() AND gstr_parlote.codgen = 0 Then
	istr_mant.argumento[5] = '1'
End If

If gstr_parlote.codgen = 1 OR istr_mant.argumento[5] = '1' Then
	dw_2.Object.buscalote.Visible = False
Else
	dw_2.Object.buscalote.Visible = True
End If

dw_2.SetTransObject(sqlca)
/*
Al eliminarse la columna calibre minimo, el porcentaje 
exportación se debe habilitar por lo tanto no será necesario
ocupar esta función. Todo esto solicitado por Veronica Costa 24/04/2007

deshabilita_porexp(ii_especie)
*/
end event

event ue_modifica_detalle;//IF dw_1.RowCount()>0 THEN
//	istr_mant.Agrega = False
//	istr_mant.Borra  = False	
//	OpenWithParm(iw_mantencion,istr_mant)
//END IF
//
//IF dw_1.GetItemStatus(il_Fila,0,Primary!) = DataModified! OR & 
//	dw_1.GetItemStatus(il_Fila,0,Primary!) = NotModified! THEN
//	istr_mant.Argumento[22]	=	'1'
//END IF
//
end event

event ue_borra_detalle;//IF dw_1.rowcount() < 1 THEN RETURN
end event

event ue_imprimir;Long		fila

DataWindowChild  dwc_packing

istr_info.titulo	=	"INFORME DE RECEPCIÓN DE FRUTA A PROCESO"
istr_info.copias	=	1

OpenWithParm(vinf,istr_info)

If Not iuo_Especies.Existe(dw_2.Object.espe_codigo[1], True, Sqlca) Then Return
If Not iuo_prodpredio.Existepredioprod(dw_2.Object.prod_codigo[1], dw_2.Object.prbr_codpre[1], True, Sqlca) Then Return
If Not iuo_prodcuarteles.Existe(dw_2.Object.prod_codigo[1], dw_2.Object.prbr_codpre[1],  dw_2.Object.prcc_codigo[1], True, Sqlca) Then Return
			  
If iuo_Especies.Codigo = 21 Then	//Cerezas
	// Problema en dw_info_recepcion_condicion_cereza
	vinf.dw_1.DataObject	=	"dw_info_planillarecepespenc"
	vinf.dw_1.Modify('DataWindow.Zoom = 100')
ElseIf iuo_Especies.Codigo = 23 Then //Ciruelas
	vinf.dw_1.DataObject	=	"dw_info_recepcion_ciruelas"
//	vinf.dw_1.Modify('DataWindow.Zoom = 92')
ElseIf iuo_Especies.Codigo = 41 Then //Kiwis
	//	vinf.dw_1.DataObject	=	"dw_info_recepcion_kiwis" 
	//  Problema en dw_info_ctlcalinpecevaluadetkiwi
	vinf.dw_1.DataObject	=	"dw_info_planillarecepkiwisenc" 
ElseIf iuo_Especies.Codigo = 26 or iuo_Especies.Codigo = 36 Then //Clementinas - Mandarinas - Lima
	vinf.dw_1.DataObject	=	"dw_info_recepcion_nara"
ElseIf iuo_Especies.Codigo = 10 Then //Lima
	vinf.dw_1.DataObject	=	"dw_info_recepcion_lima"
ElseIf iuo_Especies.Codigo = 27 Then //Naranjas 
	vinf.dw_1.DataObject	=	"dw_info_recepcion_nara1"
//	vinf.dw_1.Modify('DataWindow.Zoom = 98')
ElseIf iuo_Especies.Codigo = 78 Then //Limones
	vinf.dw_1.DataObject	=	"dw_info_planillarecepnaraenc1"
ElseIf iuo_Especies.Codigo = 81 Then //Paltas
	vinf.dw_1.DataObject	=	"dw_info_recepcion_paltas"
ElseIf iuo_Especies.Codigo = 82 Then //Granadas
	vinf.dw_1.DataObject	=	"dw_info_recepcion_granados"
	vinf.dw_1.Modify('DataWindow.Zoom = 90')
End If

vinf.dw_1.SetTransObject(sqlca)

If iuo_Especies.Codigo = 23 Then
	fila	=	vinf.dw_1.Retrieve(dw_2.Object.clie_codigo[1], dw_2.Object.plde_codigo[1], dw_2.Object.ccre_numero[1],iuo_Especies.Codigo, &
							-1, -1, -1, -1, -1, Date('19000101'), Today(), 1)
ElseIf iuo_Especies.Codigo = 82 Then
	fila	=	vinf.dw_1.Retrieve(dw_2.Object.clie_codigo[1], dw_2.Object.plde_codigo[1], dw_2.Object.ccre_numero[1],iuo_Especies.Codigo, &
							-1, -1, -1, -1, -1, Date('19000101'), Today())
ElseIf iuo_Especies.Codigo =41 Then
	fila = vinf.dw_1.Retrieve(dw_2.Object.clie_codigo[1], iuo_Especies.Codigo, -1, -1, dw_2.Object.plde_codigo[1], -1, &
				-1, Date('19000101'), Today(), dw_2.Object.ccre_numero[1], 1)
ElseIf iuo_Especies.Codigo = 21 Then
	fila = vinf.dw_1.Retrieve(dw_2.Object.clie_codigo[1], iuo_Especies.Codigo, -1, -1, dw_2.Object.plde_codigo[1], -1, &
				-1, Date('19000101'), Today(), dw_2.Object.ccre_tipore[1], dw_2.Object.ccre_numero[1], 1)
ElseIf iuo_Especies.Codigo = 78 Then
	fila = vinf.dw_1.Retrieve(dw_2.Object.clie_codigo[1], iuo_Especies.Codigo, -1, -1, dw_2.Object.plde_codigo[1], -1, &
				-1, Date('19000101'), Today(), dw_2.Object.ccre_tipore[1], dw_2.Object.ccre_numero[1], 1)
Else
	fila	=	vinf.dw_1.Retrieve(dw_2.Object.clie_codigo[1], dw_2.Object.plde_codigo[1], dw_2.Object.ccre_numero[1],iuo_Especies.Codigo)
End If

If fila	=	-1 Then
	MessageBox("Error en Base de Datos", "Se ha producido un error en Base de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ElseIf fila	=	0 Then
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
Else
	F_Membrete(vinf.dw_1)
	vinf.dw_1.Modify("predio.text = '" + iuo_prodPredio.Nombre + "'")
	vinf.dw_1.Modify("cuartel.text = '" +  iuo_prodcuarteles.Nombre + "'")
	If gs_Ambiente <> 'Windows' Then F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
End If
end event

event resize;call super::resize;Integer	maximo, li_posic_x, li_posic_y, li_visible = 0

IF dw_2.width > il_AnchoDw_1 THEN
	maximo		=	dw_2.width
ELSE
	Tab_1.width	=	This.WorkSpaceWidth() - 400
	maximo		=	Tab_1.width
END IF

dw_2.x					= 37 + Round((This.WorkSpaceWidth() - dw_2.width - 400) / 2, 0)
dw_2.y					= 37

Tab_1.width			= This.WorkSpaceWidth() - 400

Tab_1.x					= 37 + Round((This.WorkSpaceWidth() - 400 - Tab_1.width ) / 2, 0)
Tab_1.y					= 64 + dw_2.Height

Tab_1.Height			= This.WorkSpaceHeight() - Tab_1.y - 41

Tab_1.TabPage_1.dw_evaluacion.Height		=	Tab_1.Height -	250
Tab_1.TabPage_2.dw_inspeccion.Height		=	Tab_1.Height -	250
Tab_1.TabPage_3.dw_distribucion.Height		=	Tab_1.Height -	250
Tab_1.TabPage_5.dw_inspecpromedio.Height	=	Tab_1.Height -	250
Tab_1.TabPage_4.dw_madurez.Height			=	Tab_1.Height -	250
Tab_1.TabPage_6.dw_firmeza.Height			=	Tab_1.Height -	250
Tab_1.TabPage_7.dw_catguarda.Height		=	Tab_1.Height -	250
Tab_1.TabPage_8.dw_botrytis.Height			=	Tab_1.Height -	250
Tab_1.TabPage_9.mle_observacion.Height		=	Tab_1.Height -	250
Tab_1.TabPage_10.dw_materiaseca.Height	=	Tab_1.Height -	250
Tab_1.TabPage_12.dw_alternaria.Height		=	Tab_1.Height -	250
Tab_1.TabPage_13.dw_segregacion.Height	=	Tab_1.Height -	250

Tab_1.TabPage_1.dw_evaluacion.Width		=	Tab_1.Width -	200
Tab_1.TabPage_2.dw_inspeccion.Width			=	Tab_1.Width -	200
Tab_1.TabPage_3.dw_distribucion.Width		=	Tab_1.Width -	200
Tab_1.TabPage_5.dw_inspecpromedio.Width	=	Tab_1.Width -	200
Tab_1.TabPage_4.dw_madurez.Width			=	Tab_1.Width -	200
Tab_1.TabPage_6.dw_firmeza.Width				=	Tab_1.Width -	200
Tab_1.TabPage_7.dw_catguarda.Width			=	Tab_1.Width -	200
Tab_1.TabPage_8.dw_botrytis.Width			=	Tab_1.Width -	200
Tab_1.TabPage_9.mle_observacion.Width		=	Tab_1.Width -	200
Tab_1.TabPage_10.dw_materiaseca.Width		=	Tab_1.Width -	200
Tab_1.TabPage_12.dw_alternaria.Width		=	Tab_1.Width -	200
Tab_1.TabPage_13.dw_segregacion.Width		=	Tab_1.Width -	200
end event

event ue_antesguardar;Long		ll_fil1, ll_fil2, ll_fil3 , ll_fil4, ll_fil5, ll_fila,ll_filexp, ll_fil6, ll_Fil7 ,ll_fil10, ll_fil11, ll_fil12, ll_fil13
Integer	li_cont, li_existeError, li_suma, li_Calibre, li_resultado
String		ls_Mensaje, ls_colu[],ls_Null, ls_Calibre
Decimal{2} ld_valexpor, suma_porc

SetNull(ls_Null)

If dw_2.RowCount() > 0 Then	
	If ii_Especie = 21 Then
		If IsNull(dw_2.Object.ccre_camref[1]) Then
			li_cont ++
			ls_mensaje 		= ls_mensaje + "~nCamión Termo"
			ls_colu[li_cont]	= "ccre_camref"
		End If	
	End If
	
	If ii_Especie = 27 Or ii_Especie = 78 Then //Or ii_Especie = 23 Then
			If Not wf_ValidaPorcentaje('', '0', 0) Then
				MessageBox('Atención...', 'El porcentaje total debe ser igual 100%', Information!, Ok!)
				Message.DoubleParm = -1
				Return 
			End If
		End If
	
	If ii_Especie =  81 Then
		If IsNull(dw_2.Object.ccre_hordes[1]) Or dw_2.Object.ccre_hordes[1] = Time('00:00:00') Then
			If MessageBox('Atencion', "Hora Llegada esta en 00:00~nDesea Continuar", Exclamation!, YesNo!, 2) = 2 Then
				Message.DoubleParm = -1
				Return
			End If
		End If
	
		If IsNull(dw_2.Object.ccre_horing[1]) Or dw_2.Object.ccre_horing[1] = Time('00:00:00')  Then
			If MessageBox('Atencion', "Hora Inspección esta en 00:00~nDesea Continuar", Exclamation!, YesNo!, 2) = 2 Then
				Message.DoubleParm = -1
				Return
			End If
		End If
	End If
	
	If IsNull(dw_2.Object.clie_codigo[1]) Or dw_2.Object.clie_codigo[1] = 0 Then
		li_cont ++
		ls_mensaje 			= ls_mensaje + "~nCliente"
		ls_colu[li_cont]	= "clie_codigo"
	End If	
	
	If IsNull(dw_2.Object.ccre_numero[1]) Or dw_2.Object.ccre_numero[1] = 0 Then
		li_cont ++
		ls_mensaje 			= ls_mensaje + "~nNº Planilla"
		ls_colu[li_cont]	= "ccre_numero"
	End If
	
	If IsNull(dw_2.Object.zona_codigo[1]) Or dw_2.Object.zona_codigo[1] = 0 Then
		li_cont ++
		ls_mensaje 			= ls_mensaje + "~nZona"
		ls_colu[li_cont]	= "zona_codigo"
	End If	
	
	If IsNull(dw_2.Object.plde_codigo[1]) Or dw_2.Object.plde_codigo[1] = 0 Then
		li_cont ++
		ls_mensaje 			= ls_mensaje + "~nPlanta"
		ls_colu[li_cont]	= "plde_codigo"
	End If	
	
	If IsNull(dw_2.Object.espe_codigo[1]) Or dw_2.Object.espe_codigo[1] = 0 Then
		li_cont ++
		ls_mensaje 		= ls_mensaje + "~nEspecie"
		ls_colu[li_cont]	= "espe_codigo"
	End If
	
	If IsNull(dw_2.Object.vari_codigo[1]) Or dw_2.Object.vari_codigo[1] = 0 Then
		li_cont ++
		ls_mensaje 		= ls_mensaje + "~nVariedad"
		ls_colu[li_cont]	= "vari_codigo"
	End If
	
	If IsNull(dw_2.Object.prod_codigo[1]) Or dw_2.Object.prod_codigo[1] = 0 Then
		li_cont ++
		ls_mensaje 		= ls_mensaje + "~nProductor"
		ls_colu[li_cont]	= "prod_codigo"
	End If
	
	If IsNull(dw_2.Object.prbr_codpre[1]) Or dw_2.Object.prbr_codpre[1] = 0 Then
		li_cont ++
		ls_mensaje 		= ls_mensaje + "~nPredio"
		ls_colu[li_cont]	= "prbr_codpre"
	End If
	
	If IsNull(dw_2.Object.prcc_codigo[1]) Or dw_2.Object.prcc_codigo[1] = 0 Then
		li_cont ++
		ls_mensaje 		= ls_mensaje + "~nCuartel"
		ls_colu[li_cont]	= "prcc_codigo"
	End If
	
	If IsNull(dw_2.Object.ccre_feccos[1]) Then
		li_cont ++
		ls_mensaje 		= ls_mensaje + "~nFecha de Cosecha"
		ls_colu[li_cont]	= "ccre_feccos"
	End If
	
	If IsNull(dw_2.Object.ccre_fecrec[1]) Then
		li_cont ++
		ls_mensaje 		= ls_mensaje + "~nFecha Recepción"
		ls_colu[li_cont]	= "ccre_fecrec"
	End If
	
	If IsNull(dw_2.Object.ccre_reslot[1]) Then
		li_cont ++
		ls_mensaje 		= ls_mensaje + "~nResolución de Lote"
		ls_colu[li_cont]	= "ccre_reslot"
	End If

	If ii_Especie <> 23 And ii_Especie <> 41 And ii_Especie <> 81 Then
		If ii_Especie <> 82 Then
			If IsNull(dw_2.Object.ccre_hordes[1]) Then
				li_cont ++
				ls_mensaje 		= ls_mensaje + "~nHora Recepcion"
				ls_colu[li_cont]	= "ccre_hordes"
			End If
		End If
	
		If IsNull(dw_2.Object.ccre_horing[1]) Then
			li_cont ++
			ls_mensaje 		= ls_mensaje + "~nHora Inspeccion"
			ls_colu[li_cont]	= "ccre_horing"
		End If
	End If
	
	If IsNull(dw_2.Object.ccin_codigo[1]) Or dw_2.Object.ccin_codigo[1] = 0 Then
		li_cont ++
		ls_mensaje 		= ls_mensaje + "~nInspector"
		ls_colu[li_cont]	= "ccin_codigo"
	End If
	
	If IsNull(dw_2.Object.lote_codigo[1]) Or dw_2.Object.lote_codigo[1] = 0 Then
		li_cont ++
		ls_mensaje 		= ls_mensaje + "~nLote"
		ls_colu[li_cont]	= "lote_codigo"
	End If
	
	If ii_especie = 41 Then
		If IsNull(dw_2.Object.lote_totnet[1]) Or dw_2.Object.lote_totnet[1] = 0 Then
			li_cont ++
			ls_mensaje 		= ls_mensaje + "~nKg Netos"
			ls_colu[li_cont]	= "lote_totnet"
		End If
		
//		If (dw_2.Object.ccre_pocat1[1] + dw_2.Object.ccre_pocat2[1] + dw_2.Object.ccre_poplan[1]) < 100 Then
//			li_cont ++
//			ls_mensaje 		= ls_mensaje + "~nPorcentaje de Categoria no Suma el 100%"
//			ls_colu[li_cont]	= "ccre_pocat1"
//		End If
	End If
End If

If li_cont > 0 Then
		MessageBox("Error de Consistencia", "Falta el ingreso de :" + &
					ls_mensaje + ".", StopSign!, Ok!)
		dw_2.SetColumn(ls_colu[1])
		dw_2.SetFocus()
		HabilitaEncab(TRUE)
		Message.DoubleParm = -1
		Return		
Else		
	//Valida dw_3
	If  dw_3.RowCount() > 0  And ii_Especie =  41 Then
		li_existeError	=	Valida_frutosEvaluados()
		If li_existeError > 0 Then
			MessageBox("Error de Consistencia", "Falta el ingreso de algún item en Muestra 1" +  &
								"~n~nen Carpeta Evaluación de Condición.", StopSign!, Ok!)
			Tab_1.TabPage_1.dw_evaluacion.SetColumn("ccec_mues01")
			Tab_1.TabPage_1.dw_evaluacion.SetFocus()
			Tab_1.SelectTab(3)
			Bloquea_Columnas()
			Message.DoubleParm = -1
			Return
		End If
	End If
	
	If  dw_3.RowCount() > 0  And (ii_Especie =  26 Or  ii_Especie =  27 Or ii_Especie =  78 Or ii_Especie = 21 Or ii_Especie = 36) Then
		li_existeError	=	wf_Valida_EvaluacionColor()
		If li_existeError > 0 Then
			MessageBox("Error de Consistencia", "Sumatoria de Muestra  debe ser 100%" +  &
								"~n~nen Carpeta Evaluación de Color.", StopSign!, Ok!)
								
			Tab_1.TabPage_1.dw_evaluacion.SetColumn("ccec_mues01")			
			Tab_1.TabPage_1.dw_evaluacion.SetFocus()
			If ii_Especie = 21 Then
				Tab_1.SelectTab(6)
			Else
				Tab_1.SelectTab(3)
			End If
			
			Bloquea_Columnas()
			Message.DoubleParm = -1
			Return
		End If
	End If
	
	//Valida dw_4
	If not ii_Especie = 82 Then
	If dw_4.RowCount() > 0 Then
			
		For ll_fila = 1 To dw_4.RowCount()
			 If IsNull(dw_4.Object.ccic_cantid[ll_fila]) Or dw_4.Object.ccic_cantid[ll_fila] = 0 Then
				 li_suma++
			 Else
				li_suma = 0
				ll_fila = dw_4.RowCount()
			 End If
		Next
			
		If li_suma > 0 Then
			MessageBox("Error de Consistencia", "Falta el ingreso de a lo menos de una Cantidad" + &
							"~n~nen Carpeta de Inspección.", StopSign!, Ok!)
			Tab_1.TabPage_2.dw_inspeccion.SetColumn("ccic_cantid")
			Tab_1.TabPage_2.dw_inspeccion.SetFocus()
			Tab_1.SelectTab(1)
			Bloquea_Columnas()
			Message.DoubleParm = -1
			Return
		End If
	End If
		
		If IsNull(dw_4.Object.ccic_Totfru[1]) Or dw_4.Object.ccic_Totfru[1] = 0  Then
			MessageBox("Error de Consistencia", "Falta el ingreso de Total frutos Evaluados" + &
							"~n~nen Carpeta de Inspección .", StopSign!, Ok!)
			Tab_1.TabPage_2.dw_inspeccion.SetColumn("ccic_Totfru")
			Tab_1.TabPage_2.dw_inspeccion.SetFocus()
			Tab_1.SelectTab(1)
			Bloquea_Columnas()
			Message.DoubleParm = -1
			Return
		End If
		
		If dw_4.Object.Total_defecTos[1] > dw_4.Object.ccic_Totfru[1] Then
			Messagebox("Error","Total DefecTos no puede ser superior a Total frutos Evaluados.", StopSign!, Ok!)
			dw_4.SetColumn("Total_defectos")
			Tab_1.SelectTab(1)
			dw_4.SetFocus()
			Bloquea_Columnas()
			Message.DoubleParm = -1
			Return
		End If
		
	End If
	
	//Valida dw_5
	If dw_5.RowCount() > 0 Then
		li_suma = 0
		For ll_fila = 1 To dw_5.RowCount()
			 If IsNull(dw_5.Object.cidc_cantid[ll_fila]) Or dw_5.Object.cidc_cantid[ll_fila] = 0 Then
				 li_suma++
			 Else
				li_suma = 0
				ll_fila = dw_5.RowCount()
			 End If
		Next
					
		If li_suma > 0 Then
			MessageBox("Error de Consistencia", "Falta el ingreso de a lo menos " + &
			    			"~n~nuna Cantidad en Distribución de Calibres. ", StopSign!, Ok!)
			Tab_1.TabPage_3.dw_distribucion.SetColumn("cidc_cantid")
			Tab_1.TabPage_3.dw_distribucion.SetFocus()
			Tab_1.SelectTab(5)
			Bloquea_Columnas()
			Message.DoubleParm = -1
			Return
		End If	
	End If
	
/*solicitado pOr veronica costa*/

//	If dw_4.RowCount() > 0 AND dw_5.RowCount() > 0 Then
//	
//		If dw_4.Object.ccic_Totfru[1] <> dw_5.Object.Total_frutos[1] Then
//			Messagebox("Error","Total frutos Evaluados debe ser igual a Total" + &
//							"~n~nen Distrubución de Calibres.", StopSign!, Ok!)
//			dw_4.SetColumn("ccic_Totfru")
//			dw_4.SetFocus()
//			Tab_1.SelectTab(1)
//			Bloquea_Columnas()
//			Message.DoubleParm = -1
//			Return
//		End If
//	End If

End If

If ii_Especie = 27 Or ii_Especie = 26 Or ii_Especie = 81 Or ii_Especie = 36 Or ii_Especie = 10 Then
//Valida dw_6
	If dw_6.RowCount() > 0 Then
		li_suma = 0
		For ll_fila = 1 To dw_6.RowCount()
			 If IsNull(dw_6.Object.evmc_muestr[ll_fila]) Or dw_6.Object.evmc_muestr[ll_fila] = 0 Then
				 li_suma++
			 Else
				li_suma = 0
				ll_fila = dw_6.RowCount()
			 End If
		Next
					
		If li_suma > 0 Then
			If ii_Especie = 81 Then
				MessageBox("Error de Consistencia", "Falta el ingreso de a lo menos " + &
			    			"~n~nuna Cantidad en Calculo porcentaje Aceite", StopSign!, Ok!)
			Else
			MessageBox("Error de Consistencia", "Falta el ingreso de a lo menos " + &
			    			"~n~nuna Cantidad en Evaluación de Madurez", StopSign!, Ok!)
			End If
			Tab_1.TabPage_4.dw_madurez.SetColumn("evmc_muestr")
			Tab_1.TabPage_4.dw_madurez.SetFocus()
			Tab_1.SelectTab(4)
			Bloquea_Columnas()
			Message.DoubleParm = -1
			Return
		End If	
	End If

//Valida dw_7
	li_existeError	=	Valida_muestras()
	If li_existeError > 0 Then
		MessageBox("Error de Consistencia", "Falta el ingreso de algún item en Muestra 1" +  &
						"~n~nen Carpeta Inspección de Condición.", StopSign!, Ok!)
		Tab_1.TabPage_5.dw_inspecpromedio.SetColumn("iccd_mues01")
		Tab_1.TabPage_5.dw_inspecpromedio.SetFocus()
		Tab_1.SelectTab(2)
		Bloquea_Columnas()
		Message.DoubleParm = -1
		Return
	End If
End If	

If dw_7.RowCount() > 0 And (ii_Especie =  21 Or  ii_Especie =  26 Or  ii_Especie =  27 Or ii_Especie =  78 Or ii_Especie = 21 Or ii_Especie = 36) Then
	If IsNull(dw_7.Object.iccd_clasif[1]) Or dw_7.Object.iccd_clasif[1] = 0  Then
		MessageBox("Error de Consistencia", " Falta Vida PC Final~n~nen Carpeta Evaluación de Condición.", StopSign!, Ok!)
		Tab_1.TabPage_5.dw_inspecpromedio.SetColumn("iccd_clasif")
		Tab_1.TabPage_5.dw_inspecpromedio.SetFocus()
		Tab_1.SelectTab(2)
		Bloquea_Columnas()
		Message.DoubleParm = -1
		Return
	End If
End If

If dw_7.RowCount() > 0 And ii_Especie = 21 Then
	If IsNull(dw_7.Object.iccd_vpcnor[1]) Or dw_7.Object.iccd_vpcnor[1] = 0  Then
		MessageBox("Error de Consistencia", " Falta Vida PC Norma" +  &
						"~n~nen Carpeta Evaluación de Condición.", StopSign!, Ok!)
		Tab_1.TabPage_5.dw_inspecpromedio.SetColumn("iccd_vpcnor")
		Tab_1.TabPage_5.dw_inspecpromedio.SetFocus()
		Tab_1.SelectTab(2)
		Bloquea_Columnas()
		Message.DoubleParm = -1
		Return
	End If

	If IsNull(dw_7.Object.iccd_usumod[1]) Or dw_7.Object.iccd_usumod[1] = 0  Then
		If dw_7.Object.iccd_clasIf[1] <> dw_7.Object.iccd_vpcnor[1] Then
			MessageBox("Error de Consistencia", " Falta Quien modfica" +  &
							"~n~nen Carpeta P.Interno.", StopSign!, Ok!)
			Tab_1.TabPage_5.dw_inspecpromedio.SetColumn("iccd_usumod")
			Tab_1.TabPage_5.dw_inspecpromedio.SetFocus()
			Tab_1.SelectTab(2)
			Bloquea_Columnas()
			Message.DoubleParm = -1
			Return
		End If
	End If
End If	

If ii_Especie = 27 Or ii_Especie = 26 Or ii_Especie = 78 Or ii_Especie = 41 Or ii_Especie = 21 Or ii_Especie = 81 Or ii_Especie = 36  Or ii_Especie = 10 Then
	If IsNull(dw_2.Object.ccre_reslot[1]) Then
		li_resultado = MessageBox("Atención",'Desea Aprobar la Resolucion del Lote',Exclamation!, YesNo! , 2)
		
		If li_resultado = 1 Then
			dw_2.Object.ccre_reslot[1] = 1
		Else
			dw_2.Object.ccre_reslot[1] = 0
		End If
	End If
	
	If ii_Especie <> 41 And ii_Especie <> 21 And ii_Especie <> 81 And ii_Especie <> 10 Then 	
		If dw_4.Object.ccic_Totfru[1] <> dw_3.Object.frutos[1] Then
			Messagebox("Error","Falta Cuadrar Total de frutos(Defectos) con N° de frutos(Evaluación de Color).", StopSign!, Ok!)
			Message.DoubleParm = -1
			Return
		End If	
	End If	
End If	

//Valida Granadas

If dw_9.RowCount() > 0 AND ii_Especie = 82 Then
	Tab_1.TabPage_7.dw_catguarda.AcceptText()
	li_suma = 0
	For ll_fila = 1 To dw_9.RowCount()
		If Not IsNull(dw_9.Object.cccg_cantid[ll_Fila]) Or dw_9.Object.cccg_cantid[ll_Fila] > 0  Then	li_Suma	=	li_Suma + dw_9.Object.cccg_cantid[ll_Fila]
	Next		
		
	If IsNull(li_suma) Or li_suma <> 100  Then
		MessageBox("Error de Consistencia", " Suma Total debe ser 100%." +  &
						"~n~nen Carpeta  ClasIficación Categorías.", StopSign!, Ok!)
		Tab_1.TabPage_7.dw_catguarda.SetColumn("cccg_cantid")
		Tab_1.TabPage_7.dw_catguarda.SetFocus()
		Tab_1.SelectTab(7)
		Bloquea_Columnas()
		Message.DoubleParm = -1
		Return
	End If
End If	

If dw_2.GetItemStatus(1,0,Primary!) = New! Or &
	dw_2.GetItemStatus(1,0,Primary!) = NewModIfied! Then

	UPDATE 	dbo.ctlcalrecepcionfrutasenca
		SET 	ccre_numero = 0
		WHERE	1 = 2 ;
End If

For ll_Fil1 = 1 To dw_3.RowCount()	
	If dw_3.GetItemStatus(ll_Fil1, 0, Primary!) = New! Or &
		dw_3.GetItemStatus(ll_Fil1, 0, Primary!) = NewModIfied! Then
		dw_3.SetItem(ll_Fil1, "ccre_numero", dw_2.Object.ccre_numero[1])
		dw_3.SetItem(ll_Fil1, "clie_codigo", dw_2.Object.clie_codigo[1])
		dw_3.SetItem(ll_Fil1, "espe_codigo", ii_Especie)
		dw_3.SetItem(ll_Fil1, "plde_codigo", dw_2.Object.plde_codigo[1])
		//dw_3.Object.ccec_media[ll_Fil1] = dw_3.Object.media[ll_Fil1] 
	End If	
	//PORCENTAJE COLOR PARA GRANADAS Y CLEMENTINAS-NARANJAS
	If ii_Especie = 82 or ii_Especie = 26 or ii_Especie = 27 or ii_Especie = 36  Then
		If IsNull(dw_3.Object.ccec_mues01[ll_Fil1]) Then
			dw_3.Object.ccec_porcen[ll_Fil1] = dec(ls_Null)
		Else   
			dw_3.Object.ccec_porcen[ll_Fil1] = dw_3.Object.calc_porce[ll_Fil1] 
		End If	
	End If
Next

For ll_Fil2 = 1 To dw_4.RowCount()	
	 If dw_4.GetItemStatus(ll_Fil2, 0, Primary!) = New! Or &
		dw_4.GetItemStatus(ll_Fil2, 0, Primary!) = NewModIfied! Then
		dw_4.SetItem(ll_Fil2, "ccre_numero", dw_2.Object.ccre_numero[1])
		dw_4.SetItem(ll_Fil2, "clie_codigo", dw_2.Object.clie_codigo[1])
		dw_4.SetItem(ll_Fil2, "espe_codigo", ii_Especie)
		dw_4.SetItem(ll_Fil2, "plde_codigo", dw_2.Object.plde_codigo[1])		
	End If	
	
	If IsNull(dw_4.Object.ccic_cantid[ll_Fil2]) Then
		dw_4.Object.ccic_porcen[ll_Fil2] = dec(ls_Null)
	Else
		dw_4.Object.ccic_porcen[ll_Fil2] = dw_4.Object.calc_porce[ll_Fil2]	
	End If
	
	If ii_Especie = 82 Or ii_Especie = 27 Or ii_Especie = 78 Then //Or ii_Especie = 23 Then 
		If IsNull(dw_4.Object.ccic_cancom[ll_Fil2]) Then
			dw_4.Object.ccic_porcom[ll_Fil2] = dec(ls_Null)
		Else
			dw_4.Object.ccic_porcom[ll_Fil2] = dw_4.Object.calc_porcom[ll_Fil2]	
		End If
	
		If IsNull(dw_4.Object.ccic_canti2[ll_Fil2]) Then
			dw_4.Object.ccic_porce2[ll_Fil2] = dec(ls_Null)
		Else
			dw_4.Object.ccic_porce2[ll_Fil2] = dw_4.Object.calc_porce2[ll_Fil2]	
		End If
	End If
Next

For ll_Fil3 = 1 To dw_5.RowCount()	
	 If dw_5.GetItemStatus(ll_Fil3, 0, Primary!) = New! Or &
		dw_5.GetItemStatus(ll_Fil3, 0, Primary!) = NewModIfied! Then
		dw_5.SetItem(ll_Fil3, "ccre_numero", dw_2.Object.ccre_numero[1])
		dw_5.SetItem(ll_Fil3, "clie_codigo", dw_2.Object.clie_codigo[1])
		dw_5.SetItem(ll_Fil3, "espe_codigo", ii_Especie)
		dw_5.SetItem(ll_Fil3, "plde_codigo", dw_2.Object.plde_codigo[1])
		dw_5.SetItem(ll_Fil3, "vari_codigo", dw_2.Object.vari_codigo[1])
	End If	
	If IsNull(dw_5.Object.cidc_cantid[ll_Fil3]) Then
		dw_5.Object.cidc_porcen[ll_Fil3] = dec(ls_Null)
	Else   
		dw_5.Object.cidc_porcen[ll_Fil3] = dw_5.Object.calc_porce[ll_Fil3] 
	End If	
Next	

If ii_Especie = 27 Or ii_Especie = 26 Or ii_Especie = 81 Or ii_Especie = 78 Or ii_Especie = 23 &
	Or ii_Especie = 82 Or ii_Especie = 21 Or ii_Especie = 36  Or ii_Especie = 10 Then
	If ii_Especie <> 23 Then
		For ll_Fil4 = 1 To dw_6.RowCount()	
			If dw_6.GetItemStatus(ll_Fil4, 0, Primary!) = New! Or &
				dw_6.GetItemStatus(ll_Fil4, 0, Primary!) = NewModIfied! Then
				dw_6.SetItem(ll_Fil4, "ccre_numero", dw_2.Object.ccre_numero[1])
				dw_6.SetItem(ll_Fil4, "clie_codigo", dw_2.Object.clie_codigo[1])
				dw_6.SetItem(ll_Fil4, "espe_codigo", ii_Especie)
				dw_6.SetItem(ll_Fil4, "plde_codigo", dw_2.Object.plde_codigo[1])
			End If	
		Next
	End If
	
	For ll_Fil5 = 1 To dw_7.RowCount()	
		If dw_7.GetItemStatus(ll_Fil5, 0, Primary!) = New! Or &
			dw_7.GetItemStatus(ll_Fil5, 0, Primary!) = NewModIfied! Then
			dw_7.SetItem(ll_Fil5, "ccre_numero", dw_2.Object.ccre_numero[1])
			dw_7.SetItem(ll_Fil5, "clie_codigo", dw_2.Object.clie_codigo[1])
			dw_7.SetItem(ll_Fil5, "espe_codigo", ii_Especie)
			dw_7.SetItem(ll_Fil5, "plde_codigo", dw_2.Object.plde_codigo[1])
		End If	
	Next
	
	If ii_Especie = 26 Or  ii_Especie = 27 Or ii_Especie = 78 Or ii_Especie = 36  Or ii_Especie = 10 Then
		For ll_fil13 = 1 To dw_13.RowCount()
			If dw_13.GetItemStatus(ll_fil13, 0, Primary!) = New! Or &
				dw_13.GetItemStatus(ll_fil13, 0, Primary!) = NewModIfied! Then
				dw_13.Object.clie_codigo[ll_fil13] = dw_2.Object.clie_codigo[1]
				dw_13.Object.plde_codigo[ll_fil13] = dw_2.Object.plde_codigo[1]
				dw_13.Object.ccre_numero[ll_fil13] = dw_2.Object.ccre_numero[1]
				dw_13.Object.espe_codigo[ll_fil13] = ii_Especie
			End If	
		Next
	End If
End If

If ii_Especie = 41 Or ii_Especie = 82 Then
	For ll_Fil6 = 1 To dw_8.RowCount()	
		If dw_8.GetItemStatus(ll_Fil6, 0, Primary!) = New! Or &
			dw_8.GetItemStatus(ll_Fil6, 0, Primary!) = NewModIfied! Then
			dw_8.SetItem(ll_Fil6, "ccre_numero", dw_2.Object.ccre_numero[1])
			dw_8.SetItem(ll_Fil6, "clie_codigo", dw_2.Object.clie_codigo[1])
			dw_8.SetItem(ll_Fil6, "espe_codigo", ii_Especie)
			dw_8.SetItem(ll_Fil6, "plde_codigo", dw_2.Object.plde_codigo[1])
		End If	
	Next
	
	For ll_Fil7 = 1 To dw_9.RowCount()	
		If dw_9.GetItemStatus(ll_Fil7, 0, Primary!) = New! Or &
			dw_9.GetItemStatus(ll_Fil7, 0, Primary!) = NewModIfied! Then
			dw_9.SetItem(ll_Fil7, "ccre_numero", dw_2.Object.ccre_numero[1])
			dw_9.SetItem(ll_Fil7, "clie_codigo", dw_2.Object.clie_codigo[1])
			dw_9.SetItem(ll_Fil7, "espe_codigo", ii_Especie)
			dw_9.SetItem(ll_Fil7, "plde_codigo", dw_2.Object.plde_codigo[1])
		End If	
	Next
	
	For ll_fil10 = 1 To dw_10.RowCount()
		If dw_10.GetItemStatus(ll_fil10, 0, Primary!) = New! Or &
			dw_10.GetItemStatus(ll_fil10, 0, Primary!) = NewModIfied! Then
			dw_10.Object.clie_codigo[ll_fil10] = dw_2.Object.clie_codigo[1]
			dw_10.Object.plde_codigo[ll_fil10] = dw_2.Object.plde_codigo[1]
			dw_10.Object.ccre_numero[ll_fil10] = dw_2.Object.ccre_numero[1]
			dw_10.Object.espe_codigo[ll_fil10] = ii_Especie
			dw_10.Object.ctbo_secuen[ll_fil10] = ll_fil10
		End If	
	Next
	
	If ii_Especie = 41 Then
		For ll_fil11 = 1 To dw_11.RowCount()
			If dw_11.GetItemStatus(ll_fil11, 0, Primary!) = New! Or &
				dw_11.GetItemStatus(ll_fil11, 0, Primary!) = NewModIfied! Then
				dw_11.Object.clie_codigo[ll_fil11] = dw_2.Object.clie_codigo[1]
				dw_11.Object.plde_codigo[ll_fil11] = dw_2.Object.plde_codigo[1]
				dw_11.Object.ccre_numero[ll_fil11] = dw_2.Object.ccre_numero[1]
				dw_11.Object.espe_codigo[ll_fil11] = ii_Especie
			End If	
		Next
	End If
	
	If ii_Especie = 82 Then
		For ll_fil12 = 1 To dw_11.RowCount()
			If dw_12.GetItemStatus(ll_fil12, 0, Primary!) = New! Or &
				dw_12.GetItemStatus(ll_fil12, 0, Primary!) = NewModIfied! Then
				dw_12.Object.clie_codigo[ll_fil12] = dw_2.Object.clie_codigo[1]
				dw_12.Object.plde_codigo[ll_fil12] = dw_2.Object.plde_codigo[1]
				dw_12.Object.ccre_numero[ll_fil12] = dw_2.Object.ccre_numero[1]
				dw_12.Object.espe_codigo[ll_fil12] = ii_Especie
			End If	
		Next
	End If
End If
/*Se elimina columna calibre minimo. Solicitado por veronica costa 24/04/2007*/
		
////ll_valexpOr =  100 - (dw_4.Object.Tot_porce[1]) /*frutos EVALUADOS - frutos DEFECTUOSOS*/
///*Se suman los porcentajes de distribución de
//  calibres excluyEndo las dos últimas filas*/
//  
If ii_Especie = 21 Then  
	ls_Calibre	=	'22'
//ElseIf ii_Especie = 41 Then 
//	ls_Calibre	=	'42'
//ElseIf ii_Especie = 27 Then
//	ls_Calibre  =  '88'
//ElseIf ii_Especie = 26 Then
//	ls_calibre = '4'
//ElseIf ii_Especie = 81 Then
//	ls_calibre = '70'
End If
//  
// If (ii_Especie <> 27 AND ii_Especie <> 26 AND ii_Especie <> 81) Or &
// (IsNull(dw_2.Object.ccre_porexp[1]) AND ii_Especie <> 27) Or &
// (IsNull(dw_2.Object.ccre_porexp[1]) AND ii_Especie <> 26) Or &
// (IsNull(dw_2.Object.ccre_porexp[1]) AND ii_Especie <> 81) Then
If ii_Especie = 21 Then
	If MessageBox("Calcula Porcentaje","Calibre mínimo embalado será " + ls_Calibre + '?.', Question!, YesNo!)	= 1 Then
		 Message.DoubleParm = 1 
		 //calcula_porexport()
		 dw_2.object.ccre_tipcal[1] = 2	 
	Else
//		/*Solo excluye la última fila	*/
		If ii_Especie = 21 Then  
			ls_Calibre	=	'20'
//		ElseIf ii_Especie = 41 Then 
//			ls_Calibre	=	'45'
//		ElseIf ii_Especie = 27 Then
//			ls_Calibre  =  '113' 
//		ElseIf ii_Especie = 26 Then
//			ls_calibre = '5'
//		ElseIf ii_Especie = 81 Then
//			ls_calibre = '80'
		End If
//	 porexpor_final()
	 dw_2.object.ccre_tipcal[1] = 1
	 MessageBox("Calcula Porcentaje","Porcentaje Calculado fué para Calibre"  + &
	 				"~r~rmínimo embalado de "+ ls_Calibre + '.', StopSign!, Ok!)
	End If
End If

dw_2.Object.ccre_observ[1] =	Tab_1.TabPage_9.mle_observacion.Text

//CAMARAS FRIO
If ii_especie = 41 Then
//	If Tab_1.TabPage_11.rb_fc1.Checked Then
//		dw_2.Object.ccre_tipoat[1] =	'FC 1'
//	ElseIf Tab_1.TabPage_11.rb_ac1.Checked Then
//		dw_2.Object.ccre_tipoat[1] =	'AC 1'
//	ElseIf Tab_1.TabPage_11.rb_fc2.Checked Then
//		dw_2.Object.ccre_tipoat[1] =	'FC 2'
//	ElseIf Tab_1.TabPage_11.rb_ac2.Checked Then
//		dw_2.Object.ccre_tipoat[1] =	'AC 2'
//	Else
//		MessageBox("Error de Consistencia", "Debe Seleccionar un Tipo de Frío " , StopSign!, Ok!)
//		Tab_1.SelectTab(8)
//		Message.DoubleParm = -1
//		Return
//	End If
ElseIf ii_especie = 27 Or ii_especie = 36 Then
	If Tab_1.TabPage_11.rb_fc1.Checked Then
		dw_2.Object.ccre_tipoat[1] =	'FC'
	ElseIf Tab_1.TabPage_11.rb_ac1.Checked Then
		dw_2.Object.ccre_tipoat[1] =	'AC'
	Else
		MessageBox("Error de Consistencia", "Debe Seleccionar un Tipo de Frío " , StopSign!, Ok!)
		Tab_1.SelectTab(8)
		Message.DoubleParm = -1
		Return
	End If
End If

HabilitaEncab(False)	
dw_2.Object.buscalote.visible = False
end event

event ue_borrar;IF dw_2.RowCount() < 1 THEN RETURN

SetPointer(HourGlass!)

ib_borrar = True
w_main.SetMicroHelp("Validando la eliminación...")

Message.DoubleParm = 0

This.TriggerEvent ("ue_validaborrar")

IF Message.DoubleParm = -1 THEN RETURN

IF dw_3.RowCount() > 0 THEN dw_3.RowsMove(1,dw_3.RowCount(),Primary!,dw_3,1,Delete!)
IF dw_4.RowCount() > 0 THEN dw_4.RowsMove(1,dw_4.RowCount(),Primary!,dw_4,1,Delete!)
IF dw_5.RowCount() > 0 THEN dw_5.RowsMove(1,dw_5.RowCount(),Primary!,dw_5,1,Delete!)
IF dw_6.RowCount() > 0 THEN dw_6.RowsMove(1,dw_6.RowCount(),Primary!,dw_6,1,Delete!)
IF dw_7.RowCount() > 0 THEN dw_7.RowsMove(1,dw_7.RowCount(),Primary!,dw_7,1,Delete!)
IF dw_8.RowCount() > 0 THEN dw_8.RowsMove(1,dw_8.RowCount(),Primary!,dw_8,1,Delete!)
IF dw_9.RowCount() > 0 THEN dw_9.RowsMove(1,dw_9.RowCount(),Primary!,dw_9,1,Delete!)
IF dw_10.RowCount() > 0 THEN dw_10.RowsMove(1,dw_10.RowCount(),Primary!,dw_10,1,Delete!)
IF dw_11.RowCount() > 0 THEN dw_11.RowsMove(1,dw_11.RowCount(),Primary!,dw_11,1,Delete!)
IF dw_12.RowCount() > 0 THEN dw_12.RowsMove(1,dw_12.RowCount(),Primary!,dw_12,1,Delete!)
IF dw_13.RowCount() > 0 THEN dw_13.RowsMove(1,dw_13.RowCount(),Primary!,dw_13,1,Delete!)

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

event ue_controlborrar;call super::ue_controlborrar;//Long		ll_fil1, ll_fil2, ll_fil3 , ll_fila, ll_valexpor
//Integer  li_cont, li_existeerror, li_suma
//String	ls_Mensaje, ls_colu[]
//
//IF dw_2.RowCount() > 0 THEN	
//	IF Isnull(dw_2.Object.clie_codigo[1]) OR dw_2.Object.clie_codigo[1] = 0 THEN
//		li_cont ++
//		ls_mensaje 			= ls_mensaje + "~nCliente"
//		ls_colu[li_cont]	= "clie_codigo"
//	END IF	
//	
//	IF Isnull(dw_2.Object.ccre_numero[1]) OR dw_2.Object.ccre_numero[1] = 0 THEN
//		li_cont ++
//		ls_mensaje 			= ls_mensaje + "~nNº Planilla"
//		ls_colu[li_cont]	= "ccre_numero"
//	END IF
//	
//	IF Isnull(dw_2.Object.zona_codigo[1]) OR dw_2.Object.zona_codigo[1] = 0 THEN
//		li_cont ++
//		ls_mensaje 			= ls_mensaje + "~nZona"
//		ls_colu[li_cont]	= "zona_codigo"
//	END IF	
//	
//	IF Isnull(dw_2.Object.plde_codigo[1]) OR dw_2.Object.plde_codigo[1] = 0 THEN
//		li_cont ++
//		ls_mensaje 			= ls_mensaje + "~nPlanta"
//		ls_colu[li_cont]	= "plde_codigo"
//	END IF	
//	
//	IF Isnull(dw_2.Object.espe_codigo[1]) OR dw_2.Object.espe_codigo[1] = 0 THEN
//		li_cont ++
//		ls_mensaje 			= ls_mensaje + "~nEspecie"
//		ls_colu[li_cont]	= "espe_codigo"
//	END IF
//	
//	IF Isnull(dw_2.Object.vari_codigo[1]) OR dw_2.Object.vari_codigo[1] = 0 THEN
//		li_cont ++
//		ls_mensaje 			= ls_mensaje + "~nVariedad"
//		ls_colu[li_cont]	= "vari_codigo"
//	END IF
//	
//	IF Isnull(dw_2.Object.prod_codigo[1]) OR dw_2.Object.prod_codigo[1] = 0 THEN
//		li_cont ++
//		ls_mensaje 			= ls_mensaje + "~nProductor"
//		ls_colu[li_cont]	= "prod_codigo"
//	END IF
//	
//	IF Isnull(dw_2.Object.ccre_fecrec[1]) THEN
//		li_cont ++
//		ls_mensaje 			= ls_mensaje + "~nFecha Recepción"
//		ls_colu[li_cont]	= "ccre_fecrec"
//	END IF
//	
//	IF Isnull(dw_2.Object.ccre_noguia[1]) OR dw_2.Object.ccre_noguia[1] = 0 THEN
//		li_cont ++
//		ls_mensaje 			= ls_mensaje + "~nNº Guía"
//		ls_colu[li_cont]	= "ccre_noguia"
//	END IF
//	
//	IF Isnull(dw_2.Object.ccag_codigo[1]) OR dw_2.Object.ccag_codigo[1] = 0 THEN
//		li_cont ++
//		ls_mensaje 			= ls_mensaje + "~nAgrónomo"
//		ls_colu[li_cont]	= "ccag_codigo"
//	END IF
//	
//	IF Isnull(dw_2.Object.lote_codigo[1]) OR dw_2.Object.lote_codigo[1] = 0 THEN
//		li_cont ++
//		ls_mensaje 			= ls_mensaje + "~nLote"
//		ls_colu[li_cont]	= "lote_codigo"
//	END IF
//	
//	
//
//END IF
//
////Valida dw_3
//li_existeerror	=	Valida_FrutosEvaluados()
//IF li_existeerror > 0 THEN
//	li_cont++
//	ls_Mensaje = ls_mensaje + "~na lo menos una Muestra en Carpeta Evaluación de Condición"
//	ls_colu[li_cont] = "ccre_numero"
//END IF
//
//IF dw_3.RowCount() > 0 THEN
//	IF IsNull(dw_3.Object.ccec_clasif[1]) OR dw_3.Object.ccec_clasif[1] = 0  THEN
//		li_cont++
//		ls_mensaje = ls_mensaje + "~nClasificación de Condición"
//		ls_colu[li_cont] = "ccre_numero"
//	END IF
//END IF
//
////Valida dw_4
//IF dw_4.RowCount() > 0 THEN
//		
//	FOR ll_fila = 1 to dw_4.RowCount()
//		 IF IsNull(dw_4.Object.ccic_cantid[ll_fila]) OR dw_4.Object.ccic_cantid[ll_fila] = 0 THEN
//			 li_suma++
//		 ELSE
//			li_suma = 0
//			ll_fila = dw_4.RowCount()
//		 END IF
//	NEXT
//		
//	IF li_suma > 0 THEN
//		li_cont++
//		ls_mensaje = ls_mensaje + "~n a lo menos de una Cantidad en Carpeta de Inspección"
//		ls_colu[li_cont] = "ccre_numero"
//	END IF
//	
//	IF IsNull(dw_4.Object.ccic_totfru[1]) OR dw_4.Object.ccic_totfru[1] = 0  THEN
//		li_cont++
//		ls_mensaje = ls_mensaje + "~nTotal Frutos Evaluados en Carpeta de Inspección"
//		ls_colu[li_cont] = "ccre_numero"
//	END IF
//END IF
//
////Valida dw_5
//IF dw_5.RowCount() > 0 THEN
//	li_suma = 0
//	FOR ll_fila = 1 to dw_5.RowCount()
//	    IF IsNull(dw_5.Object.cidc_cantid[ll_fila]) OR dw_5.Object.cidc_cantid[ll_fila] = 0 THEN
//			 li_suma++
//       ELSE
//			li_suma = 0
//			ll_fila = dw_5.RowCount()
//		 END IF
//	NEXT
//	
//	IF li_suma > 0 THEN
//		li_cont++
//		ls_mensaje = ls_mensaje + "~na lo menos una Cantidad en Distribución de Calibres"
//		ls_colu[li_cont] = "ccre_numero"
//	END IF	
//END IF
//
//IF li_cont > 0 THEN
//		MessageBox("Error de Consistencia", "Falta el ingreso de :" + &
//					ls_mensaje + ".", StopSign!, Ok!)
//		dw_2.SetColumn(ls_colu[1])
//		dw_2.SetFocus()
//		HabilitaEncab(TRUE)
//		Message.DoubleParm = -1
//	ELSEIF dw_4.Object.total_defectos[1] > dw_4.Object.ccic_totfru[1] THEN
//		Messagebox("Error","Total Defectos no puede ser superior a Total frutos Evaluados", StopSign!, Ok!)
//		dw_4.SetColumn("total_defectos")
//		dw_4.SetFocus()
//		HabilitaEncab(TRUE)
//		Message.DoubleParm = -1
//	ELSEIF dw_4.Object.ccic_totfru[1] <> dw_5.Object.total_frutos[1] THEN
//		Messagebox("Error","Total Frutos Evaluados debe ser igual a Total en Distrubución de Calibres", StopSign!, Ok!)
//		dw_4.SetColumn("ccic_totfru")
//		dw_4.SetFocus()
//		HabilitaEncab(TRUE)
//		Message.DoubleParm = -1
//	ELSE
//		ll_valexpor =  100 - (dw_4.Object.tot_porce[1])
//		dw_2.Object.ccre_porexp[1] = ll_valexpor		
//		
//		IF dw_2.GetItemStatus(1,0,Primary!) = New! OR &
//			dw_2.GetItemStatus(1,0,Primary!) = NewModified! THEN
//		
//			UPDATE 	dba.ctlcalrecepcionfrutasenca
//				SET 	ccre_numero = 0
//				WHERE	1 = 2 ;
//		END IF
//		
//	   FOR ll_Fil1 = 1 TO dw_3.RowCount()	
//			IF dw_3.GetItemStatus(ll_Fil1, 0, Primary!) = New! OR &
//				dw_3.GetItemStatus(ll_Fil1, 0, Primary!) = NewModified! THEN
//				dw_3.SetItem(ll_Fil1, "ccre_numero", dw_2.Object.ccre_numero[1])
//				dw_3.SetItem(ll_Fil1, "clie_codigo", gi_codexport)
//				dw_3.SetItem(ll_Fil1, "espe_codigo", ii_especie)
//				dw_3.SetItem(ll_Fil1, "plde_codigo", dw_2.Object.plde_codigo[1])
//				dw_3.Object.ccec_media[ll_Fil1] = dw_3.Object.media[ll_Fil1] 
//			END IF	
//		NEXT
//		
//		FOR ll_Fil2 = 1 TO dw_4.RowCount()	
//			 IF dw_4.GetItemStatus(ll_Fil2, 0, Primary!) = New! OR &
//				dw_4.GetItemStatus(ll_Fil2, 0, Primary!) = NewModified! THEN
//				dw_4.SetItem(ll_Fil2, "ccre_numero", dw_2.Object.ccre_numero[1])
//				dw_4.SetItem(ll_Fil2, "clie_codigo", gi_codexport)
//				dw_4.SetItem(ll_Fil2, "espe_codigo", ii_especie)
//				dw_4.SetItem(ll_Fil2, "plde_codigo", dw_2.Object.plde_codigo[1])
//				dw_4.Object.ccic_porcen[ll_Fil2] = dw_4.Object.calc_porce[ll_Fil2]
//			END IF	
//		NEXT
//		
//		FOR ll_Fil3 = 1 TO dw_5.RowCount()	
//			 IF dw_5.GetItemStatus(ll_Fil3, 0, Primary!) = New! OR &
//				dw_5.GetItemStatus(ll_Fil3, 0, Primary!) = NewModified! THEN
//				dw_5.SetItem(ll_Fil3, "ccre_numero", dw_2.Object.ccre_numero[1])
//				dw_5.SetItem(ll_Fil3, "clie_codigo", gi_codexport)
//				dw_5.SetItem(ll_Fil3, "espe_codigo", ii_especie)
//				dw_5.SetItem(ll_Fil3, "plde_codigo", dw_2.Object.plde_codigo[1])
//				dw_5.SetItem(ll_Fil3, "vari_codigo", dw_2.Object.vari_codigo[1])
//				dw_5.Object.cidc_porcen[ll_Fil3] = dw_5.Object.calc_porce[ll_Fil3] 
//			END IF	
//		NEXT
//HabilitaEncab(FALSE)	
//END IF
//
end event

type dw_1 from w_mant_encab_deta_csd`dw_1 within w_maed_ctlcalplanillaespecies
boolean visible = false
integer x = 73
integer y = 884
integer width = 914
integer height = 632
boolean titlebar = false
string title = ""
boolean hscrollbar = false
boolean hsplitscroll = true
end type

type dw_2 from w_mant_encab_deta_csd`dw_2 within w_maed_ctlcalplanillaespecies
integer x = 750
integer y = 4
integer width = 3095
integer height = 824
integer taborder = 10
string dataobject = "dw_maed_ctlcalplanillaespecies"
boolean controlmenu = true
end type

event dw_2::itemchanged;String		ls_Columna, ls_Nula, ls_NroGuia, ls_Usuario
Integer	li_grupo, li_zona


ls_Usuario	=	Upper(Gstr_Us.Nombre)

SetNull(ls_Nula)
ls_Columna	=	dwo.Name

Choose Case ls_Columna
	Case "ccre_tipore"
		If Data = '3' Then  
			If ii_Especie = 78 Then
				dw_2.Object.t_13.Text				=	'% Fancy'
				dw_2.Object.t_19.Text				=	'% Choice / Stndr'
				dw_2.object.ccre_porxfa.Protect	= 0
				dw_2.object.ccre_porxfa.BackGround.Color	= RGB(255,255,225)
			End If
		Else
			If ii_Especie = 78 Then
				dw_2.object.ccre_porxfa.Protect	= 1
				dw_2.object.ccre_porxfa.BackGround.Color	=	553648127
				dw_2.Object.t_13.Text				=	'% Cat 1'
				dw_2.Object.t_19.Text				=	'% Cat 2'
			End If
		End If
		
	Case "zona_codigo"
		If iuo_zonas.existe(Integer(data),TRUE, sqlca) Then
			dw_2.GetChild("plde_codigo", idwc_plantas)
			idwc_plantas.SetTransObject(sqlca)
			idwc_plantas.Retrieve(1,-1)
			dw_2.SetItem(1, "plde_codigo",gi_codPlanta)
			
			dw_2.GetChild("prod_codigo", idwc_productores)
			idwc_productores.SetTransObject(sqlca)
			idwc_productores.Retrieve(Integer(Data))
			dw_2.SetItem(1, "prod_codigo", Integer(ls_Nula))
		Else
			This.SetItem(1, ls_Columna, Integer(ls_Nula))
			Return 1
		End If
		
	Case "plde_codigo"
		If iuo_plantas.existefrigo(Integer(data),TRUE,sqlca) Then
			If Trae_Planilla(dw_2.Object.ccre_numero[1],Integer(data),ii_especie) Then
				If ii_sw = 0 Then
					Parent.TriggerEvent("ue_recuperadatos")
				Else
					Messagebox("Atención","Planilla Digitada existe para otra Especie, no puede ingresar",exclamation!)
					dw_2.Object.ccre_numero.Protect				=	0
					dw_2.SetItem(1,"ccre_numero",Integer(ls_Nula))
				End If
				dw_2.SetColumn("ccre_numero")
				dw_2.SetFocus()
			ElseIf IsNull(dw_2.Object.lote_codigo[1]) OR dw_2.Object.lote_codigo[1] = 0 Then
				HabilitaEncab(TRUE)
			Else
				Bloquea_Columnas()
			End If		
		Else
			This.SetItem(1, ls_Columna, Integer(ls_Nula))
			Return 1			
		End If	
			
	Case "ccre_numero"
		If Trae_Planilla(Long(data),dw_2.Object.plde_codigo[1],ii_especie) Then
			If ii_sw = 0 Then
				Parent.TriggerEvent("ue_recuperadatos")
			Else
				Messagebox("Atención","Planilla Digitada existe para otra Especie, no puede ingresar",exclamation!)
				dw_2.Object.ccre_numero.Protect				=	0
				dw_2.SetItem(1,"ccre_numero",Integer(ls_Nula))
				Return 1
			End If
			dw_2.SetColumn("ccre_numero")
			dw_2.SetFocus()
	   ElseIf IsNull(dw_2.Object.lote_codigo[1]) OR dw_2.Object.lote_codigo[1] = 0 Then
			HabilitaEncab(TRUE)
		Else
			Bloquea_Columnas()
		End If
		
	Case "espe_codigo"
		  If iuo_especies.existe(Integer(data),TRUE, sqlca) Then
			  ii_especie = Integer(data)
			  Recupera_Evaluacion(This.Object.clie_codigo[Row],0,0,Integer(data))
			  Recupera_inspeccion(This.Object.clie_codigo[Row],0,0,Integer(data))
			  Recupera_madurez(This.Object.clie_codigo[Row],0,0,Integer(data))
			  Recupera_inspromedio(This.Object.clie_codigo[Row],0,0,Integer(data))
			  Recupera_firmeza(This.Object.clie_codigo[Row],0,0,Integer(data))
			  
			  dw_2.GetChild("vari_codigo", idwc_variedades)
           	  idwc_variedades.SetTransObject(sqlca)
	           idwc_variedades.Retrieve(Integer(data))
			  dw_2.SetItem(1, "vari_codigo", Integer(ls_Nula))			  
			  dw_3.Reset()
			  dw_5.reset()	
		  Else
			  This.SetItem(1, ls_Columna, Integer(ls_Nula))
			  Return 1
		  End If		
				
	Case "vari_codigo"
		  If iuo_variedades.existe(ii_especie,Integer(data),TRUE, sqlca) Then
			  If Recupera_Distribucion(This.Object.clie_codigo[Row],0,0,ii_especie,Integer(data), 0) = 0 Then
				  Messagebox("Atención","No Existe Distribución de Calibres para variedad seleccionada.", StopSign!, Ok!)
			  End If	 
	     Else
			  This.SetItem(1, ls_Columna, Integer(ls_Nula))
			  Return 1
		  End If	
		  
	Case "prod_codigo"
		  If NOT iuo_productor.existe(Long(data),dw_2.Object.zona_codigo[1],TRUE, sqlca) Then
		     This.SetItem(1, ls_Columna, Long(ls_Nula))
			  Return 1
		  Else
			   dw_2.GetChild("prbr_codpre", idwc_predio)
				idwc_predio.Retrieve(Long(data)) 
				dw_2.Object.prbr_codpre[1] = Integer(ls_Nula)
				
				dw_2.GetChild("prcc_codigo", idwc_cuartel)
			   idwc_cuartel.Retrieve(Long(data),0)
			   dw_2.Object.prcc_codigo[1] = Integer(ls_Nula)					
		  End If	
		  
	Case "prbr_codpre"
		  If Not iuo_prodpredio.existepredioprod(dw_2.Object.prod_codigo[1],Integer(data),TRUE, sqlca) Then
		     This.SetItem(1, ls_Columna, Long(ls_Nula))
			  Return 1
		  Else
			  dw_2.GetChild("prcc_codigo", idwc_cuartel)
			  idwc_cuartel.Retrieve(dw_2.Object.prod_codigo[1],Integer(data))
			  dw_2.Object.prcc_codigo[1] = Integer(ls_Nula)				
		  End If		  
		  
	Case "prcc_codigo"
		  If Not iuo_prodcuarteles.existe(dw_2.Object.prod_codigo[1],dw_2.Object.prbr_codpre[1],Integer(data),TRUE, sqlca) Then
		     This.SetItem(1, ls_Columna, Long(ls_Nula))
			  Return 1
		  End If		  
			  
	Case "ccre_temper"
		  If Integer(data) > 99.99 Then
		     This.SetItem(1, ls_Columna, Integer(ls_Nula))
			  Return 1
		  End If		  
	
	Case "ccin_codigo"
		  If NOT iuo_inspectores.existe(sqlca,Integer(data),TRUE) Then
			  This.SetItem(1, ls_Columna, Integer(ls_Nula))
			  Return 1			
	     End If	 
		  
	Case "lote_codigo"
		If IsNull(istr_mant.argumento[5]) Then istr_mant.argumento[5] = '0'
		If Existe_lote(ls_columna,Integer(data)) Then
			This.SetItem(1,ls_Columna,Integer(ls_Nula))
			Return 1
		End If
		
		If gstr_parlote.codgen = 0 AND istr_mant.argumento[5] = '0' Then
			If Existelote(dw_2.Object.plde_codigo[1],dw_2.Object.espe_codigo[1],Integer(data)) Then
				 Bloquea_Columnas()
			Else
				If ii_especie = 78 Then
				 	Messagebox("Atención","Número Lote no Existe, o no corresponde a Planta,Especie o Tipo Recepción Ingresada, Verifique.", StopSign!, Ok!)
				 	This.SetItem(1, ls_Columna, Integer(ls_Nula))
				 	Return 1			
				End If	 
			End If
		End If
		
		If dw_2.Object.ccre_tipore[Row] = 2 Then
			If existeloteoriginal(Integer(Data)) Then
				If il_lote1 <> 0 Then
					dw_2.Object.ccre_loteo1[1] = il_lote1 
				End If
				
				If il_lote2 <> 0 Then
					dw_2.Object.ccre_loteo2[1] = il_lote2
				End If
				
				If il_lote3 <> 0 Then
					dw_2.Object.ccre_loteo3[1] = il_lote3
				End If
				
				If il_lote4 <> 0 Then
					dw_2.Object.ccre_loteo4[1] = il_lote4
				End If
				
				If il_lote5 <> 0 Then
					dw_2.Object.ccre_loteo5[1] = il_lote5
				End If	
				dw_2.Object.ccre_fecpre[1] = id_fechaproc
				dw_2.Object.ccre_colorl[1] = is_calibre
			End If	
		End If	
		
	Case "ccre_expack"
		If Integer(Data) > 100 Then
			MessageBox("Atención","Porcentaje Exportación no puede ser mayor a 100")
			This.SetItem(Row, ls_Columna, Integer(ls_Nula))
			Return 1
		End If
		/* Or ii_Especie = 82  sacado por correo de Vcosta 20120425*/
		If ii_Especie = 27 Or ii_Especie = 78 Or ii_Especie = 23 Then
			If Not wf_ValidaPorcentaje(ls_Columna, Data, 1) Then
				MessageBox('Atención...', 'El procentaje total debe ser igual 100%', Information!, Ok!)
				This.SetItem(Row, ls_Columna, Integer(ls_Nula))
				Return 1
			End If
		End If
		
	Case "ccre_porexp"
		If Integer(Data) > 100 Then
			MessageBox("Atención","Porcentaje Exportación no puede ser mayor a 100")
			This.SetItem(Row, ls_Columna, Integer(ls_Nula))
			Return 1
		End If
		
		If ii_Especie = 27 Or ii_Especie = 78 Or ii_Especie = 82 Or ii_Especie = 23 Then
			If Not wf_ValidaPorcentaje(ls_Columna, Data, 1) Then
				MessageBox('Atención...', 'El procentaje total debe ser igual 100%', Information!, Ok!)
				This.SetItem(Row, ls_Columna, Integer(ls_Nula))
				Return 1
			End If
		End If
		
	Case "ccre_porxfa"
		If Integer(Data) > 100 Then
			MessageBox("Atención","Porcentaje Extra Fancy no puede ser mayor a 100")
			This.SetItem(Row, ls_Columna, Integer(ls_Nula))
			Return 1
		End If
		
		If ii_Especie = 78 Then
			If Not wf_ValidaPorcentaje(ls_Columna, Data, 1) Then
				MessageBox('Atención...', 'El procentaje total debe ser igual 100%', Information!, Ok!)
				This.SetItem(Row, ls_Columna, Integer(ls_Nula))
				Return 1
			End If
		End If

	Case "ccre_excome"
		If Integer(Data) > 100 Then
			MessageBox("Atención","Porcentaje Comercial no puede ser mayor a 100")
			This.SetItem(Row, ls_Columna, Integer(ls_Nula))
			Return 1
		End If

		If ii_Especie = 27 Or ii_Especie = 78 Or ii_Especie = 82 Or ii_Especie = 23 Then
			If Not wf_ValidaPorcentaje(ls_Columna, Data, 1) Then
				MessageBox('Atención...', 'El procentaje total debe ser igual 100%', Information!, Ok!)
				This.SetItem(Row, ls_Columna, Integer(ls_Nula))
				Return 1
			End If
		End If
		
	Case 'ccre_feccos'
		If Date(data) > Today() Then 
			MessageBox('Atención', 'Fecha de Cosecha no pueder ser mayor a fecha actual.', Exclamation!, Ok!)
			This.SetItem(1, ls_Columna, Date(ls_Nula))
			Return 1
		Else
			If Date(data) > This.Object.ccre_fecrec[Row] Then
				MessageBox("Atención","Fecha de cosecha no puede ser mayor a fecha recepcion.", Information!, Ok!)
				This.SetItem(1, ls_Columna, Date(ls_Nula))
				Return 1
			End If
		End If
		
	Case 'ccre_fecrec'
		If Date(data) > Today() Then 
			MessageBox('Atención', 'Fecha de Recepcion no pueder ser mayor a fecha actual.', Exclamation!, Ok!)
			This.SetItem(1, ls_Columna, Date(ls_Nula))
			Return 1
		Else
			If Date(data) < This.Object.ccre_feccos[Row] Then
				MessageBox("Atención","Fecha de recepcion no puede ser menor a fecha cosecha.", Information!, Ok!)
				This.SetItem(1, ls_Columna, Date(ls_Nula))
				Return 1
			ElseIf Date(data) > Today() Then
				MessageBox("Atención","Fecha de recepcion no puede ser Posterior a fecha Actual.", Information!, Ok!)
				This.SetItem(1, ls_Columna, Date(ls_Nula))
				Return 1
			End If
		End If
		
	Case 'ccre_fecins'
		If Date(data) > Today() Then 
			MessageBox('Atención', 'Fecha de Inspeccion no pueder ser mayor a fecha actual.', Exclamation!, Ok!)
			This.SetItem(1, ls_Columna, Date(ls_Nula))
			Return 1
		Else
			If Date(data) < This.Object.ccre_fecrec[Row] Then
				MessageBox("Atención","Fecha de Inspección no puede ser Anterior a fecha recepcion.", Information!, Ok!)
				This.SetItem(1, ls_Columna, Date(ls_Nula))
				Return 1
			End If
		End If
		
	Case 'ccre_hordes'
		If This.Object.ccre_fecins[Row] = This.Object.ccre_fecrec[Row] Then
			If Time(data) > This.Object.ccre_horing[Row] Then
				MessageBox("Atención","Fecha de Recepcion no puede ser Posterior a Hora Inspeccion.", Information!, Ok!)
				This.SetItem(1, ls_Columna, Date(ls_Nula))
				Return 1
			End If
		End If

	Case 'ccre_horing'
		If This.Object.ccre_fecins[Row] = This.Object.ccre_fecrec[Row] Then
			If Time(data) < This.Object.ccre_hordes[Row] Then
				MessageBox("Atención","Fecha de Inspeccion no puede ser Anterior a Hora Recepción.", Information!, Ok!)
				This.SetItem(1, ls_Columna, Date(ls_Nula))
				Return 1
			End If
		End If	
		
	Case 'ccre_pocafe', 'ccre_acidez'
		If Dec(Data) > 100 Then
			MessageBox("Atención","Porcentaje de frutos no puede ser mayor a 100")
			This.SetItem(Row, ls_Columna, Dec(ls_Nula))
			Return 1
		End If
		
	Case 'ccre_pocat1', 'ccre_pocat2', 'ccre_poplan'
		li_Zona = wf_ValidaCategoria(ls_Columna, Data, This, Row)
		If li_Zona = 2 Then
			MessageBox("Atención","Porcentaje de frutos no puede ser mayor a 100")
			This.SetItem(Row, ls_Columna, Dec(ls_Nula))
			Return 1
		ElseIf li_Zona = 1 Then
			MessageBox("Atención","Suma de Porcentajes de frutos no puede ser mayor a 100")
			This.SetItem(Row, ls_Columna, Dec(ls_Nula))
			Return 1						
		End If
		
End Choose

habilitaingreso(ls_Columna)
end event

event dw_2::clicked;call super::clicked;String	ls_columna

ls_columna = dwo.name

CHOOSE CASE ls_columna
	CASE "buscalote"
		buscalote()
			
END CHOOSE
end event

event dw_2::sqlpreview;//
end event

type pb_nuevo from w_mant_encab_deta_csd`pb_nuevo within w_maed_ctlcalplanillaespecies
integer x = 4709
integer y = 388
integer taborder = 40
end type

type pb_eliminar from w_mant_encab_deta_csd`pb_eliminar within w_maed_ctlcalplanillaespecies
integer x = 4704
integer y = 576
integer taborder = 60
end type

type pb_grabar from w_mant_encab_deta_csd`pb_grabar within w_maed_ctlcalplanillaespecies
integer x = 4704
integer y = 760
integer taborder = 50
end type

type pb_imprimir from w_mant_encab_deta_csd`pb_imprimir within w_maed_ctlcalplanillaespecies
integer x = 4704
integer y = 932
integer taborder = 70
end type

type pb_salir from w_mant_encab_deta_csd`pb_salir within w_maed_ctlcalplanillaespecies
integer x = 4704
integer y = 1116
integer taborder = 80
end type

type pb_ins_det from w_mant_encab_deta_csd`pb_ins_det within w_maed_ctlcalplanillaespecies
boolean visible = false
integer x = 4709
integer y = 1408
integer taborder = 0
end type

type pb_eli_det from w_mant_encab_deta_csd`pb_eli_det within w_maed_ctlcalplanillaespecies
boolean visible = false
integer x = 4709
integer y = 1580
integer taborder = 0
end type

type pb_buscar from w_mant_encab_deta_csd`pb_buscar within w_maed_ctlcalplanillaespecies
integer x = 4704
integer y = 216
integer taborder = 30
end type

type tab_1 from tab within w_maed_ctlcalplanillaespecies
integer x = 119
integer y = 908
integer width = 4421
integer height = 1196
integer taborder = 20
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16777215
boolean raggedright = true
boolean focusonbuttondown = true
alignment alignment = center!
integer selectedtab = 1
tabpage_2 tabpage_2
tabpage_5 tabpage_5
tabpage_1 tabpage_1
tabpage_4 tabpage_4
tabpage_3 tabpage_3
tabpage_6 tabpage_6
tabpage_7 tabpage_7
tabpage_8 tabpage_8
tabpage_9 tabpage_9
tabpage_10 tabpage_10
tabpage_11 tabpage_11
tabpage_12 tabpage_12
tabpage_13 tabpage_13
end type

on tab_1.create
this.tabpage_2=create tabpage_2
this.tabpage_5=create tabpage_5
this.tabpage_1=create tabpage_1
this.tabpage_4=create tabpage_4
this.tabpage_3=create tabpage_3
this.tabpage_6=create tabpage_6
this.tabpage_7=create tabpage_7
this.tabpage_8=create tabpage_8
this.tabpage_9=create tabpage_9
this.tabpage_10=create tabpage_10
this.tabpage_11=create tabpage_11
this.tabpage_12=create tabpage_12
this.tabpage_13=create tabpage_13
this.Control[]={this.tabpage_2,&
this.tabpage_5,&
this.tabpage_1,&
this.tabpage_4,&
this.tabpage_3,&
this.tabpage_6,&
this.tabpage_7,&
this.tabpage_8,&
this.tabpage_9,&
this.tabpage_10,&
this.tabpage_11,&
this.tabpage_12,&
this.tabpage_13}
end on

on tab_1.destroy
destroy(this.tabpage_2)
destroy(this.tabpage_5)
destroy(this.tabpage_1)
destroy(this.tabpage_4)
destroy(this.tabpage_3)
destroy(this.tabpage_6)
destroy(this.tabpage_7)
destroy(this.tabpage_8)
destroy(this.tabpage_9)
destroy(this.tabpage_10)
destroy(this.tabpage_11)
destroy(this.tabpage_12)
destroy(this.tabpage_13)
end on

type tabpage_2 from userobject within tab_1
integer x = 18
integer y = 168
integer width = 4384
integer height = 1012
long backcolor = 16777215
string text = "Defectos"
long tabtextcolor = 33554432
long tabbackcolor = 12632256
long picturemaskcolor = 536870912
dw_inspeccion dw_inspeccion
end type

on tabpage_2.create
this.dw_inspeccion=create dw_inspeccion
this.Control[]={this.dw_inspeccion}
end on

on tabpage_2.destroy
destroy(this.dw_inspeccion)
end on

type dw_inspeccion from uo_dw within tabpage_2
integer x = 50
integer y = 52
integer width = 2949
integer height = 864
integer taborder = 11
string dataobject = "dw_mues_ctlcalinspecalidadet"
boolean hscrollbar = true
boolean hsplitscroll = true
boolean livescroll = true
end type

event itemchanged;call super::itemchanged;String	ls_Columna
Long     ll_fila, ll_total, ll_defectos

ls_Columna	=	dwo.Name

CHOOSE CASE ls_Columna
	CASE "ccic_totfru"
		FOR ll_fila = 2 TO dw_4.RowCount()
			dw_4.Object.ccic_totfru[ll_fila] =  Long(data)
		NEXT 
		
  CASE "ccic_cantid"
		dw_4.Object.ccic_totfru[row] =  dw_4.Object.ccic_totfru[1]

END CHOOSE


IF ii_especie = 41 THEN
	//recupera_catguarda(gi_codexport,0,ii_especie,0,1)
END IF


end event

event sqlpreview;//
end event

type tabpage_5 from userobject within tab_1
boolean visible = false
integer x = 18
integer y = 168
integer width = 4384
integer height = 1012
long backcolor = 16777215
string text = "Otros~r~nCalidad"
long tabtextcolor = 33554432
long tabbackcolor = 12632256
long picturemaskcolor = 536870912
dw_inspecpromedio dw_inspecpromedio
end type

on tabpage_5.create
this.dw_inspecpromedio=create dw_inspecpromedio
this.Control[]={this.dw_inspecpromedio}
end on

on tabpage_5.destroy
destroy(this.dw_inspecpromedio)
end on

type dw_inspecpromedio from datawindow within tabpage_5
integer x = 50
integer y = 52
integer width = 2949
integer height = 868
integer taborder = 21
string title = "none"
string dataobject = "dw_mues_ctlcalinspecalicondi_det"
boolean hscrollbar = true
boolean vscrollbar = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event itemchanged;String		ls_Columna, ls_colu, ls_null
Long     	ll_fila
Integer	li_Numero

SetNull(ls_Null)
ls_Columna	=	dwo.Name

Choose Case ls_Columna
	Case "iccd_usumod"
		For ll_fila = 1 To dw_7.RowCount()
			dw_7.Object.iccd_usumod [ll_fila] =  Integer(Data)
		Next
			
	Case "iccd_clasif"
		If Integer(data) >= 1 And Integer(data) <= 5 Then
			For ll_fila = 1 To dw_7.RowCount()
				dw_7.Object.iccd_clasif[ll_fila] =  Integer(Data)
				
				If dw_7.Object.iccd_vpcnor[ll_Fila] = Integer(Data) Then
					dw_7.SetItem(ll_Fila, "iccd_usumod ", Integer(ls_Null))
				End If
			Next
			
			Choose Case Integer(data)
				Case 4
					dw_2.Object.ccre_reslot[1] = 0
				Case Else
					dw_2.Object.ccre_reslot[1] = Integer(ls_Null)
				End Choose
		Else
			MessageBox('Atencion...','Estado debe estar entre 1 y 5')
			This.SetItem(Row, ls_Columna, ls_Null)
			Return 1
		End If
		
	Case "iccd_vpcnor"
		If Integer(data) >= 1 And Integer(data) <= 5 Then
 			
			For ll_Fila = 1 To dw_7.RowCount()
				dw_7.Object.iccd_vpcnor[ll_fila]	=  Integer(Data)
				dw_7.Object.iccd_clasif[ll_fila]		=  Integer(Data)
				
				If dw_7.Object.iccd_clasif[ll_Fila] = Integer(Data) Then
					dw_7.SetItem(ll_Fila, "iccd_usumod", Integer(ls_Null))
				End If
			Next
			
			Choose Case Integer(data)
				Case 4
					dw_2.Object.ccre_reslot[1] = 0
				Case Else
					dw_2.Object.ccre_reslot[1] = Integer(ls_Null)
				End Choose
		Else
			MessageBox('Atencion...','Estado debe estar entre 1 y 4')
			This.SetItem(Row, ls_Columna, ls_Null)
			Return 1
		End If
		
	Case "iccd_mues01","iccd_mues02","iccd_mues03","iccd_mues04","iccd_mues05",&
			"iccd_mues06","iccd_mues07","iccd_mues08","iccd_mues09","iccd_mues10",&
			"iccd_mues11","iccd_mues12","iccd_mues13","iccd_mues14","iccd_mues15",&
			"iccd_mues16","iccd_mues17","iccd_mues18","iccd_mues19","iccd_mues20",&
			"iccd_mues21","iccd_mues22","iccd_mues23","iccd_mues24","iccd_mues25",&
			"iccd_mues26","iccd_mues27","iccd_mues28","iccd_mues29","iccd_mues30"

		If IsNull(data) Then
			If Integer(Mid(ls_columna,10,2))>= 1 Then
				li_Numero	=	Integer(Mid(ls_columna,10,2)) //- 1
				If li_Numero = 0 Then li_Numero = 1
				
				ls_colu 		= 	Fill("0",2 - Len(String(li_Numero))) + String(li_Numero)
				istr_mant.argumento[3] = ls_Colu
			End If
		Else
			ls_colu = Mid(ls_columna,10,2)
		End If
		
		If li_Numero = 0 Then istr_mant.argumento[1] = ls_Null
		If li_Numero = 0 Then istr_mant.argumento[2] = ls_Null

		If ii_Especie = 82 Then 
			If This.RowCount() >= 4  And Row < 5 Then Calcula_Relacion(Row, Dec(Data), ls_Columna)
		End If
			
		Calcula_Promedio(row,ls_colu)	
		wf_Calcula_MinMax(Row, Mid(ls_columna,10,2), 1, This, True)
		wf_Calcula_MinMax(Row, Mid(ls_columna,10,2), 1, This, False)
		If ii_Especie = 21 Then wf_Calcula_Promedio_FB(Row, ls_Colu)	
		If ii_Especie <> 10 And ii_Especie <> 26 And ii_Especie <> 27 And ii_Especie <> 78 AND ii_Especie <> 82 and ii_Especie <> 36 Then Desviacion_Estandar(Row, ls_colu, 1, This)
End Choose

If ii_especie = 41 Then
	//recupera_catguarda(gi_codexport,0,ii_especie,0,1)
End If
end event

event itemerror;Return 1
end event

type tabpage_1 from userobject within tab_1
integer x = 18
integer y = 168
integer width = 4384
integer height = 1012
long backcolor = 16777215
string text = "Color"
long tabtextcolor = 33554432
long tabbackcolor = 12632256
long picturemaskcolor = 536870912
dw_evaluacion dw_evaluacion
end type

on tabpage_1.create
this.dw_evaluacion=create dw_evaluacion
this.Control[]={this.dw_evaluacion}
end on

on tabpage_1.destroy
destroy(this.dw_evaluacion)
end on

type dw_evaluacion from uo_dw within tabpage_1
integer x = 50
integer y = 52
integer width = 4334
integer height = 860
integer taborder = 11
string dataobject = "dw_mues_ctlcalinpecevaluadet"
boolean hscrollbar = true
boolean hsplitscroll = true
boolean livescroll = true
end type

event itemchanged;call super::itemchanged;String	ls_Columna, ls_colu, ls_null, ls_Columnas
Long     ll_fila
Integer	li_Numero, li_candat, li_columna
SetNull(ls_null)

ls_Columna	=	dwo.Name
Message.DoubleParm = 0

Choose Case ls_Columna
	Case "ccec_clasif"
		If Integer(data) >= 1 And Integer(data) <= 4 Then
			
			For ll_fila = 2 TO dw_3.RowCount()
				dw_3.Object.ccec_clasIf[ll_fila] =  Integer(data)
			Next
			
			Choose Case Integer(data)
				Case 4
					dw_2.Object.ccre_reslot[1] = 0
					
				Case Else
					dw_2.Object.ccre_reslot[1] = Integer(ls_null)
					
				End Choose
		Else
			MessageBox('Atencion...','Estado debe estar entre 1 y 4')
			This.SetItem(row, ls_Columna, ls_Null)
			Return 1
		End If
		
	CASE "ccec_mues01","ccec_mues02","ccec_mues03","ccec_mues04","ccec_mues05","ccec_mues06",&
			"ccec_mues07","ccec_mues08","ccec_mues09","ccec_mues10","ccec_mues11","ccec_mues12",&
			"ccec_mues13","ccec_mues14","ccec_mues15","ccec_mues16","ccec_mues17","ccec_mues18",&
			"ccec_mues19","ccec_mues20","ccec_mues21","ccec_mues22","ccec_mues23","ccec_mues24",&
			"ccec_mues25"
		/*20141103 se agrega porcentaje*/
		IF ii_especie = 21 THEN
			This.Object.ccec_porcen[row] = dec(data)
		END IF
			
		/*
		Deshabilita muestras deacuerdo a cantidad a digitar en evaluación de calidad
		*/
		
		li_Columna = (This.GetColumn() - 6)
		li_candat  = This.Object.ccev_candat[row]
			
		If li_Columna > li_candat Then
			MessageBox("Error","No puede ingresar mas de " + String(li_candat) + " muestras",StopSign!)
		//	FOR li_columna = li_Columna TO 25
		//		 If li_columna >= 10 Then
		//			 ls_columna	=	"ccec_mues" +  String(li_columna)
		//		 Else
		//			 ls_columna	=	"ccec_mues" + String(0) + String(li_columna)
		//		 End If
				 This.Modify(ls_columna + ".protect='1'") 
		//	NEXT
			Return 1
		End If
	
		/*********************************************/

      If IsNull(data) Then
			If Integer(Mid(ls_columna,10,2))>= 1 Then
				li_Numero	=	Integer(Mid(ls_columna,10,2)) - 1
				ls_colu 		= 	Fill("0",2 - Len(String(li_Numero))) + String(li_Numero)
				istr_mant.argumento[2] = ls_Colu
			End If
		Else
			ls_colu = Mid(ls_columna,10,2)
		End If
		
		If /* ii_especie = 27 Or ii_especie = 26 OR ii_especie = 78  OR ii_especie = 82 Or */ii_Especie = 21 Then
			Calcula_Rango(row, ls_colu)
			If Message.DoubleParm = -1 Then 
				This.SetItem(Row, ls_Columna, Dec(ls_Null))
				Return 1
			End If
		Else
			If li_Numero = 0 Then istr_mant.argumento[1] = ls_Null
			If li_Numero = 0 Then istr_mant.argumento[2] = ls_Null
			
			/*Calcula_Media(Row, ls_Colu, 1, This) //ESTAS FUNCIONES YA NO SE USAN EN ESTA TABPAGE
			Desviacion_Estandar(Row, ls_Colu, 1, This)*/
		End If
		If ii_Especie = 41 Then
			Calcula_Media(Row, ls_colu, 1, This)
			Desviacion_Estandar(Row,ls_Colu, 1,This)
		End If
		
//	CASE "ctlcalinpecevaluadet_ccec_claosc_1"
//		this.object.ccec_claosc[row] = integer(data)//this.object.ctlcalinpecevaluadet_ccec_claosc_1[row] 
		
End Choose
	
//If ii_especie = 41 Then
//	recupera_catguarda(gi_codexport,0,ii_especie,0,1)
//End If
end event

event clicked;call super::clicked;Integer li_candat, li_colum

IF row > 0 THEN
	li_candat = This.Object.ccev_candat[Row]
	li_colum  = (This.GetClickedColumn() - 6)
	IF li_colum > li_candat THEN
		habilita(False)
	ELSE
		habilita(True)
	END IF
ELSE
	RETURN 1
END IF


end event

event sqlpreview;//
end event

type tabpage_4 from userobject within tab_1
boolean visible = false
integer x = 18
integer y = 168
integer width = 4384
integer height = 1012
long backcolor = 16777215
string text = "Madurez "
long tabtextcolor = 33554432
long tabbackcolor = 12632256
long picturemaskcolor = 536870912
dw_madurez dw_madurez
end type

on tabpage_4.create
this.dw_madurez=create dw_madurez
this.Control[]={this.dw_madurez}
end on

on tabpage_4.destroy
destroy(this.dw_madurez)
end on

type dw_madurez from datawindow within tabpage_4
accessiblerole accessiblerole = cursorrole!
integer x = 50
integer y = 52
integer width = 1595
integer height = 840
integer taborder = 30
string title = "none"
string dataobject = "dw_mues_ctlcalevalumaducolor_det"
boolean vscrollbar = true
boolean hsplitscroll = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event itemchanged;Integer li_Numero
String	ls_Columna, ls_colu, ls_dato
Long     ll_fila

ls_Columna	=	dwo.Name

If ii_especie = 81 Then
	Choose Case ls_Columna
		Case "evmc_muestr","evmc_mues02","evmc_mues03","evmc_mues04","evmc_mues05",&
			  "evmc_mues06","evmc_mues07","evmc_mues08","evmc_mues09","evmc_mues10"
		
			If IsNull(data) Then
				If Integer(Mid(ls_columna,10,2))>= 1 Then
					ls_dato = (Mid(ls_columna,10,2))
					If ls_dato = 'tr' Then
						ls_columna = 'evmc_mues01'
					End If	
					
					li_Numero	=	Integer(Mid(ls_columna,10,2)) - 1
					ls_colu 		= 	Fill("0",2 - Len(String(li_Numero))) + String(li_Numero)
					istr_mant.argumento[3] = ls_Colu
				End If
			Else
				ls_dato = (Mid(ls_columna,10,2))
				If ls_dato = 'tr' Then
					ls_columna = 'evmc_mues01'
				End If	
				ls_colu = Mid(ls_columna,10,2)
			End If
			
			Calcula_Promedio_aceites(row, ls_colu)
			wf_Calcula_MinMax(Row, Mid(ls_columna,10,2), 2, This, True)
			wf_Calcula_MinMax(Row, Mid(ls_columna,10,2), 2, This, False)		
	End Choose
ElseIf ii_especie = 26 Or ii_especie = 36 Or ii_especie = 27 Then
	Choose Case ls_Columna
		Case "evmc_muestr"
			Calcula_Relacion(Row, Dec(data), "evmc_muestr")

	End Choose
ElseIf ii_especie = 41 Then
		//recupera_catguarda(gi_codexport,0,ii_especie,0,1)
End If	

end event

type tabpage_3 from userobject within tab_1
integer x = 18
integer y = 168
integer width = 4384
integer height = 1012
long backcolor = 16777215
string text = "Calibres"
long tabtextcolor = 33554432
long tabbackcolor = 12632256
long picturemaskcolor = 536870912
dw_distribucion dw_distribucion
end type

on tabpage_3.create
this.dw_distribucion=create dw_distribucion
this.Control[]={this.dw_distribucion}
end on

on tabpage_3.destroy
destroy(this.dw_distribucion)
end on

type dw_distribucion from uo_dw within tabpage_3
integer x = 50
integer y = 52
integer width = 2949
integer height = 840
integer taborder = 11
string dataobject = "dw_mues_ctlcalinspdistribcaldet"
boolean hsplitscroll = true
boolean livescroll = true
end type

event itemchanged;call super::itemchanged;IF ii_especie = 41 THEN
	//recupera_catguarda(gi_codexport,0,ii_especie,0,1)
END IF
end event

event sqlpreview;//
end event

type tabpage_6 from userobject within tab_1
boolean visible = false
integer x = 18
integer y = 168
integer width = 4384
integer height = 1012
long backcolor = 16777215
string text = "Firmeza~r~n"
long tabtextcolor = 33554432
long tabbackcolor = 12632256
long picturemaskcolor = 536870912
dw_firmeza dw_firmeza
end type

on tabpage_6.create
this.dw_firmeza=create dw_firmeza
this.Control[]={this.dw_firmeza}
end on

on tabpage_6.destroy
destroy(this.dw_firmeza)
end on

type dw_firmeza from datawindow within tabpage_6
integer x = 50
integer y = 52
integer width = 4279
integer height = 848
integer taborder = 21
string title = "none"
string dataobject = "dw_mues_ctlcalevalfirmezadet"
boolean hscrollbar = true
boolean vscrollbar = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event itemchanged;String	ls_Columna, ls_colu, ls_null, ls_Columnas
Long     ll_fila
Integer	li_Numero, li_candat

AcceptText()

SetNull(ls_null)

ls_Columna	=	dwo.Name

Choose Case ls_Columna
	Case "ccef_mue001","ccef_mue002","ccef_mue003","ccef_mue004","ccef_mue005","ccef_mue006",&
		"ccef_mue007","ccef_mue008","ccef_mue009","ccef_mue010","ccef_mue011","ccef_mue012",&
		"ccef_mue013","ccef_mue014","ccef_mue015","ccef_mue016","ccef_mue017","ccef_mue018",&
		"ccef_mue019","ccef_mue020","ccef_mue021","ccef_mue022","ccef_mue023",&
		"ccef_mue024","ccef_mue025","ccef_mue026","ccef_mue027","ccef_mue028","ccef_mue029",&
		"ccef_mue030","ccef_mue031","ccef_mue032","ccef_mue033","ccef_mue034","ccef_mue035",&
		"ccef_mue036","ccef_mue037","ccef_mue038","ccef_mue039","ccef_mue040","ccef_mue041",&
		"ccef_mue042","ccef_mue043","ccef_mue044","ccef_mue045","ccef_mue046","ccef_mue047",&
		"ccef_mue048","ccef_mue049","ccef_mue050","ccef_mue051","ccef_mue052","ccef_mue053",&
		"ccef_mue054","ccef_mue055","ccef_mue056","ccef_mue057","ccef_mue058","ccef_mue059",&
		"ccef_mue060","ccef_mue061","ccef_mue062","ccef_mue063","ccef_mue064","ccef_mue065",&
		"ccef_mue066","ccef_mue067","ccef_mue068","ccef_mue069","ccef_mue070","ccef_mue071",&
		"ccef_mue072","ccef_mue073","ccef_mue074","ccef_mue075","ccef_mue076","ccef_mue077",&
		"ccef_mue078","ccef_mue079","ccef_mue080","ccef_mue081","ccef_mue082","ccef_mue083",&
		"ccef_mue084","ccef_mue085","ccef_mue086","ccef_mue087","ccef_mue088","ccef_mue089",&
		"ccef_mue090","ccef_mue091","ccef_mue092","ccef_mue093","ccef_mue094","ccef_mue095",&
		"ccef_mue096","ccef_mue097","ccef_mue098","ccef_mue099","ccef_mue100"
			
		/*********************************************/

      If IsNull(data) Then
			If Integer(Mid(ls_columna,9,2))>= 1 Then
				li_Numero	=	Integer(Mid(ls_columna,10,3)) - 1
				ls_colu 		= 	Fill("0",3 - Len(String(li_Numero))) + String(li_Numero)
				istr_mant.argumento[2] = ls_Colu
			End If
		ELSE
			ls_colu = Mid(ls_columna,9,3)
		End If
		
		If ii_especie = 41 Then
			Desviacion_Estandar(Row, ls_Colu, 3, This)
  			Calculo_Profir(Row, ls_Colu)
		End If
		
	End Choose
	
If ii_especie = 41 Then
//	recupera_catguarda(gi_codexport,0,ii_especie,0,1)
End If
end event

type tabpage_7 from userobject within tab_1
boolean visible = false
integer x = 18
integer y = 168
integer width = 4384
integer height = 1012
long backcolor = 16777215
string text = "Cat Guarda~r~nRecepción"
long tabtextcolor = 33554432
long tabbackcolor = 12632256
long picturemaskcolor = 536870912
dw_catguarda dw_catguarda
end type

on tabpage_7.create
this.dw_catguarda=create dw_catguarda
this.Control[]={this.dw_catguarda}
end on

on tabpage_7.destroy
destroy(this.dw_catguarda)
end on

type dw_catguarda from datawindow within tabpage_7
integer x = 50
integer y = 52
integer width = 2953
integer height = 884
integer taborder = 40
string title = "none"
string dataobject = "dw_mues_categoriaguarda_selec"
boolean vscrollbar = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event itemchanged;String	ls_Columna
Long	ll_Fila
AcceptText()

ls_Columna	=	dwo.Name

Choose Case ls_Columna
	Case "cccg_selecc"
		If Data = '1' Then
			For ll_Fila = 1 To This.RowCount()
				This.SetItem(ll_Fila, ls_Columna, 0)
			Next
		End If

End Choose
end event

event itemerror;Return 1
end event

type tabpage_8 from userobject within tab_1
boolean visible = false
integer x = 18
integer y = 168
integer width = 4384
integer height = 1012
long backcolor = 16777215
string text = "Pronostico~r~nBotrytis"
long tabtextcolor = 33554432
long tabbackcolor = 12632256
long picturemaskcolor = 536870912
dw_botrytis dw_botrytis
end type

on tabpage_8.create
this.dw_botrytis=create dw_botrytis
this.Control[]={this.dw_botrytis}
end on

on tabpage_8.destroy
destroy(this.dw_botrytis)
end on

type dw_botrytis from datawindow within tabpage_8
integer x = 50
integer y = 52
integer width = 3017
integer height = 892
integer taborder = 40
string title = "none"
string dataobject = "dw_mues_pronbotrytis"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type tabpage_9 from userobject within tab_1
integer x = 18
integer y = 168
integer width = 4384
integer height = 1012
long backcolor = 16777215
string text = "Observaciones"
long tabtextcolor = 33554432
long tabbackcolor = 12632256
string picturename = "Comment!"
long picturemaskcolor = 536870912
mle_observacion mle_observacion
end type

on tabpage_9.create
this.mle_observacion=create mle_observacion
this.Control[]={this.mle_observacion}
end on

on tabpage_9.destroy
destroy(this.mle_observacion)
end on

type mle_observacion from multilineedit within tabpage_9
integer x = 50
integer y = 52
integer width = 4256
integer height = 848
integer taborder = 21
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
boolean autovscroll = true
borderstyle borderstyle = stylelowered!
end type

type tabpage_10 from userobject within tab_1
boolean visible = false
integer x = 18
integer y = 168
integer width = 4384
integer height = 1012
long backcolor = 16777215
string text = "Materia~r~nSeca"
long tabtextcolor = 33554432
long tabbackcolor = 12632256
long picturemaskcolor = 536870912
dw_materiaseca dw_materiaseca
end type

on tabpage_10.create
this.dw_materiaseca=create dw_materiaseca
this.Control[]={this.dw_materiaseca}
end on

on tabpage_10.destroy
destroy(this.dw_materiaseca)
end on

type dw_materiaseca from uo_dw within tabpage_10
integer x = 50
integer y = 52
integer width = 4306
integer height = 960
integer taborder = 11
string dataobject = "dw_mues_ctlcalevalmateriasecadet"
boolean vscrollbar = false
boolean livescroll = true
end type

event itemchanged;call super::itemchanged;String		ls_Columna, ls_colu, ls_null, ls_Columnas
Long     	ll_fila
Integer	li_Numero, li_candat

AcceptText()
SetNull(ls_null)
ls_Columna	=	dwo.Name

// Comentado porque no desean calcular datos	

Choose Case ls_Columna
	
	Case	"ccms_mues01","ccms_mues02","ccms_mues03","ccms_mues04","ccms_mues05",&
			"ccms_mues06","ccms_mues07","ccms_mues08","ccms_mues09","ccms_mues10",&
			"ccms_mues11","ccms_mues12","ccms_mues13","ccms_mues14","ccms_mues15",&
			"ccms_mues16","ccms_mues17","ccms_mues18","ccms_mues19","ccms_mues20",&
			"ccms_mues21","ccms_mues22","ccms_mues23","ccms_mues24","ccms_mues25"
		If IsNull(data) Then
			If Integer(Mid(ls_columna, 10, 2))>= 1 Then
				li_Numero	=	Integer(Mid(ls_columna, 10, 2)) - 1
				ls_colu 		= 	Fill("0", 2 - Len(String(li_Numero))) + String(li_Numero)
				istr_mant.argumento[2] = ls_Colu
			End If
		Else
			ls_colu = Mid(ls_columna, 10, 2)
		End If
		
		If li_Numero = 0 Then istr_mant.argumento[1] = ls_Null
		If li_Numero = 0 Then istr_mant.argumento[2] = ls_Null

		Desviacion_Estandar(Row, ls_Colu, 2, This)
		Calcula_Media(Row, ls_Colu, 2, This)
		wf_Calcula_MinMax(Row, ls_colu, 3, This, True)
		wf_Calcula_MinMax(Row, ls_colu, 3, This, False)		

End Choose
end event

type tabpage_11 from userobject within tab_1
boolean visible = false
integer x = 18
integer y = 168
integer width = 4384
integer height = 1012
long backcolor = 16777215
string text = "Cat Guarda~r~nHuerto"
long tabtextcolor = 33554432
long tabbackcolor = 12632256
long picturemaskcolor = 536870912
rb_ac2 rb_ac2
rb_fc2 rb_fc2
rb_ac1 rb_ac1
rb_fc1 rb_fc1
gb_3 gb_3
end type

on tabpage_11.create
this.rb_ac2=create rb_ac2
this.rb_fc2=create rb_fc2
this.rb_ac1=create rb_ac1
this.rb_fc1=create rb_fc1
this.gb_3=create gb_3
this.Control[]={this.rb_ac2,&
this.rb_fc2,&
this.rb_ac1,&
this.rb_fc1,&
this.gb_3}
end on

on tabpage_11.destroy
destroy(this.rb_ac2)
destroy(this.rb_fc2)
destroy(this.rb_ac1)
destroy(this.rb_fc1)
destroy(this.gb_3)
end on

type rb_ac2 from radiobutton within tabpage_11
integer x = 2327
integer y = 584
integer width = 704
integer height = 76
boolean bringtotop = true
integer textsize = -12
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string pointer = "AppStarting!"
long textcolor = 16777215
long backcolor = 553648127
string text = "AC 2"
end type

type rb_fc2 from radiobutton within tabpage_11
integer x = 1358
integer y = 584
integer width = 704
integer height = 76
boolean bringtotop = true
integer textsize = -12
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string pointer = "AppStarting!"
long textcolor = 33554431
long backcolor = 553648127
string text = "FC 2"
end type

type rb_ac1 from radiobutton within tabpage_11
integer x = 2341
integer y = 388
integer width = 704
integer height = 76
boolean bringtotop = true
integer textsize = -12
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "AC 1"
end type

type rb_fc1 from radiobutton within tabpage_11
integer x = 1358
integer y = 388
integer width = 704
integer height = 76
boolean bringtotop = true
integer textsize = -12
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string pointer = "AppStarting!"
long textcolor = 16777215
long backcolor = 553648127
string text = "FC 1"
end type

type gb_3 from groupbox within tabpage_11
integer x = 1216
integer y = 232
integer width = 1888
integer height = 568
integer taborder = 50
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554431
long backcolor = 16711680
string text = " Tipo Frio"
end type

type tabpage_12 from userobject within tab_1
boolean visible = false
integer x = 18
integer y = 168
integer width = 4384
integer height = 1012
long backcolor = 16777215
string text = "Alternaria"
long tabtextcolor = 33554432
long tabbackcolor = 12632256
long picturemaskcolor = 536870912
dw_alternaria dw_alternaria
end type

on tabpage_12.create
this.dw_alternaria=create dw_alternaria
this.Control[]={this.dw_alternaria}
end on

on tabpage_12.destroy
destroy(this.dw_alternaria)
end on

type dw_alternaria from uo_dw within tabpage_12
integer x = 50
integer y = 52
integer width = 1742
integer height = 840
integer taborder = 11
string dataobject = "dw_mues_alternariadeta"
boolean hscrollbar = true
end type

type tabpage_13 from userobject within tab_1
boolean visible = false
integer x = 18
integer y = 168
integer width = 4384
integer height = 1012
long backcolor = 16777215
string text = "Segregación"
long tabtextcolor = 33554432
long tabbackcolor = 12632256
long picturemaskcolor = 536870912
dw_segregacion dw_segregacion
end type

on tabpage_13.create
this.dw_segregacion=create dw_segregacion
this.Control[]={this.dw_segregacion}
end on

on tabpage_13.destroy
destroy(this.dw_segregacion)
end on

type dw_segregacion from uo_dw within tabpage_13
integer x = 50
integer y = 52
integer width = 3017
integer height = 908
integer taborder = 11
string dataobject = "dw_mues_segregaciondet"
end type

event itemchanged;call super::itemchanged;String	ls_Columna
Long	ll_Fila
AcceptText()

ls_Columna	=	dwo.Name

Choose Case ls_Columna
	Case "ccsd_activa"
		If Data = '1' Then
			For ll_Fila = 1 To This.RowCount()
				This.SetItem(ll_Fila, ls_Columna, 0)
			Next
		End If

End Choose
end event

