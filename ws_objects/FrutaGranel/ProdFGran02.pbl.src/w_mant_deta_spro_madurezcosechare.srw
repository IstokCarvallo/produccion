$PBExportHeader$w_mant_deta_spro_madurezcosechare.srw
forward
global type w_mant_deta_spro_madurezcosechare from w_mant_detalle
end type
end forward

global type w_mant_deta_spro_madurezcosechare from w_mant_detalle
integer width = 2720
integer height = 2216
string title = "MADUREZ DE COSECHA RECEPCION"
end type
global w_mant_deta_spro_madurezcosechare w_mant_deta_spro_madurezcosechare

type variables
uo_colordefondo	iuo_ColorFondo
uo_ParamMadurez	iuo_ParamMadurez
uo_variedades		iuo_variedad

DataWindowChild	idwc_ColorFondo

Integer ii_colorFondo, ii_calibre, ii_grupo, ii_subgrupo //segùn parametro de cosecha 0=No,  1=Si
end variables

forward prototypes
public function boolean duplicado (string campo, integer tipo)
public subroutine habilitacolumnas ()
end prototypes

public function boolean duplicado (string campo, integer tipo);Long		ll_fila
String	mcor_grucal, cofo_codigo

mcor_grucal	=	dw_1.Object.mcor_grucal[il_fila]
cofo_codigo	=	String(dw_1.Object.cofo_codigo[il_fila])

CHOOSE CASE tipo
	CASE 1
		mcor_grucal	=	Campo

	CASE 2
		cofo_codigo	=	Campo

END CHOOSE

ll_fila = dw_1.Find("mcor_grucal = '" + mcor_grucal + "' AND " + &
						  "cofo_codigo = " + cofo_codigo + "", 1, dw_1.RowCount())

IF ll_fila > 0 and ll_fila <> il_fila THEN
	MessageBox("Error","Registro ya fue ingresado anteriormente",Information!, OK!)
   RETURN True
ELSE
	RETURN False
END IF
end function

public subroutine habilitacolumnas ();Long		ll_ColorSol, ll_ColorPreE, ll_ColorPreH, ll_ColorPreA, ll_ColorAlm, &
			ll_ColorAcu, ll_ColorMoh, ll_colorAcei, ll_ColorHume
Integer	li_Solidos, li_PresEcu, li_PresHom, li_PresApi, li_Almidon, li_Acuoso, &
			li_Mohoso, li_Aceite, li_Humedad
Boolean  lb_existe = FALSE			
			
IF iuo_variedad.existe(Integer(istr_Mant.Argumento[2]),Integer(istr_Mant.Argumento[4]),&
                       TRUE,SQLCA) THEN
	ii_grupo 	= 	iuo_variedad.grupo
	ii_subgrupo	=	iuo_variedad.subgrupo
ELSE
	SetNull(ii_grupo)
	SetNull(ii_subgrupo)
END IF

IF NOT iuo_ParamMadurez.Existe(Integer(istr_Mant.Argumento[2]),ii_grupo,ii_subgrupo,&
											Integer(istr_Mant.Argumento[4]), &
											FALSE, Sqlca) THEN
	IF NOT iuo_ParamMadurez.Existe(Integer(istr_Mant.Argumento[2]),ii_grupo,ii_subgrupo,&
											-1, &
											FALSE, Sqlca) THEN
		IF NOT iuo_ParamMadurez.Existe(Integer(istr_Mant.Argumento[2]),ii_grupo,-1,&
											-1, &
											FALSE, Sqlca) THEN
			IF NOT iuo_ParamMadurez.Existe(Integer(istr_Mant.Argumento[2]),-1,-1,&
											-1, &
											True, Sqlca) THEN
			   lb_existe = FALSE
			ELSE
				lb_existe = TRUE
			END IF
		ELSE
			lb_existe = TRUE	
		END IF
	ELSE
		lb_existe = TRUE	
	END IF
ELSE
 	lb_existe = TRUE	
END IF	
	
IF lb_existe THEN	

//iuo_ParamMadurez.Existe(Integer(istr_mant.Argumento[2]),li_Solidos,li_Solidos,Integer(istr_mant.Argumento[4]),True,SQLCA)

	IF iuo_ParamMadurez.SolidosSol		= 1 THEN
		ll_ColorSol	=	RGB(255,255,255)
		li_Solidos	=	0
	ELSE
		ll_ColorSol	=	RGB(192,192,192)
		li_Solidos	=	1
	END IF
	
	IF iuo_ParamMadurez.PresEcuatorial	= 1 THEN
		ll_ColorPreE	=	RGB(255,255,255)
		li_PresEcu		=	0
	ELSE
		ll_ColorPreE	=	RGB(192,192,192)
		li_PresEcu		=	1
	END IF
	
	IF iuo_ParamMadurez.PresHombros		= 1 THEN
		ll_ColorPreH	=	RGB(255,255,255)
		li_PresHom		=	0
	ELSE
		ll_ColorPreH	=	RGB(192,192,192)
		li_PresHom		=	1
	END IF
	
	IF iuo_ParamMadurez.PresApice			= 1 THEN
		ll_ColorPreA	=	RGB(255,255,255)
		li_PresApi		=	0
	ELSE
		ll_ColorPreA	=	RGB(192,192,192)
		li_PresApi		=	1
	END IF
	
	IF iuo_ParamMadurez.Almidon			= 1 THEN
		ll_ColorAlm		=	RGB(255,255,255)
		li_Almidon		=	0
	ELSE
		ll_ColorAlm		=	RGB(192,192,192)
		li_Almidon		=	1
	END IF
	
	IF iuo_ParamMadurez.Acuoso				= 1 THEN
		ll_ColorAcu		=	RGB(255,255,255)
		li_Acuoso		=	0
	ELSE
		ll_ColorAcu		=	RGB(192,192,192)
		li_Acuoso		=	1
	END IF
	
	IF iuo_ParamMadurez.Mohoso				= 1 THEN
		ll_ColorMoh		=	RGB(255,255,255)
		li_Mohoso		=	0
	ELSE
		ll_ColorMoh		=	RGB(192,192,192)
		li_Mohoso		=	1
	END IF
	
	IF iuo_ParamMadurez.Aceite				= 1 THEN
		ll_ColorAcei		=	RGB(255,255,255)
		li_Aceite			=	0
	ELSE
		ll_ColorAcei		=	RGB(192,192,192)
		li_Aceite			=	1
	END IF
	
	IF iuo_ParamMadurez.Humedad			= 1 THEN
		ll_ColorHume	=	RGB(255,255,255)
		li_Humedad		=	0
	ELSE
		ll_ColorHume	=	RGB(192,192,192)
		li_Humedad		=	1
	END IF


	dw_1.Object.mcor_ssolpr.Protect				=	li_Solidos
	dw_1.Object.mcor_ssolpr.BackGround.Color	=	ll_ColorSol
	dw_1.Object.mcor_ssolmi.Protect				=	li_Solidos
	dw_1.Object.mcor_ssolmi.BackGround.Color	=	ll_ColorSol
	dw_1.Object.mcor_ssolma.Protect				=	li_Solidos
	dw_1.Object.mcor_ssolma.BackGround.Color	=	ll_ColorSol
	dw_1.Object.mcor_preepr.Protect				=	li_PresEcu
	dw_1.Object.mcor_preepr.BackGround.Color	=	ll_ColorPreE
	dw_1.Object.mcor_preemi.Protect				=	li_PresEcu
	dw_1.Object.mcor_preemi.BackGround.Color	=	ll_ColorPreE
	dw_1.Object.mcor_preema.Protect				=	li_PresEcu
	dw_1.Object.mcor_preema.BackGround.Color	=	ll_ColorPreE
	dw_1.Object.mcor_prehpr.Protect				=	li_PresHom
	dw_1.Object.mcor_prehpr.BackGround.Color	=	ll_ColorPreH
	dw_1.Object.mcor_prehmi.Protect				=	li_PresHom
	dw_1.Object.mcor_prehmi.BackGround.Color	=	ll_ColorPreH
	dw_1.Object.mcor_prehma.Protect				=	li_PresHom
	dw_1.Object.mcor_prehma.BackGround.Color	=	ll_ColorPreH
	dw_1.Object.mcor_preapr.Protect				=	li_PresApi
	dw_1.Object.mcor_preapr.BackGround.Color	=	ll_ColorPreA
	dw_1.Object.mcor_preami.Protect				=	li_PresApi
	dw_1.Object.mcor_preami.BackGround.Color	=	ll_ColorPreA
	dw_1.Object.mcor_preama.Protect				=	li_PresApi
	dw_1.Object.mcor_preama.BackGround.Color	=	ll_ColorPreA
	dw_1.Object.mcor_talmpr.Protect				=	li_Almidon
	dw_1.Object.mcor_talmpr.BackGround.Color	=	ll_ColorAlm
	dw_1.Object.mcor_talmmi.Protect				=	li_Almidon
	dw_1.Object.mcor_talmmi.BackGround.Color	=	ll_ColorAlm
	dw_1.Object.mcor_talmma.Protect				=	li_Almidon
	dw_1.Object.mcor_talmma.BackGround.Color	=	ll_ColorAlm
	//
	dw_1.Object.mcor_aceipr.Protect				=	li_Aceite
	dw_1.Object.mcor_aceipr.BackGround.Color	=	ll_ColorAcei
	dw_1.Object.mcor_aceimi.Protect				=	li_Aceite
	dw_1.Object.mcor_aceimi.BackGround.Color	=	ll_ColorAcei
	dw_1.Object.mcor_aceima.Protect				=	li_Aceite
	dw_1.Object.mcor_aceima.BackGround.Color	=	ll_ColorAcei
	dw_1.Object.mcor_humepr.Protect				=	li_Humedad
	dw_1.Object.mcor_humepr.BackGround.Color	=	ll_ColorHume
	dw_1.Object.mcor_humemi.Protect				=	li_Humedad
	dw_1.Object.mcor_humemi.BackGround.Color	=	ll_ColorHume
	dw_1.Object.mcor_humema.Protect				=	li_Humedad
	dw_1.Object.mcor_humema.BackGround.Color	=	ll_ColorHume
	//
	dw_1.Object.mcor_coacle.Protect				=	li_Acuoso
	dw_1.Object.mcor_coacle.BackGround.Color	=	ll_ColorAcu
	dw_1.Object.mcor_coacmo.Protect				=	li_Acuoso
	dw_1.Object.mcor_coacmo.BackGround.Color	=	ll_ColorAcu
	dw_1.Object.mcor_coacse.Protect				=	li_Acuoso
	dw_1.Object.mcor_coacse.BackGround.Color	=	ll_ColorAcu
	dw_1.Object.mcor_comole.Protect				=	li_Mohoso
	dw_1.Object.mcor_comole.BackGround.Color	=	ll_ColorMoh
	dw_1.Object.mcor_comomo.Protect				=	li_Mohoso
	dw_1.Object.mcor_comomo.BackGround.Color	=	ll_ColorMoh
	dw_1.Object.mcor_comose.Protect				=	li_Mohoso
	dw_1.Object.mcor_comose.BackGround.Color	=	ll_ColorMoh

END IF 

RETURN
end subroutine

on w_mant_deta_spro_madurezcosechare.create
call super::create
end on

on w_mant_deta_spro_madurezcosechare.destroy
call super::destroy
end on

event ue_recuperadatos();call super::ue_recuperadatos;Integer li_Null
Boolean lb_existe = FALSE

SetNull(li_Null)

ias_campo[1]  = dw_1.Object.mcor_grucal[il_fila]
//ias_campo[2]  = String(dw_1.Object.vari_codigo[il_fila])
ias_campo[3]  = String(dw_1.Object.cofo_codigo[il_fila])
ias_campo[4]  = String(dw_1.Object.mcor_ssolpr[il_fila])
ias_campo[5]  = String(dw_1.Object.mcor_ssolmi[il_fila])
ias_campo[6]  = String(dw_1.Object.mcor_ssolma[il_fila])
ias_campo[7]  = String(dw_1.Object.mcor_preepr[il_fila])
ias_campo[8]  = String(dw_1.Object.mcor_preemi[il_fila])
ias_campo[9]  = String(dw_1.Object.mcor_preema[il_fila])
ias_campo[10] = String(dw_1.Object.mcor_prehpr[il_fila])
ias_campo[11] = String(dw_1.Object.mcor_prehmi[il_fila])
ias_campo[12] = String(dw_1.Object.mcor_prehma[il_fila])
ias_campo[13] = String(dw_1.Object.mcor_preapr[il_fila])
ias_campo[14] = String(dw_1.Object.mcor_preami[il_fila])
ias_campo[15] = String(dw_1.Object.mcor_preama[il_fila])
ias_campo[16] = String(dw_1.Object.mcor_talmpr[il_fila])
ias_campo[17] = String(dw_1.Object.mcor_talmmi[il_fila])
ias_campo[18] = String(dw_1.Object.mcor_talmma[il_fila])
ias_campo[19] = String(dw_1.Object.mcor_coacle[il_fila])
ias_campo[20] = String(dw_1.Object.mcor_coacmo[il_fila])
ias_campo[21] = String(dw_1.Object.mcor_coacse[il_fila])
ias_campo[22] = String(dw_1.Object.mcor_comole[il_fila])
ias_campo[23] = String(dw_1.Object.mcor_comomo[il_fila])
ias_campo[24] = String(dw_1.Object.mcor_comose[il_fila])
ias_campo[25] = String(dw_1.Object.mcor_acidpr[il_fila])
ias_campo[26] = String(dw_1.Object.mcor_acidmi[il_fila])
ias_campo[27] = String(dw_1.Object.mcor_acidma[il_fila])
ias_campo[28] = String(dw_1.Object.mcor_sofipr[il_fila])
ias_campo[29] = String(dw_1.Object.mcor_sofimi[il_fila])
ias_campo[30] = String(dw_1.Object.mcor_sofima[il_fila])
ias_campo[31] = String(dw_1.Object.mcor_grasle[il_fila])
ias_campo[32] = String(dw_1.Object.mcor_grasmo[il_fila])
ias_campo[33] = String(dw_1.Object.mcor_grasse[il_fila])
ias_campo[34] = String(dw_1.Object.mcor_harile[il_fila])
ias_campo[35] = String(dw_1.Object.mcor_harimo[il_fila])
ias_campo[36] = String(dw_1.Object.mcor_harise[il_fila])
ias_campo[37] = String(dw_1.Object.mcor_matspr[il_fila])
ias_campo[38] = String(dw_1.Object.mcor_matsmi[il_fila])
ias_campo[39] = String(dw_1.Object.mcor_matsma[il_fila])

dw_1.SetItem(il_fila, "lote_pltcod", Integer(istr_mant.argumento[1]))
dw_1.SetItem(il_fila, "lote_espcod", Integer(istr_mant.argumento[2]))
dw_1.SetItem(il_fila, "lote_codigo", Integer(istr_mant.argumento[3]))
//dw_1.SetItem(il_fila, "vari_codigo", Integer(istr_mant.argumento[4]))


IF ias_campo[1]='GLO' THEN
	dw_1.Object.cofo_codigo.Protect 				=	1
	dw_1.Object.cofo_codigo.BackGround.Color	=	RGB(192,192,192)
END IF	

IF iuo_variedad.existe(Integer(istr_Mant.Argumento[2]),Integer(istr_Mant.Argumento[4]),&
                       TRUE,SQLCA) THEN
	ii_grupo 	= 	iuo_variedad.grupo
	ii_subgrupo	=	iuo_variedad.subgrupo
ELSE
	SetNull(ii_grupo)
	SetNull(ii_subgrupo)
END IF

IF NOT iuo_ParamMadurez.Existe(Integer(istr_Mant.Argumento[2]),ii_grupo,ii_subgrupo,&
											Integer(istr_Mant.Argumento[4]), &
											FALSE, Sqlca) THEN
	IF NOT iuo_ParamMadurez.Existe(Integer(istr_Mant.Argumento[2]),ii_grupo,ii_subgrupo,&
											-1, &
											FALSE, Sqlca) THEN
		IF NOT iuo_ParamMadurez.Existe(Integer(istr_Mant.Argumento[2]),ii_grupo,-1,&
											-1, &
											FALSE, Sqlca) THEN
			IF NOT iuo_ParamMadurez.Existe(Integer(istr_Mant.Argumento[2]),-1,-1,&
											-1, &
											True, Sqlca) THEN
			   lb_existe = FALSE
			ELSE
				lb_existe = TRUE
			END IF
		ELSE
			lb_existe = TRUE	
		END IF
	ELSE
		lb_existe = TRUE	
	END IF
ELSE
 	lb_existe = TRUE	
END IF	
	
IF lb_existe THEN	
	IF iuo_ParamMadurez.Calibre = 0 THEN
		dw_1.Object.mcor_grucal.Protect 				=	1
		dw_1.Object.mcor_grucal.BackGround.Color	=	RGB(192,192,192)
		ii_calibre = 0
	ElSE
	   ii_calibre = 1
   END IF
	
	IF iuo_ParamMadurez.Color = 0 THEN
		dw_1.Object.cofo_codigo.Protect 				=	1
		dw_1.Object.cofo_codigo.BackGround.Color	=	RGB(192,192,192)
		ii_colorfondo = 0
	ElSE
	   ii_colorfondo = 1
		
		dw_1.GetChild("cofo_codigo", idwc_colorfondo)
		idwc_colorfondo.SetTransObject(SqlCa)			
		IF idwc_colorfondo.Retrieve(Integer(istr_mant.argumento[2]),ii_grupo,ii_subgrupo, &
											  Integer(istr_mant.argumento[4])) = 0 THEN
			IF idwc_colorfondo.Retrieve(Integer(istr_mant.argumento[2]),ii_grupo,ii_subgrupo, &
												 -1) = 0 THEN	
				IF idwc_colorfondo.Retrieve(Integer(istr_mant.argumento[2]),ii_grupo,-1, &
													  -1) = 0 THEN	
					IF idwc_colorfondo.Retrieve(Integer(istr_mant.argumento[2]),-1,-1, &
														  -1) = 0 THEN	
						idwc_colorfondo.InsertRow(0)
					END IF	
				END IF	
			END IF
		END IF
   END IF	

   IF iuo_ParamMadurez.SolidosSol = 0 THEN
		dw_1.Object.mcor_ssolpr.Protect 				=	1
		dw_1.Object.mcor_ssolpr.BackGround.Color	=	RGB(192,192,192)
		dw_1.Object.mcor_ssolmi.Protect 				=	1
		dw_1.Object.mcor_ssolmi.BackGround.Color	=	RGB(192,192,192)
		dw_1.Object.mcor_ssolma.Protect 				=	1
		dw_1.Object.mcor_ssolma.BackGround.Color	=	RGB(192,192,192)
   END IF
	
   IF iuo_ParamMadurez.PresEcuatorial = 0 THEN
		dw_1.Object.mcor_preepr.Protect 				=	1
		dw_1.Object.mcor_preepr.BackGround.Color	=	RGB(192,192,192)
		dw_1.Object.mcor_preemi.Protect 				=	1
		dw_1.Object.mcor_preemi.BackGround.Color	=	RGB(192,192,192)
		dw_1.Object.mcor_preema.Protect 				=	1
		dw_1.Object.mcor_preema.BackGround.Color	=	RGB(192,192,192)
   END IF

   IF iuo_ParamMadurez.PresHombros = 0 THEN
		dw_1.Object.mcor_prehpr.Protect 				=	1
		dw_1.Object.mcor_prehpr.BackGround.Color	=	RGB(192,192,192)
		dw_1.Object.mcor_prehmi.Protect 				=	1
		dw_1.Object.mcor_prehmi.BackGround.Color	=	RGB(192,192,192)
		dw_1.Object.mcor_prehma.Protect 				=	1
		dw_1.Object.mcor_prehma.BackGround.Color	=	RGB(192,192,192)
   END IF

   IF iuo_ParamMadurez.PresApice = 0 THEN
		dw_1.Object.mcor_preapr.Protect 				=	1
		dw_1.Object.mcor_preapr.BackGround.Color	=	RGB(192,192,192)
		dw_1.Object.mcor_preami.Protect 				=	1
		dw_1.Object.mcor_preami.BackGround.Color	=	RGB(192,192,192)
		dw_1.Object.mcor_preama.Protect 				=	1
		dw_1.Object.mcor_preama.BackGround.Color	=	RGB(192,192,192)
   END IF
	
   IF iuo_ParamMadurez.Almidon = 0 THEN
		dw_1.Object.mcor_talmpr.Protect 				=	1
		dw_1.Object.mcor_talmpr.BackGround.Color	=	RGB(192,192,192)
		dw_1.Object.mcor_talmmi.Protect 				=	1
		dw_1.Object.mcor_talmmi.BackGround.Color	=	RGB(192,192,192)
		dw_1.Object.mcor_talmma.Protect 				=	1
		dw_1.Object.mcor_talmma.BackGround.Color	=	RGB(192,192,192)
   END IF

   IF iuo_ParamMadurez.Acuoso = 0 THEN
		dw_1.Object.mcor_coacle.Protect 				=	1
		dw_1.Object.mcor_coacle.BackGround.Color	=	RGB(192,192,192)
		dw_1.Object.mcor_coacmo.Protect 				=	1
		dw_1.Object.mcor_coacmo.BackGround.Color	=	RGB(192,192,192)
		dw_1.Object.mcor_coacse.Protect 				=	1
		dw_1.Object.mcor_coacse.BackGround.Color	=	RGB(192,192,192)
   END IF

   IF iuo_ParamMadurez.Mohoso = 0 THEN
		dw_1.Object.mcor_comole.Protect 				=	1
		dw_1.Object.mcor_comole.BackGround.Color	=	RGB(192,192,192)
		dw_1.Object.mcor_comomo.Protect 				=	1
		dw_1.Object.mcor_comomo.BackGround.Color	=	RGB(192,192,192)
		dw_1.Object.mcor_comose.Protect 				=	1
		dw_1.Object.mcor_comose.BackGround.Color	=	RGB(192,192,192)
   END IF
	
   IF iuo_ParamMadurez.Acidez = 0 THEN
		dw_1.Object.mcor_acidpr.Protect 				=	1
		dw_1.Object.mcor_acidpr.BackGround.Color	=	RGB(192,192,192)
		dw_1.Object.mcor_acidmi.Protect 				=	1
		dw_1.Object.mcor_acidmi.BackGround.Color	=	RGB(192,192,192)
		dw_1.Object.mcor_acidma.Protect 				=	1
		dw_1.Object.mcor_acidma.BackGround.Color	=	RGB(192,192,192)
   END IF

   IF iuo_ParamMadurez.Harinosidad = 0 THEN
		dw_1.Object.mcor_harise.Protect 				=	1
		dw_1.Object.mcor_harise.BackGround.Color	=	RGB(192,192,192)
		dw_1.Object.mcor_harimo.Protect 				=	1
		dw_1.Object.mcor_harimo.BackGround.Color	=	RGB(192,192,192)
		dw_1.Object.mcor_harile.Protect 				=	1
		dw_1.Object.mcor_harile.BackGround.Color	=	RGB(192,192,192)
   END IF
	
   IF iuo_ParamMadurez.Grasitud = 0 THEN
		dw_1.Object.mcor_grasse.Protect 				=	1
		dw_1.Object.mcor_grasse.BackGround.Color	=	RGB(192,192,192)
		dw_1.Object.mcor_grasmo.Protect 				=	1
		dw_1.Object.mcor_grasmo.BackGround.Color	=	RGB(192,192,192)
		dw_1.Object.mcor_grasle.Protect 				=	1
		dw_1.Object.mcor_grasle.BackGround.Color	=	RGB(192,192,192)
   END IF
	
   IF iuo_ParamMadurez.SolFinales = 0 THEN
		dw_1.Object.mcor_sofipr.Protect 				=	1
		dw_1.Object.mcor_sofipr.BackGround.Color	=	RGB(192,192,192)
		dw_1.Object.mcor_sofimi.Protect 				=	1
		dw_1.Object.mcor_sofimi.BackGround.Color	=	RGB(192,192,192)
		dw_1.Object.mcor_sofima.Protect 				=	1
		dw_1.Object.mcor_sofima.BackGround.Color	=	RGB(192,192,192)
   END IF

   IF iuo_ParamMadurez.MateriaSeca = 0 THEN
		dw_1.Object.mcor_matspr.Protect 				=	1
		dw_1.Object.mcor_matspr.BackGround.Color	=	RGB(192,192,192)
		dw_1.Object.mcor_matsmi.Protect 				=	1
		dw_1.Object.mcor_matsmi.BackGround.Color	=	RGB(192,192,192)
		dw_1.Object.mcor_matsma.Protect 				=	1
		dw_1.Object.mcor_matsma.BackGround.Color	=	RGB(192,192,192)
   END IF

END IF


dw_1.SetFocus()
end event

event ue_deshace();call super::ue_deshace;IF UpperBound(ias_campo) > 0 THEN
	dw_1.SetItem(il_fila, "mcor_grucal", ias_campo[1])
	//dw_1.SetItem(il_fila, "vari_codigo", Integer(ias_campo[2]))
	dw_1.SetItem(il_fila, "cofo_codigo", Integer(ias_campo[3]))
	dw_1.SetItem(il_fila, "mcor_ssolpr", Dec(ias_campo[4]))
	dw_1.SetItem(il_fila, "mcor_ssolmi", Dec(ias_campo[5]))
	dw_1.SetItem(il_fila, "mcor_ssolma", Dec(ias_campo[6]))
	dw_1.SetItem(il_fila, "mcor_preepr", Dec(ias_campo[7]))
	dw_1.SetItem(il_fila, "mcor_preemi", Dec(ias_campo[8]))
	dw_1.SetItem(il_fila, "mcor_preema", Dec(ias_campo[9]))
	dw_1.SetItem(il_fila, "mcor_prehpr", Dec(ias_campo[10]))
	dw_1.SetItem(il_fila, "mcor_prehmi", Dec(ias_campo[11]))
	dw_1.SetItem(il_fila, "mcor_prehma", Dec(ias_campo[12]))
	dw_1.SetItem(il_fila, "mcor_preapr", Dec(ias_campo[13]))
	dw_1.SetItem(il_fila, "mcor_preami", Dec(ias_campo[14]))
	dw_1.SetItem(il_fila, "mcor_preama", Dec(ias_campo[15]))
	dw_1.SetItem(il_fila, "mcor_talmpr", Dec(ias_campo[16]))
	dw_1.SetItem(il_fila, "mcor_talmmi", Dec(ias_campo[17]))
	dw_1.SetItem(il_fila, "mcor_talmma", Dec(ias_campo[18]))
	dw_1.SetItem(il_fila, "mcor_coacle", Dec(ias_campo[19]))
	dw_1.SetItem(il_fila, "mcor_coacmo", Dec(ias_campo[20]))
	dw_1.SetItem(il_fila, "mcor_coacse", Dec(ias_campo[21]))
	dw_1.SetItem(il_fila, "mcor_comole", Dec(ias_campo[22]))
	dw_1.SetItem(il_fila, "mcor_comomo", Dec(ias_campo[23]))
	dw_1.SetItem(il_fila, "mcor_comose", Dec(ias_campo[24]))
END IF
end event

event ue_antesguardar();Integer	li_cont
String	ls_mensaje, ls_mens[], ls_colu[]

IF ii_calibre = 1 THEN
	IF Isnull(dw_1.GetItemString(il_fila, "mcor_grucal")) OR dw_1.GetItemString(il_fila, "mcor_grucal") = "" THEN
		li_cont ++
		ls_mensaje = 	ls_mensaje + "~nGrupo de Calibre"
		ls_colu[li_cont] = "mcor_grucal"
	END IF
ELSE
	dw_1.Object.mcor_grucal[il_fila] = "   "
END IF

//IF Isnull(dw_1.GetItemNumber(il_fila, "vari_codigo")) OR dw_1.GetItemNumber(il_fila, "vari_codigo") = 0 THEN
//	li_cont ++
//	ls_mensaje = 	ls_mensaje + "~nCódigo de Variedad"
//	ls_colu[li_cont] = "vari_codigo"
//END IF

IF Isnull(dw_1.GetItemNumber(il_fila, "cofo_codigo")) OR dw_1.GetItemNumber(il_fila, "cofo_codigo") = 0 THEN
	IF dw_1.Object.mcor_grucal[il_fila]<>'GLO' And ii_colorfondo=1 THEN
   	li_cont ++
	   ls_mensaje = 	ls_mensaje + "~nColor de Fondo"
	   ls_colu[li_cont] = "cofo_codigo"
   END IF		
END IF

IF li_cont > 0 THEN
	MessageBox("Error de Consistencia", "Falta el ingreso de :" + ls_mensaje + ".", StopSign!, Ok!)
	dw_1.SetColumn(ls_colu[1])
	dw_1.SetFocus()
	Message.DoubleParm = -1
END IF
end event

event open;/*
istr_mant.argumento[1] = Código Planta
istr_mant.argumento[2] = Código Especie
istr_mant.argumento[3] = Número Lote
istr_mant.argumento[4] = Código Variedad
*/
Integer li_Null
SetNull(li_Null)

iuo_ColorFondo		=	Create uo_colordefondo
iuo_ParamMadurez	=	Create uo_ParamMadurez
iuo_variedad		=	Create uo_variedades

x	= 100
y	= 450

This.Icon	=	Gstr_apl.Icono

PostEvent("ue_recuperadatos")

istr_mant = Message.PowerObjectParm

dw_1.GetChild("cofo_codigo", idwc_colorfondo)
idwc_colorfondo.SetTransObject(SqlCa)

IF iuo_variedad.existe(Integer(istr_Mant.Argumento[2]),Integer(istr_Mant.Argumento[4]),&
                       TRUE,SQLCA) THEN
	ii_grupo 	= 	iuo_variedad.grupo
	ii_subgrupo	=	iuo_variedad.subgrupo
ELSE
	SetNull(ii_grupo)
	SetNull(ii_subgrupo)
END IF
			
IF idwc_colorfondo.Retrieve(Integer(istr_mant.argumento[2]),ii_grupo,ii_subgrupo, &
                             Integer(istr_mant.argumento[4])) = 0 THEN
	IF idwc_colorfondo.Retrieve(Integer(istr_mant.argumento[2]),ii_grupo,ii_subgrupo, &
                                -1) = 0 THEN	
		IF idwc_colorfondo.Retrieve(Integer(istr_mant.argumento[2]),ii_grupo,-1, &
                                   -1) = 0 THEN	
			IF idwc_colorfondo.Retrieve(Integer(istr_mant.argumento[2]),-1,-1, &
	                                   -1) = 0 THEN	
				idwc_colorfondo.InsertRow(0)
			END IF	
		END IF	
	END IF
END IF

dw_1.SetTransObject(sqlca)
istr_mant.dw.ShareData(dw_1)

HabilitaColumnas()

end event

event ue_nuevo();call super::ue_nuevo;dw_1.SetItem(il_fila, "lote_pltcod", Integer(istr_mant.argumento[1]))
dw_1.SetItem(il_fila, "lote_espcod", Integer(istr_mant.argumento[2]))
dw_1.SetItem(il_fila, "lote_codigo", Integer(istr_mant.argumento[3]))
//dw_1.SetItem(il_fila, "vari_codigo", Integer(istr_mant.argumento[4]))
end event

type pb_ultimo from w_mant_detalle`pb_ultimo within w_mant_deta_spro_madurezcosechare
string tag = ""
end type

type pb_siguiente from w_mant_detalle`pb_siguiente within w_mant_deta_spro_madurezcosechare
string tag = ""
end type

type pb_anterior from w_mant_detalle`pb_anterior within w_mant_deta_spro_madurezcosechare
string tag = ""
end type

type pb_primero from w_mant_detalle`pb_primero within w_mant_deta_spro_madurezcosechare
string tag = ""
end type

type pb_cancela from w_mant_detalle`pb_cancela within w_mant_deta_spro_madurezcosechare
string tag = ""
integer x = 2377
integer y = 496
end type

type pb_acepta from w_mant_detalle`pb_acepta within w_mant_deta_spro_madurezcosechare
string tag = ""
integer x = 2377
integer y = 280
end type

type pb_salir from w_mant_detalle`pb_salir within w_mant_deta_spro_madurezcosechare
string tag = ""
integer x = 2377
integer y = 712
end type

type dw_1 from w_mant_detalle`dw_1 within w_mant_deta_spro_madurezcosechare
integer y = 48
integer width = 2112
integer height = 2028
string dataobject = "dw_mant_spro_madurezcosechare"
end type

event dw_1::itemchanged;String	ls_Columna, ls_Null

SetNull(ls_Null)

ls_Columna	=	dwo.Name

CHOOSE CASE ls_Columna

	CASE "mcor_grucal"
		IF Duplicado(data, 1) THEN
			This.SetItem(il_fila, ls_Columna, ls_Null)
			RETURN 1
		ELSE
			IF Data='GLO' or ii_colorfondo = 0 THEN
			  This.Object.cofo_codigo[row]					=	Integer(ls_Null)
			  This.Object.cofo_codigo.Protect				=	1
			  This.Object.cofo_codigo.BackGround.Color 	= 	RGB(192,192,192)
		   ELSE
  			  This.Object.cofo_codigo.Protect 				=	0
			  This.Object.cofo_codigo.BackGround.Color 	=	RGB(255,255,255)
		   END IF
		END IF

	CASE "cofo_codigo"
		IF iuo_ColorFondo.Existe(Integer(istr_Mant.Argumento[2]), ii_grupo, ii_subgrupo,&
											Integer(istr_Mant.Argumento[4]), &
											Integer(data), FALSE, Sqlca) THEN
			IF Duplicado(data, 2) THEN
				This.SetItem(il_fila, ls_Columna, Integer(ls_Null))
				RETURN 1
			END IF	
		ELSE
			IF iuo_ColorFondo.Existe(Integer(istr_Mant.Argumento[2]), ii_grupo, ii_subgrupo,&
											-1, Integer(data), FALSE, Sqlca) THEN
				IF Duplicado(data, 2) THEN
					This.SetItem(il_fila, ls_Columna, Integer(ls_Null))
					RETURN 1
				END IF	
			ELSE
				IF iuo_ColorFondo.Existe(Integer(istr_Mant.Argumento[2]), ii_grupo, -1,&
											-1, Integer(data), FALSE, Sqlca) THEN
					IF Duplicado(data, 2) THEN
						This.SetItem(il_fila, ls_Columna, Integer(ls_Null))
						RETURN 1
					END IF	
				ELSE
					IF iuo_ColorFondo.Existe(Integer(istr_Mant.Argumento[2]), -1, -1,&
		   										 -1, Integer(data), True, Sqlca) THEN
						IF Duplicado(data, 2) THEN
							This.SetItem(il_fila, ls_Columna, Integer(ls_Null))
						END IF
					END IF	
				END IF	
			END IF	
		END IF

	CASE "mcor_ssolpr", "mcor_ssolmi", "mcor_ssolma"
		IF Not iuo_ParamMadurez.CorrespondeRango("Sólido Soluble", Data) THEN
			This.SetItem(il_fila, ls_Columna, Dec(ls_Null))
			RETURN 1
		END IF
			
	CASE "mcor_preepr", "mcor_preemi", "mcor_preema"
		IF Not iuo_ParamMadurez.CorrespondeRango("Presión Ecuatorial", Data) THEN
			This.SetItem(il_fila, ls_Columna, Dec(ls_Null))
			RETURN 1
		END IF
			
	CASE "mcor_prehpr", "mcor_prehmi", "mcor_prehma"
		IF Not iuo_ParamMadurez.CorrespondeRango("Presión Hombros", Data) THEN
			This.SetItem(il_fila, ls_Columna, Dec(ls_Null))
			RETURN 1
		END IF
			
	CASE "mcor_preapr", "mcor_preami", "mcor_preama"
		IF Not iuo_ParamMadurez.CorrespondeRango("Presión Apice", Data) THEN
			This.SetItem(il_fila, ls_Columna, Dec(ls_Null))
			RETURN 1
		END IF
			
	CASE "mcor_talmpr", "mcor_talmmi", "mcor_talmma"
		IF Not iuo_ParamMadurez.CorrespondeRango("Test Almidón", Data) THEN
			This.SetItem(il_fila, ls_Columna, Dec(ls_Null))
			RETURN 1
		END IF
			
	CASE "mcor_acidpr", "mcor_acidmi", "mcor_acidma"
		IF Not iuo_ParamMadurez.CorrespondeRango("Acidez", Data) THEN
			This.SetItem(il_fila, ls_Columna, Dec(ls_Null))
			RETURN 1
		END IF
			
	CASE "mcor_sofipr", "mcor_sofimi", "mcor_sofima"
		IF Not iuo_ParamMadurez.CorrespondeRango("Solidos Finales", Data) THEN
			This.SetItem(il_fila, ls_Columna, Dec(ls_Null))
			RETURN 1
		END IF
		
	CASE "mcor_matspr", "mcor_matsmi", "mcor_matsma"
		IF Not iuo_ParamMadurez.CorrespondeRango("Materia Seca", Data) THEN
			This.SetItem(il_fila, ls_Columna, Dec(ls_Null))
			RETURN 1
		END IF

	CASE "mcor_aceipr", "mcor_aceimi", "mcor_aceima"
		IF Not iuo_ParamMadurez.CorrespondeRango("Aceite", Data) THEN
			This.SetItem(il_fila, ls_Columna, Dec(ls_Null))
			RETURN 1
		END IF

	CASE "mcor_humepr", "mcor_humemi", "mcor_humema"
		IF Not iuo_ParamMadurez.CorrespondeRango("Humedad", Data) THEN
			This.SetItem(il_fila, ls_Columna, Dec(ls_Null))
			RETURN 1
		END IF
		
END CHOOSE
end event

