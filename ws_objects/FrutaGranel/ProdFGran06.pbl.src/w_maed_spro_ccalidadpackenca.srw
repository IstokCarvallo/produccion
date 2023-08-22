$PBExportHeader$w_maed_spro_ccalidadpackenca.srw
$PBExportComments$Mantenedor Encabezado de Control de Calidad.
forward
global type w_maed_spro_ccalidadpackenca from w_mant_encab_deta_csd
end type
type tab_1 from tab within w_maed_spro_ccalidadpackenca
end type
type tp_1 from userobject within tab_1
end type
type dw_calidades from uo_dw within tp_1
end type
type tp_1 from userobject within tab_1
dw_calidades dw_calidades
end type
type tp_2 from userobject within tab_1
end type
type dw_analfirm from uo_dw within tp_2
end type
type tp_2 from userobject within tab_1
dw_analfirm dw_analfirm
end type
type tp_3 from userobject within tab_1
end type
type dw_analpeso from uo_dw within tp_3
end type
type tp_3 from userobject within tab_1
dw_analpeso dw_analpeso
end type
type tab_1 from tab within w_maed_spro_ccalidadpackenca
tp_1 tp_1
tp_2 tp_2
tp_3 tp_3
end type
type dw_6 from datawindow within w_maed_spro_ccalidadpackenca
end type
type dw_7 from datawindow within w_maed_spro_ccalidadpackenca
end type
end forward

global type w_maed_spro_ccalidadpackenca from w_mant_encab_deta_csd
integer width = 3616
integer height = 2048
string title = "CONTROL CALIDAD PACKING"
string menuname = ""
tab_1 tab_1
dw_6 dw_6
dw_7 dw_7
end type
global w_maed_spro_ccalidadpackenca w_maed_spro_ccalidadpackenca

type variables
w_mant_deta_calidadpackdetdanos	iw_mantencion_1

uo_plantadesp				iuo_PltaDestino
uo_especie					iuo_Especie
uo_lineapacking			iuo_lineapacking
uo_variedades				iuo_variedades
uo_parammadurez			iuo_parammadurez
uo_spro_ordenproceso		iuo_ordenproceso

Boolean						ib_Modifica, ib_AutoCommit

DataWindowChild   		idwc_LineaPacking, idwc_planta, idwc_especie, &
								idwc_productor, idwc_varireal, idwc_varirot, &
								idwc_tratamiento, idwc_periodo, idwc_categoria, &
								idwc_embalaje, idwc_tipoenv, idwc_envase1, idwc_danos, &
								idwc_calibre
DataWindow					dw_3, dw_4, dw_5

str_embalaje				istr_Embalaje

DataStore					ids_Base

DateTime	idt_FechaSistema
end variables

forward prototypes
public function boolean existefolio (integer ai_planta, long al_folio)
protected function boolean wf_actualiza_db (boolean borrando)
public subroutine habilitaencab (boolean habilita)
public subroutine generadetalle ()
public function boolean duplicadoenvase (string as_columna, string as_valor)
public subroutine habilitacolumnas ()
public subroutine buscaembalaje ()
public function boolean corresponderango (string as_grupocolumnas, string as_valor)
public subroutine habilitaingreso (string as_columna)
public subroutine buscaorden ()
end prototypes

public function boolean existefolio (integer ai_planta, long al_folio);Long		ll_Numero
Boolean	lb_Retorno = True

SELECT	ccap_folio
	INTO	:ll_Numero
	FROM	dba.spro_ccalidadpackenca
	WHERE	plde_codigo	=	:ai_Planta
	AND	ccap_folio	=	:al_Folio;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla de Control de Calidad Packing")
	
	lb_Retorno	=	False
ELSEIF sqlca.SQLCode = 0 THEN
	istr_mant.Argumento[2]	=	String(al_Folio)
	This.TriggerEvent("ue_recuperadatos")
ELSE
	MessageBox("Atención","Número de Folio No ha sido generado. Ingrese Otro.")
	lb_Retorno	=	False
END IF

RETURN lb_Retorno
end function

protected function boolean wf_actualiza_db (boolean borrando);Boolean	lb_Retorno

IF Not dw_2.uf_check_required(0) THEN RETURN False

IF Not dw_1.uf_validate(0) THEN RETURN False

IF Borrando THEN
	IF dw_7.Update(True, False) = 1 THEN
		IF dw_3.Update(True, False) = 1 THEN
			IF dw_4.Update(True, False) = 1 THEN
				IF dw_5.Update(True, False) = 1 THEN
					IF dw_2.Update(True,False) = 1 THEN
						Commit;
					
						IF sqlca.SQLCode <> 0 THEN
							F_ErrorBaseDatos(sqlca, This.Title)
							RollBack;
						ELSE
							lb_Retorno	=	True
							
							dw_6.ResetUpdate()
							dw_5.ResetUpdate()
							dw_4.ResetUpdate()
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
		ELSE
			F_ErrorBaseDatos(sqlca, This.Title)
			RollBack;
		END IF
	ELSE
		F_ErrorBaseDatos(sqlca, This.Title)
	END IF
ELSE
	IF dw_2.Update(True, False) = 1 THEN
		IF dw_3.Update(True, False) = 1 THEN
			IF dw_7.Update(True, False) = 1 THEN
				IF dw_4.Update(True, False) = 1 THEN
					IF dw_5.Update(True,False) = 1 THEN
						Commit;
					
						IF sqlca.SQLCode <> 0 THEN
							F_ErrorBaseDatos(sqlca, This.Title)
						ELSE
							lb_Retorno	=	True
							
							dw_7.ResetUpdate()
							dw_5.ResetUpdate()
							dw_4.ResetUpdate()
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
		ELSE
			F_ErrorBaseDatos(sqlca, This.Title)
			RollBack;
		END IF
	ELSE
		F_ErrorBaseDatos(sqlca, This.Title)
		RollBack;
	END IF
END IF

sqlca.AutoCommit	=	ib_AutoCommit

RETURN lb_Retorno
end function

public subroutine habilitaencab (boolean habilita);IF Habilita THEN
	dw_2.Object.ccap_folio.Protect				=	0
	dw_2.Object.ccap_folio.BackGround.Color	=	RGB(255,255,255)
//	dw_2.Object.vari_codrot.Protect				=	0
//	dw_2.Object.vari_codrot.BackGround.Color	=	RGB(255,255,255)
	dw_2.Object.ccap_feccon.Protect				=	0
	dw_2.Object.ccap_feccon.BackGround.Color	=	RGB(255,255,255)
	dw_2.Object.line_codigo.Protect				=	0
	dw_2.Object.line_codigo.BackGround.Color	=	RGB(255,255,255)
	dw_2.Object.ccap_turno.Protect				=	0
	dw_2.Object.ccap_turno.BackGround.Color	=	RGB(255,255,255)
	dw_2.Object.ccap_clapal.Protect				=	0
	dw_2.Object.ccap_clapal.BackGround.Color	=	RGB(255,255,255)
	dw_2.Object.ccap_claeti.Protect				=	0
	dw_2.Object.ccap_claeti.BackGround.Color	=	RGB(255,255,255)
	dw_2.Object.ccap_claenc.Protect				=	0
	dw_2.Object.ccap_claenc.BackGround.Color	=	RGB(255,255,255)
	dw_2.Object.ccap_observ.Protect				=	0
	dw_2.Object.ccap_observ.BackGround.Color	=	RGB(255,255,255)
	dw_2.Enabled	=	True
ELSE
	dw_2.Object.ccap_folio.Protect				=	1
	dw_2.Object.ccap_folio.BackGround.Color	=	RGB(192,192,192)
//	dw_2.Object.vari_codrot.Protect				=	1
//	dw_2.Object.vari_codrot.BackGround.Color	=	RGB(192,192,192)
	dw_2.Object.ccap_feccon.Protect				=	1
	dw_2.Object.ccap_feccon.BackGround.Color	=	RGB(192,192,192)
	dw_2.Object.line_codigo.Protect				=	1
	dw_2.Object.line_codigo.BackGround.Color	=	RGB(192,192,192)
	dw_2.Object.ccap_turno.Protect				=	1
	dw_2.Object.ccap_turno.BackGround.Color	=	RGB(192,192,192)
	dw_2.Object.ccap_clapal.Protect				=	1
	dw_2.Object.ccap_clapal.BackGround.Color	=	RGB(192,192,192)
	dw_2.Object.ccap_claeti.Protect				=	1
	dw_2.Object.ccap_claeti.BackGround.Color	=	RGB(192,192,192)
	dw_2.Object.ccap_claenc.Protect				=	1
	dw_2.Object.ccap_claenc.BackGround.Color	=	RGB(192,192,192)
	dw_2.Object.ccap_observ.Protect				=	1
	dw_2.Object.ccap_observ.BackGround.Color	=	RGB(192,192,192)
	dw_2.Enabled	=	False
END IF
end subroutine

public subroutine generadetalle ();Long		ll_Fila, ll_Filas, ll_FilaNueva
Integer	li_Null

SetNull(li_Null)

//	Llena Distribución de Calibre
	
IF dw_4.RowCount() = 0 THEN
	ids_Base.DataObject	=	"dw_mues_spro_espevarigrucal"
	ids_Base.SetTransObject(sqlca)
	
	ll_Filas	=	ids_Base.Retrieve(Integer(istr_Mant.Argumento[3]),&
											Integer(istr_Mant.Argumento[6]))

	IF ll_Filas = 0 THEN
		ll_Filas	=	ids_Base.Retrieve(Integer(istr_Mant.Argumento[3]),&
												li_Null)
	END IF

	IF ll_Filas = 0 THEN
		MessageBox("Atención", "No se han creado Distribución de Calibres~r" + &
						"para Especie y/0 Variedad indicadas.~r~rRegistre los antecedentes " + &
						"y vuelva a intentar registrar el control.")
	ELSE
		FOR ll_Filas = 1 TO ids_Base.RowCount()
			ll_Fila	=	dw_4.InsertRow(0)
			
			dw_4.Object.cand_grucal[ll_Fila]	=	ids_Base.Object.evdc_grucal[ll_Filas]
		NEXT
	END IF
END IF
end subroutine

public function boolean duplicadoenvase (string as_columna, string as_valor);Long		ll_Fila
String	ls_CodEmbalaje

ls_CodEmbalaje	=	String(dw_5.Object.emba_codigo[il_Fila])
//ls_CodEnvase	=	String(dw_5.Object.enva_codigo[il_Fila])

CHOOSE CASE as_Columna

	CASE "emba_codigo"
		ls_CodEmbalaje	=	as_Valor

END CHOOSE

ll_Fila	=	dw_5.Find("emba_codigo = '" + ls_CodEmbalaje + "'" ,1, dw_5.RowCount())

IF ll_Fila > 0 AND ll_Fila <> il_Fila THEN
	MessageBox("Error", "Registro ya fue ingresada anteriormente", Information!, Ok!)
	RETURN True
ELSE
	RETURN False
END IF
end function

public subroutine habilitacolumnas ();Integer	li_PresEcu, li_PresHom, li_PresApi, li_grupo, li_subgrupo

IF iuo_variedades.existe(Integer(istr_Mant.Argumento[3]),Integer(istr_Mant.Argumento[6]),&
                        TRUE, SQLCA) THEN
	li_grupo    =  iuo_variedades.grupo
	li_subgrupo	=	iuo_variedades.subgrupo
ELSE
	SetNull(li_grupo)
	SetNull(li_Subgrupo)
END IF	

IF NOT iuo_ParamMadurez.Existe(Integer(istr_Mant.Argumento[3]),li_grupo,li_subgrupo,&
											Integer(istr_Mant.Argumento[6]), &
											FALSE, Sqlca) THEN
	IF NOT iuo_ParamMadurez.Existe(Integer(istr_Mant.Argumento[3]),li_grupo,li_subgrupo,&
											-1, &
											FALSE, Sqlca) THEN
		IF NOT iuo_ParamMadurez.Existe(Integer(istr_Mant.Argumento[3]),li_grupo,-1,&
											-1, &
											FALSE, Sqlca) THEN
			IF NOT iuo_ParamMadurez.Existe(Integer(istr_Mant.Argumento[3]),-1,-1,&
											-1, &
											FALSE, Sqlca) THEN
			END IF
		END IF
	END IF
END IF	

IF iuo_ParamMadurez.PresEcuatorial	= 1 THEN
	li_PresEcu	=	0
ELSE
	li_PresEcu	=	1
END IF

IF iuo_ParamMadurez.PresHombros		= 1 THEN
	li_PresHom	=	0
ELSE
	li_PresHom	=	1
END IF

IF iuo_ParamMadurez.PresApice			= 1 THEN
	li_PresApi	=	0
ELSE
	li_PresApi	=	1
END IF

dw_4.Object.ccfi_fiecpr.Protect		=	li_PresEcu
dw_4.Object.ccfi_fiecmi.Protect		=	li_PresEcu
dw_4.Object.ccfi_fiecma.Protect		=	li_PresEcu
dw_4.Object.ccfi_fiecpr.Protect		=	li_PresEcu
dw_4.Object.ccfi_fiecmi.Protect		=	li_PresEcu
dw_4.Object.ccfi_fiecma.Protect		=	li_PresEcu
dw_4.Object.ccfi_fihopr.Protect		=	li_PresHom
dw_4.Object.ccfi_fihomi.Protect		=	li_PresHom
dw_4.Object.ccfi_fihoma.Protect		=	li_PresHom
dw_4.Object.ccfi_fiappr.Protect		=	li_PresApi
dw_4.Object.ccfi_fiapmi.Protect		=	li_PresApi
dw_4.Object.ccfi_fiapma.Protect		=	li_PresApi

RETURN
end subroutine

public subroutine buscaembalaje ();str_busqueda	lstr_busq
String			ls_Nula

SetNull(ls_Nula)

lstr_busq.argum[1]	=	String(dw_5.Object.enva_tipoen[il_Fila])

OpenWithParm(w_busc_embalajes, lstr_busq)

lstr_busq	= Message.PowerObjectParm

IF lstr_busq.argum[2] = "" THEN
	dw_5.SetColumn("emba_codigo")
	dw_5.Object.emba_codigo[il_Fila]	=	ls_Nula
	dw_5.Object.emba_nombre[il_Fila]	=	ls_Nula
	dw_5.SetFocus()
ELSE
	dw_5.Object.emba_codigo[il_Fila]	=	Integer(lstr_busq.argum[1])
	dw_5.Object.emba_nombre[il_Fila]	=	Integer(lstr_busq.argum[2])
	
	ExisteEmbalaje(dw_5.object.emba_codigo[il_fila],istr_Embalaje)
END IF

RETURN
end subroutine

public function boolean corresponderango (string as_grupocolumnas, string as_valor);Boolean		lb_Retorno = False
Integer		li_Especie, li_Variedad, li_grupo, li_subgrupo
Decimal{2}	ld_ValMinimo, ld_ValMaximo

li_Especie	=	Integer(istr_Mant.Argumento[3])
li_Variedad	=	Integer(istr_Mant.Argumento[6])

IF iuo_variedades.existe(li_especie,li_variedad,TRUE, SQLCA) THEN
	li_grupo    =  iuo_variedades.grupo
	li_subgrupo	=	iuo_variedades.subgrupo
ELSE
	SetNull(li_grupo)
	SetNull(li_Subgrupo)
END IF	

CHOOSE CASE as_GrupoColumnas

	CASE "Firmeza Ecuatorial"
		SELECT	IsNull(copam_permin,0), IsNull(copam_permax,0)
			INTO	:ld_ValMinimo, :ld_ValMaximo
			FROM	dba.spro_contparamadurez
			WHERE	espe_codigo	=	:li_Especie
			AND   isnull(grva_codigo,-1) =  :li_grupo
			AND   isnull(grva_codsub,-1) =  :li_subgrupo
			AND	isnull(vari_codigo,-1) =  :li_Variedad;
			
			IF ld_ValMinimo=0 AND ld_ValMaximo=0 THEN
				
			  SELECT	IsNull(copam_permin,0), IsNull(copam_permax,0)
					INTO	:ld_ValMinimo, :ld_ValMaximo
					FROM	dba.spro_contparamadurez
					WHERE	espe_codigo	=	:li_Especie
					AND   isnull(grva_codigo,-1) =  :li_grupo
					AND   isnull(grva_codsub,-1) =  :li_subgrupo
					AND	isnull(vari_codigo,-1) =  -1;
					
			ELSEIF ld_ValMinimo=0 AND ld_ValMaximo=0 THEN
				
					SELECT	IsNull(copam_permin,0), IsNull(copam_permax,0)
					INTO	:ld_ValMinimo, :ld_ValMaximo
					FROM	dba.spro_contparamadurez
					WHERE	espe_codigo	=	:li_Especie
					AND   isnull(grva_codigo,-1) =  :li_grupo
					AND   isnull(grva_codsub,-1) =  -1
					AND	isnull(vari_codigo,-1) =  -1;
			ELSE
					SELECT	IsNull(copam_permin,0), IsNull(copam_permax,0)
					INTO	:ld_ValMinimo, :ld_ValMaximo
					FROM	dba.spro_contparamadurez
					WHERE	espe_codigo	=	:li_Especie
					AND   isnull(grva_codigo,-1) =  -1
					AND   isnull(grva_codsub,-1) =  -1
					AND	isnull(vari_codigo,-1) =  -1;
			END IF		
					
	CASE "Firmeza Hombros"
		 SELECT	IsNull(copam_phrmin,0), IsNull(copam_phrmax,0)
			INTO	:ld_ValMinimo, :ld_ValMaximo
			FROM	dba.spro_contparamadurez
			WHERE	espe_codigo	=	:li_Especie
			AND   isnull(grva_codigo,-1) =  :li_grupo
			AND   isnull(grva_codsub,-1) =  :li_subgrupo
			AND	isnull(vari_codigo,-1) =  :li_Variedad;
			
			IF ld_ValMinimo=0 AND ld_ValMaximo=0 THEN
				
            SELECT	IsNull(copam_phrmin,0), IsNull(copam_phrmax,0)
					INTO	:ld_ValMinimo, :ld_ValMaximo
					FROM	dba.spro_contparamadurez
					WHERE	espe_codigo	=	:li_Especie
					AND   isnull(grva_codigo,-1) =  :li_grupo
					AND   isnull(grva_codsub,-1) =  :li_subgrupo
					AND	isnull(vari_codigo,-1) =  -1;
					
			ELSEIF ld_ValMinimo=0 AND ld_ValMaximo=0 THEN
				
					SELECT	IsNull(copam_phrmin,0), IsNull(copam_phrmax,0)
					INTO	:ld_ValMinimo, :ld_ValMaximo
					FROM	dba.spro_contparamadurez
					WHERE	espe_codigo	=	:li_Especie
					AND   isnull(grva_codigo,-1) =  :li_grupo
					AND   isnull(grva_codsub,-1) =  -1
					AND	isnull(vari_codigo,-1) =  -1;
			ELSE
					SELECT	IsNull(copam_phrmin,0), IsNull(copam_phrmax,0)
					INTO	:ld_ValMinimo, :ld_ValMaximo
					FROM	dba.spro_contparamadurez
					WHERE	espe_codigo	=	:li_Especie
					AND   isnull(grva_codigo,-1) =  -1
					AND   isnull(grva_codsub,-1) =  -1
					AND	isnull(vari_codigo,-1) =  -1;
			END IF		
			

	CASE "Firmeza Apice"
		
        SELECT	IsNull(copam_parmin,0), IsNull(copam_parmax,0)
			INTO	:ld_ValMinimo, :ld_ValMaximo
			FROM	dba.spro_contparamadurez
			WHERE	espe_codigo	=	:li_Especie
			AND   isnull(grva_codigo,-1) =  :li_grupo
			AND   isnull(grva_codsub,-1) =  :li_subgrupo
			AND	isnull(vari_codigo,-1) =  :li_Variedad;
			
			IF ld_ValMinimo=0 AND ld_ValMaximo=0 THEN
				
            SELECT	IsNull(copam_parmin,0), IsNull(copam_parmax,0)
					INTO	:ld_ValMinimo, :ld_ValMaximo
					FROM	dba.spro_contparamadurez
					WHERE	espe_codigo	=	:li_Especie
					AND   isnull(grva_codigo,-1) =  :li_grupo
					AND   isnull(grva_codsub,-1) =  :li_subgrupo
					AND	isnull(vari_codigo,-1) =  -1;
					
			ELSEIF ld_ValMinimo=0 AND ld_ValMaximo=0 THEN
				
					SELECT	IsNull(copam_parmin,0), IsNull(copam_parmax,0)
					INTO	:ld_ValMinimo, :ld_ValMaximo
					FROM	dba.spro_contparamadurez
					WHERE	espe_codigo	=	:li_Especie
					AND   isnull(grva_codigo,-1) =  :li_grupo
					AND   isnull(grva_codsub,-1) =  -1
					AND	isnull(vari_codigo,-1) =  -1;
			ELSE
					SELECT	IsNull(copam_parmin,0), IsNull(copam_parmax,0)
					INTO	:ld_ValMinimo, :ld_ValMaximo
					FROM	dba.spro_contparamadurez
					WHERE	espe_codigo	=	:li_Especie
					AND   isnull(grva_codigo,-1) =  -1
					AND   isnull(grva_codsub,-1) =  -1
					AND	isnull(vari_codigo,-1) =  -1;
			END IF		
END CHOOSE

IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla de Parámetros de Madurez Cosecha")
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "No se ha creado Parámetros de Madurez Cosecha ~r" + &
					"para Especie y Variedad indicadas.~r~rIngrese o seleccione " + &
					"otra Variedad.")
ELSEIF Dec(as_Valor) <> 0 AND Dec(as_Valor) < ld_ValMinimo OR Dec(as_Valor) > ld_ValMaximo THEN
		MessageBox("Atención", "Valor registrado no corresponde al definido ~r" + &
						"en Parámetros de Madurez Cosecha " + &
						"para Especie y Variedad indicadas.~r~rIngrese otro Valor.")
ELSE
	lb_Retorno	=	True
END IF

RETURN lb_Retorno
end function

public subroutine habilitaingreso (string as_columna);Boolean	lb_Estado = True
Date  	ldt_Fecha

IF as_Columna <> "ccap_feccon" AND &
	(dw_2.Object.ccap_feccon[1] = ldt_Fecha OR IsNull(dw_2.Object.ccap_feccon[1])) THEN
	lb_Estado = False
END IF

IF as_Columna <> "vari_codrot" AND &
	(dw_2.Object.vari_codrot[1] = 0 OR IsNull(dw_2.Object.vari_codrot[1])) THEN
	lb_Estado = False
END IF

IF as_Columna <> "line_codigo" AND &
	(dw_2.Object.line_codigo[1] = 0 OR IsNull(dw_2.Object.line_codigo[1])) THEN
	lb_Estado = False
END IF

IF as_Columna <> "ccap_turno" AND &
	(dw_2.Object.ccap_turno[1] = 0 OR IsNull(dw_2.Object.ccap_turno[1])) THEN
	lb_Estado = False
END IF

IF as_Columna <> "ccap_clapal" AND &
	(dw_2.Object.ccap_clapal[1] = 0 OR IsNull(dw_2.Object.ccap_clapal[1])) THEN
	lb_Estado = False
END IF

tab_1.tp_1.Enabled	=	lb_Estado
tab_1.tp_2.Enabled	=	lb_Estado
tab_1.tp_3.Enabled	=	lb_Estado
pb_ins_det.Enabled	=	lb_Estado
end subroutine

public subroutine buscaorden ();Str_Busqueda	lstr_busq
Date  			ldt_Fecha
Long				ll_nrofila

lstr_busq.argum[1] =	String(gstr_ParamPlanta.CodigoPlanta)
lstr_busq.argum[2] =	"0"
lstr_busq.argum[3] =	""

OpenWithParm(w_busc_spro_ordenproceso, lstr_busq)

lstr_busq	=	Message.PowerObjectParm

IF lstr_busq.argum[3] <> "" THEN
	istr_Mant.Argumento[3]	=	lstr_Busq.Argum[4]
	istr_Mant.Argumento[4]	=	lstr_Busq.Argum[10]
	istr_Mant.Argumento[5]	=	lstr_Busq.Argum[6]
	istr_Mant.Argumento[6]	=	lstr_Busq.Argum[8]
	ldt_Fecha					=	Date(lstr_Busq.Argum[5])

	dw_2.Object.ccap_tipord[1]		=	Integer(lstr_Busq.Argum[10])
	dw_2.Object.ccap_nrodoc[1]		=	Long(lstr_Busq.Argum[6])
	dw_2.Object.ccap_fecmov[1]		=	ldt_Fecha
	dw_2.Object.espe_codigo[1]		=	Integer(lstr_Busq.Argum[4])
	dw_2.Object.prod_codigo[1]		=	Long(lstr_Busq.Argum[7])
	dw_2.Object.vari_codigo[1]		=	Integer(lstr_Busq.Argum[8])
	dw_2.Object.line_codigo[1]		=	Integer(lstr_Busq.Argum[12])
	dw_2.Object.ccap_turno[1]		=	Integer(lstr_Busq.Argum[13])
	dw_2.Object.tipo_frio[1]		=	lstr_Busq.Argum[14]
	dw_2.Object.periodo_frio[1]	=	Integer(lstr_Busq.Argum[15])

	dw_2.Object.vari_codrot.Protect				=	0
	dw_2.Object.vari_codrot.BackGround.Color	=	RGB(255,255,255)

	dw_2.GetChild("vari_codrot", idwc_varirot)
	idwc_varirot.SetTransObject(sqlca)
	idwc_varirot.Retrieve(Integer(istr_Mant.Argumento[3]))

	ll_nrofila = dw_6.Retrieve(Integer(istr_Mant.Argumento[4]),Integer(istr_Mant.Argumento[5]))
	
	IF ll_nrofila <> 0 THEN
		dw_2.Object.ccap_ttbulp[1]		=	Long(dw_6.Object.total_bultos[1])
		GeneraDetalle()
		HabilitaColumnas()
	ELSE
		MessageBox("Atención", "No Existe lote Despachado Para Esta Orden",Exclamation!)
		RETURN	
END IF	
END IF
end subroutine

event open;/* 
Argumentos
istr_Mant.Argumento[1]	=	Código Planta
istr_Mant.Argumento[2]	=	Numero de Folio
istr_Mant.Argumento[3]	=	Código Especie
istr_Mant.Argumento[4]	=	Tipo de Orden
istr_Mant.Argumento[5]	=	Numero de Orden
istr_Mant.Argumento[6]	=	Código Variedad Real
istr_Mant.Argumento[7]	=	Código Variedad Rotulada
*/

x				= 0
y				= 0

This.Height	= 2020
im_menu		= m_principal

This.ParentWindow().ToolBarVisible	= True
im_menu.Item[1].Item[6].Enabled		= True
im_menu.Item[7].Visible					= True

dw_3	=	tab_1.tp_1.dw_calidades
dw_4	=	tab_1.tp_2.dw_analfirm
dw_5	=	tab_1.tp_3.dw_analpeso

istr_Mant.Argumento[1]	=	String(gstr_ParamPlanta.CodigoPlanta)
istr_Mant.Argumento[2]	=	""

dw_2.GetChild("plde_codigo", idwc_planta)
idwc_planta.SetTransObject(sqlca)
idwc_planta.Retrieve(gi_codexport)

dw_2.GetChild("espe_codigo", idwc_especie)
idwc_especie.SetTransObject(sqlca)
idwc_especie.Retrieve(gstr_parempresa.empr_codexp)

dw_2.GetChild("prod_codigo", idwc_productor)
idwc_productor.SetTransObject(sqlca)
idwc_productor.Retrieve()

dw_2.GetChild("vari_codigo", idwc_varireal)
idwc_varireal.SetTransObject(sqlca)
idwc_varireal.Retrieve(0,gstr_parempresa.empr_codexp)

dw_2.GetChild("vari_codrot", idwc_varirot)
idwc_varirot.SetTransObject(sqlca)
idwc_varirot.Retrieve(0)

dw_2.GetChild("line_codigo", idwc_LineaPacking)
idwc_LineaPacking.SetTransObject(sqlca)
idwc_LineaPacking.Retrieve(gstr_ParamPlanta.CodigoPlanta)

dw_2.GetChild("tipo_frio", idwc_tratamiento)
idwc_tratamiento.SetTransObject(sqlca)
idwc_tratamiento.Retrieve()

dw_2.GetChild("tipo_frio", idwc_periodo)
idwc_periodo.SetTransObject(sqlca)
idwc_periodo.Retrieve()

dw_3.GetChild("cate_codigo", idwc_categoria)
idwc_categoria.SetTransObject(SqlCa)
idwc_categoria.Retrieve()

dw_3.GetChild("emba_codigo",idwc_embalaje)
idwc_embalaje.SetTransObject(SQLCA)
idwc_embalaje.Retrieve(0,0)

dw_5.GetChild("emba_codigo",idwc_embalaje)
idwc_embalaje.SetTransObject(SQLCA)
idwc_embalaje.Retrieve(0,0)



//dw_5.GetChild("enva_tipoen", idwc_tipoenv)
//idwc_tipoenv.SetTransObject(SqlCa)
//idwc_tipoenv.Retrieve()
//
//dw_5.GetChild("enva_codigo", idwc_envase1)
//idwc_envase1.SetTransObject(SqlCa)
//idwc_envase1.Retrieve(0)

dw_7.GetChild("ccpd_calibre", idwc_calibre)
idwc_calibre.SetTransObject(SqlCa)
idwc_calibre.Retrieve(0,0,0)

dw_7.GetChild("dade_codigo",idwc_danos)
idwc_danos.SetTransObject(SQLCA)
idwc_danos.Retrieve(0,0)

dw_1.SetTransObject(sqlca)
dw_2.SetTransObject(sqlca)
dw_3.SetTransObject(sqlca)
dw_4.SetTransObject(sqlca)
dw_5.SetTransObject(sqlca)
dw_6.SetTransObject(sqlca)
dw_7.SetTransObject(sqlca)

dw_3.Modify("datawindow.message.title='Error '+ is_titulo")
dw_3.SetRowFocusIndicator(Hand!)
dw_3.Modify("DataWindow.Footer.Height = 88")

dw_4.Modify("datawindow.message.title='Error '+ is_titulo")
dw_4.SetRowFocusIndicator(Hand!)
dw_4.Modify("DataWindow.Footer.Height = 88")

istr_Mant.dw				=	dw_1
istr_Mant.solo_consulta =	False

pb_nuevo.PostEvent(Clicked!)

iuo_PltaDestino	=	Create uo_plantadesp
iuo_LineaPacking	=	Create uo_lineapacking
iuo_Especie			=	Create uo_Especie
iuo_variedades		=	Create uo_variedades
iuo_ordenproceso	=	Create uo_spro_ordenproceso
iuo_parammadurez	=	Create uo_parammadurez

ids_Base				=	Create DataStore

dw_2.Object.vari_codrot.Protect				=	1
dw_2.Object.vari_codrot.BackGround.Color	=	RGB(192,192,192)

end event

event ue_borra_detalle();call super::ue_borra_detalle;Integer	li_tabpage, li_borra

li_tabpage	=	tab_1.SelectedTab

SetPointer(HourGlass!)

ib_borrar = True
w_main.SetMicroHelp("Validando la eliminación de detalle...")

Message.DoubleParm = 0

This.TriggerEvent ("ue_validaborrar_detalle")

IF Message.DoubleParm = -1 THEN RETURN

istr_mant.borra	= True
istr_mant.agrega	= False

CHOOSE CASE li_tabpage
	CASE 1
		istr_mant.dw	=	dw_3
		istr_mant.dw2	=	dw_7
		OpenWithParm(iw_mantencion_1, istr_mant)

		istr_mant = Message.PowerObjectParm
END CHOOSE

IF istr_mant.respuesta = 1 THEN
	CHOOSE CASE li_tabpage
		CASE 1
			li_borra	=	dw_3.DeleteRow(0)
			IF li_Borra = 1 THEN
				li_Borra = dw_7.RowsMove(1,dw_7.RowCount(),Primary!,dw_7,1,Delete!)
			END IF
	END CHOOSE
 
	IF li_borra = 1 THEN
		dw_7.SetFilter("")
		dw_7.Filter()
		
		ib_borrar = False
		w_main.SetMicroHelp("Borrando Registro...")
		SetPointer(Arrow!)
//	ELSE
//		ib_borrar = False
//		MessageBox(This.Title,"No se puede borrar actual registro.")
	END IF

	IF dw_3.RowCount() = 0 THEN pb_eli_det.Enabled = False
END IF

IF li_tabpage = 3 THEN
	IF MessageBox("Eliminación de Registro", "Está seguro de Eliminar este Registro", Question!, YesNo!) = 1 THEN
		IF dw_5.DeleteRow(0) = 1 THEN
			ib_borrar = False
			w_main.SetMicroHelp("Borrando Registro...")
			SetPointer(Arrow!)
		ELSE
			ib_borrar = False
			MessageBox(This.Title,"No se puede borrar actual registro.")
		END IF
	
	 	IF dw_5.RowCount() = 0 THEN
			pb_eliminar.Enabled = False
		ELSE
			il_fila = dw_5.GetRow()
		END IF
	END IF
END IF

istr_mant.borra	 = False
end event

event ue_nuevo_detalle();IF tab_1.SelectedTab = 3 THEN
	dw_5.SetColumn("emba_codigo")

	IF il_fila > 0 THEN
		pb_eliminar.Enabled	= True
		pb_grabar.Enabled		= True
	END IF

	il_fila = dw_5.InsertRow(0)

	dw_5.ScrollToRow(il_fila)
	dw_5.SetRow(il_fila)
	dw_5.SetFocus()
	dw_5.SetColumn(1)
ELSEIF tab_1.SelectedTab = 1 THEN
	IF dw_3.RowCount() > 10 THEN
		MessageBox("Atención","El número máximo de Registros que debe ingresar es 10.")
	ELSE
		istr_mant.Borra	=	False
		istr_mant.Agrega	=	True
		
		istr_mant.dw	=	dw_3
		istr_mant.dw2	=	dw_7
		OpenWithParm(iw_mantencion_1, istr_mant)
	END IF
END IF

IF dw_5.RowCount() > 0 OR dw_7.RowCount() > 0 THEN HabilitaEncab(False)

IF dw_5.RowCount() > 0 OR dw_7.RowCount() > 0 THEN
	pb_eliminar.Enabled	=	True
	pb_grabar.Enabled		=	True
END IF

dw_7.SetFilter("")
dw_7.Filter()

end event

event ue_recuperadatos();Long		ll_fila_d, ll_fila_e, respuesta

DO
	dw_2.SetRedraw(False)

	ll_fila_e	=	dw_2.Retrieve(Integer(istr_Mant.Argumento[1]), &
										  Integer(istr_Mant.Argumento[2]))
										  
	IF ll_fila_e = -1 THEN
		respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
										Information!, RetryCancel!)
	ELSE
		istr_Mant.Argumento[1]	=	String(dw_2.Object.plde_codigo[1])
		istr_Mant.Argumento[3]	=	String(dw_2.Object.espe_codigo[1])
		istr_Mant.Argumento[4]	=	String(dw_2.Object.ccap_tipord[1])
		istr_Mant.Argumento[5]	=	String(dw_2.Object.ccap_nrodoc[1])
		istr_Mant.Argumento[6]	=	String(dw_2.Object.vari_codigo[1])
		istr_Mant.Argumento[7]	=	String(dw_2.Object.vari_codrot[1])

		dw_6.Retrieve(Integer(istr_Mant.Argumento[4]), Integer(istr_Mant.Argumento[5]))

		iuo_OrdenProceso.Existe(Integer(istr_Mant.Argumento[1]), &
										Integer(istr_Mant.Argumento[4]), &
										Integer(istr_Mant.Argumento[5]), &
										False, SqlCa, gi_CodExport)
		
		dw_2.Object.ccap_fecmov[1]		=	iuo_OrdenProceso.FechaOrden
		dw_2.Object.tipo_frio[1]		=	iuo_OrdenProceso.TipoFrio
		dw_2.Object.periodo_frio[1]	=	iuo_OrdenProceso.PeriodoFrio

		DO
			IF dw_3.Retrieve(Integer(istr_Mant.Argumento[1]), &
								  Integer(istr_Mant.Argumento[2])) = -1 OR &
				dw_4.Retrieve(Integer(istr_Mant.Argumento[1]), &
								  Integer(istr_Mant.Argumento[2])) = -1 OR &
				dw_5.Retrieve(Integer(istr_Mant.Argumento[1]), &
								  Integer(istr_Mant.Argumento[2])) = -1  OR &
				dw_7.Retrieve(Integer(istr_Mant.Argumento[1]), &
								  Integer(istr_Mant.Argumento[2])) = -1 THEN
				respuesta = MessageBox(	"Error en Base de Datos", &
												"No es posible conectar la Base de Datos.", &
												Information!, RetryCancel!)
			ELSE
				pb_eliminar.Enabled	=	True
				pb_grabar.Enabled		=	True
				pb_imprimir.Enabled	=	True
				pb_ins_det.Enabled	=	True
				tab_1.tp_1.Enabled	=	True
				tab_1.tp_2.Enabled	=	True
				tab_1.tp_3.Enabled	=	True

				HabilitaEncab(False)
				HabilitaColumnas()

				pb_eli_det.Enabled	=	True
				
				dw_1.SetRow(1)
				dw_1.SelectRow(1,True)
				dw_1.SetFocus()
			END IF
			
			LOOP WHILE respuesta = 1

		IF respuesta = 2 THEN Close(This)
	END IF
	dw_2.SetRedraw(True)
LOOP WHILE respuesta = 1

IF respuesta = 2 THEN Close(This)
end event

on w_maed_spro_ccalidadpackenca.create
int iCurrent
call super::create
this.tab_1=create tab_1
this.dw_6=create dw_6
this.dw_7=create dw_7
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.tab_1
this.Control[iCurrent+2]=this.dw_6
this.Control[iCurrent+3]=this.dw_7
end on

on w_maed_spro_ccalidadpackenca.destroy
call super::destroy
destroy(this.tab_1)
destroy(this.dw_6)
destroy(this.dw_7)
end on

event ue_nuevo();Call Super::ue_nuevo

dw_3.Reset()
dw_4.Reset()
dw_5.Reset()
dw_6.Reset()
dw_7.Reset()


dw_2.Object.plde_codigo[1]	=	gstr_ParamPlanta.CodigoPlanta

idt_FechaSistema	=	F_FechaHora()
dw_2.Object.ccap_feccon[1]	=	Date(idt_FechaSistema)

dw_2.Object.vari_codrot.Protect				=	1
dw_2.Object.vari_codrot.BackGround.Color	=	RGB(192,192,192)


dw_2.SetColumn("ccap_folio")
dw_2.SetFocus()
end event

event ue_antesguardar();call super::ue_antesguardar;Long		ll_Fila, ll_Numero
Integer	li_Planta

li_Planta	=	dw_2.Object.plde_codigo[1]

Message.DoubleParm = 0

ib_AutoCommit		=	SQLCA.AutoCommit
SQLCA.AutoCommit	=	False

IF dw_2.GetItemStatus(1, 0, Primary!) = NewModified! THEN
	UPDATE	dba.spro_ccalidadpackenca
		SET	ccap_folio= 0
		WHERE	1 = 2;
	
	SELECT	IsNull(Max(ccap_folio), 0) + 1
		INTO	:ll_Numero
		FROM	dba.spro_ccalidadpackenca
		WHERE	plde_codigo	=	:li_Planta ;

	dw_2.Object.ccap_folio[1]	=	ll_Numero
ELSE
	ll_Numero	=	dw_2.Object.ccap_folio[1]
END IF

istr_Mant.Argumento[2]	=	String(ll_Numero)

FOR ll_Fila = 1 TO dw_3.RowCount()
	IF dw_3.GetItemStatus(ll_Fila, 0, Primary!) = NewModified! THEN
		dw_3.Object.plde_codigo[ll_Fila]	=	dw_2.Object.plde_codigo[1]
		dw_3.Object.ccap_folio[ll_Fila]	=	dw_2.Object.ccap_folio[1]
	END IF
NEXT

FOR ll_Fila = 1 TO dw_4.RowCount()
	IF dw_4.GetItemStatus(ll_Fila, 0, Primary!) = NewModified! THEN
		dw_4.Object.plde_codigo[ll_Fila]	=	dw_2.Object.plde_codigo[1]
		dw_4.Object.ccap_folio[ll_Fila]	=	dw_2.Object.ccap_folio[1]
	END IF
NEXT

FOR ll_Fila = 1 TO dw_5.RowCount()
	IF dw_5.GetItemStatus(ll_Fila, 0, Primary!) = NewModified! THEN
		dw_5.Object.plde_codigo[ll_Fila]	=	dw_2.Object.plde_codigo[1]
		dw_5.Object.ccap_folio[ll_Fila]	=	dw_2.Object.ccap_folio[1]
	END IF
NEXT

FOR ll_Fila = 1 TO dw_7.RowCount()
	IF dw_7.GetItemStatus(ll_Fila, 0, Primary!) = NewModified! THEN
		dw_7.Object.plde_codigo[ll_Fila]	=	dw_2.Object.plde_codigo[1]
		dw_7.Object.ccap_folio[ll_Fila]	=	dw_2.Object.ccap_folio[1]
		dw_7.Object.espe_codigo[ll_Fila]	=	Integer(istr_Mant.Argumento[3])
	END IF
NEXT

DO WHILE ll_Fila <= dw_3.RowCount()
	IF dw_3.GetItemStatus(ll_Fila, 0, Primary!) = New! THEN
		dw_3.DeleteRow(ll_Fila)
	ELSE
		ll_Fila ++
	END IF
LOOP

DO WHILE ll_Fila <= dw_4.RowCount()
	IF dw_4.GetItemStatus(ll_Fila, 0, Primary!) = New! THEN
		dw_4.DeleteRow(ll_Fila)
	ELSE
		ll_Fila ++
	END IF
LOOP

DO WHILE ll_Fila <= dw_5.RowCount()
	IF dw_5.GetItemStatus(ll_Fila, 0, Primary!) = New! THEN
		dw_5.DeleteRow(ll_Fila)
	ELSE
		ll_Fila ++
	END IF
LOOP

end event

event ue_borrar();IF dw_2.RowCount() < 1 THEN RETURN

SetPointer(HourGlass!)

ib_borrar		=	True
ib_AutoCommit	=	sqlca.AutoCommit

w_main.SetMicroHelp("Validando la eliminación...")

Message.DoubleParm = 0

This.TriggerEvent ("ue_validaborrar")

IF Message.DoubleParm = -1 THEN RETURN

IF dw_3.RowCount() > 0 THEN
	dw_3.RowsMove(1, dw_3.RowCount(), Primary!, dw_3, 1, Delete!)
END IF

IF dw_4.RowCount() > 0 THEN
	dw_4.RowsMove(1, dw_4.RowCount(), Primary!, dw_4, 1, Delete!)
END IF

IF dw_5.RowCount() > 0 THEN
	dw_5.RowsMove(1, dw_5.RowCount(), Primary!, dw_5, 1, Delete!)
END IF

IF dw_7.RowCount() > 0 THEN
	dw_7.RowsMove(1, dw_7.RowCount(), Primary!, dw_7, 1, Delete!)
END IF

IF dw_2.DeleteRow(0) = 1 THEN
		ib_Borrar	=	False
		
		w_main.SetMicroHelp("Borrando Registro...")
		
		IF wf_actualiza_db(True) THEN
			w_main.SetMicroHelp("Registro Borrado...")
			This.TriggerEvent("ue_nuevo")
			SetPointer(Arrow!)
		ELSE
			w_main.SetMicroHelp("Registro no Borrado...")
		END IF			
ELSE
	ib_Borrar	=	False
	
	MessageBox(This.Title,"No se puede borrar actual registro.")
END IF

HabilitaEncab(True)
end event

event resize;tab_1.x			=	dw_1.x
tab_1.y			=	dw_1.y
tab_1.Height	=	dw_1.Height

end event

event ue_seleccion();call super::ue_seleccion;Str_Busqueda	lstr_busq

lstr_Busq.Argum[1]	=	istr_Mant.Argumento[1]

OpenWithParm(w_busc_spro_ccalidadpackenca, lstr_busq)

lstr_busq	=	Message.PowerObjectParm

IF lstr_Busq.Argum[2] <> "" THEN
	istr_Mant.Argumento[2]	=	lstr_Busq.Argum[2]
	istr_Mant.Argumento[3]	=	lstr_Busq.Argum[5]
	istr_Mant.Argumento[4]	=	lstr_Busq.Argum[3]
	istr_Mant.Argumento[5]	=	lstr_Busq.Argum[4]
	istr_Mant.Argumento[6]	=	lstr_Busq.Argum[6]

	This.TriggerEvent("ue_recuperadatos")
END IF

end event

event ue_validaborrar();//
end event

event closequery;IF Not istr_mant.Solo_Consulta THEN
	CHOOSE CASE wf_modifica()
		CASE -1
			Message.ReturnValue = 1 
		CASE 0
			IF dw_3.RowCount() > 0 OR dw_4.RowCount() > 0 OR dw_5.RowCount() > 0 THEN
				CHOOSE CASE MessageBox("Grabar registro(s)","Desea Grabar la información ?", Question!, YesNoCancel!)
					CASE 1
						Message.DoubleParm = 0
						This.triggerevent("ue_guardar")
						IF message.doubleparm = -1 THEN Message.ReturnValue = 1
						RETURN
					CASE 3
						Message.ReturnValue = 1
						RETURN
				END CHOOSE
			END IF
	END CHOOSE
END IF
end event

event ue_modifica_detalle();call super::ue_modifica_detalle;Integer	li_tabpage
Boolean	lb_estado

li_tabpage	=	tab_1.SelectedTab

istr_mant.agrega	= False
istr_mant.borra	= False

lb_estado			=	istr_mant.Solo_Consulta

CHOOSE CASE li_tabpage
	CASE 1
		IF dw_3.RowCount() > 0 THEN
			istr_mant.dw	=	dw_3
			istr_mant.dw2	=	dw_7
			OpenWithParm(iw_mantencion_1, istr_mant)
		END IF
		istr_mant.Solo_Consulta	=	lb_estado

END CHOOSE

dw_7.SetFilter("")
dw_7.Filter()

end event

event ue_imprimir();SetPointer(HourGlass!)

Long		fila

istr_info.titulo	= "REVISION FRUTA EMBALADA"
istr_info.copias	= 1

OpenWithParm(vinf,istr_info)

vinf.dw_1.DataObject = "dw_info_revision_fruta_embalada"

vinf.dw_1.SetTransObject(sqlca)

fila = vinf.dw_1.Retrieve(Integer(istr_Mant.Argumento[1]), &
								  Integer(istr_Mant.Argumento[2]))

IF fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", &
					StopSign!, Ok!)
ELSE
	F_Membrete(vinf.dw_1)
	vinf.dw_1.Modify('DataWindow.Print.Preview = Yes')
	vinf.dw_1.Modify('DataWindow.Print.Preview.Zoom = 75')

	vinf.Visible	= True
	vinf.Enabled	= True
END IF

SetPointer(Arrow!)
end event

type dw_1 from w_mant_encab_deta_csd`dw_1 within w_maed_spro_ccalidadpackenca
boolean visible = false
integer x = 37
integer y = 808
integer width = 3214
integer height = 1100
boolean enabled = false
string title = ""
end type

type dw_2 from w_mant_encab_deta_csd`dw_2 within w_maed_spro_ccalidadpackenca
integer x = 37
integer y = 36
integer width = 2679
integer height = 736
integer taborder = 10
string dataobject = "dw_mant_spro_ccalidadpackenca"
end type

event dw_2::itemchanged;String	ls_Nula
String	ls_Columna

ls_Columna	=	dwo.Name
SetNull(ls_Nula)

CHOOSE CASE ls_Columna

	CASE "ccap_folio"
		IF NOT ExisteFolio(gstr_ParamPlanta.CodigoPlanta,Long(data)) THEN
			This.SetItem(1,ls_Columna, Long(ls_Nula))
			This.SetFocus()
			RETURN 1
		END IF

	CASE "line_codigo"
		IF Not iuo_LineaPacking.Existe(gstr_ParamPlanta.CodigoPlanta, Integer(Data), True, sqlca) THEN
			This.SetItem(1, ls_Columna, Integer(ls_Nula))
			RETURN 1
		END IF

	CASE "vari_codrot"
		IF Not iuo_Variedades.Existe(Integer(istr_Mant.Argumento[3]),Integer(Data), True, sqlca) THEN
			This.SetItem(1, ls_Columna, Integer(ls_Nula))
			RETURN 1
		ELSE
			istr_Mant.Argumento[7]	=	Data
		END IF

END CHOOSE

HabilitaIngreso(ls_Columna)
end event

event dw_2::doubleclicked;//
end event

event dw_2::buttonclicked;call super::buttonclicked;CHOOSE CASE dwo.Name

	CASE "b_orden"
		BuscaOrden()

END CHOOSE
end event

type pb_nuevo from w_mant_encab_deta_csd`pb_nuevo within w_maed_spro_ccalidadpackenca
integer x = 3346
integer y = 256
integer taborder = 60
end type

event pb_nuevo::clicked;call super::clicked;HabilitaEncab(TRUE)
end event

type pb_eliminar from w_mant_encab_deta_csd`pb_eliminar within w_maed_spro_ccalidadpackenca
integer x = 3346
integer y = 436
integer taborder = 80
end type

type pb_grabar from w_mant_encab_deta_csd`pb_grabar within w_maed_spro_ccalidadpackenca
integer x = 3346
integer y = 616
integer taborder = 90
end type

type pb_imprimir from w_mant_encab_deta_csd`pb_imprimir within w_maed_spro_ccalidadpackenca
integer x = 3346
integer y = 796
integer taborder = 100
end type

type pb_salir from w_mant_encab_deta_csd`pb_salir within w_maed_spro_ccalidadpackenca
integer x = 3346
integer y = 976
integer taborder = 110
end type

type pb_ins_det from w_mant_encab_deta_csd`pb_ins_det within w_maed_spro_ccalidadpackenca
integer x = 3346
integer y = 1436
integer taborder = 30
end type

type pb_eli_det from w_mant_encab_deta_csd`pb_eli_det within w_maed_spro_ccalidadpackenca
integer x = 3346
integer y = 1612
integer taborder = 40
end type

type pb_buscar from w_mant_encab_deta_csd`pb_buscar within w_maed_spro_ccalidadpackenca
integer x = 3346
integer y = 76
integer taborder = 50
end type

type tab_1 from tab within w_maed_spro_ccalidadpackenca
event create ( )
event destroy ( )
integer x = 37
integer y = 812
integer width = 3218
integer height = 1072
integer taborder = 20
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
boolean fixedwidth = true
boolean raggedright = true
boolean focusonbuttondown = true
integer selectedtab = 1
tp_1 tp_1
tp_2 tp_2
tp_3 tp_3
end type

on tab_1.create
this.tp_1=create tp_1
this.tp_2=create tp_2
this.tp_3=create tp_3
this.Control[]={this.tp_1,&
this.tp_2,&
this.tp_3}
end on

on tab_1.destroy
destroy(this.tp_1)
destroy(this.tp_2)
destroy(this.tp_3)
end on

event selectionchanged;IF NewIndex = 3 THEN
	IF dw_5.RowCount() > 0 THEN
		pb_eli_det.Enabled	=	True
		pb_ins_det.Enabled	=	True
		il_Fila 					=	1
		
	ELSE
		pb_eli_det.Enabled	=	False
		pb_ins_det.Enabled	=	True
	END IF
ELSEIF NewIndex = 1 THEN
	IF dw_3.RowCount() > 0 THEN
		pb_eli_det.Enabled	=	True
		pb_ins_det.Enabled	=	True
		il_Fila 					=	1
		
	ELSE
		pb_eli_det.Enabled	=	False
		pb_ins_det.Enabled	=	True
	END IF
ELSE
	pb_eli_det.Enabled	=	False
	pb_ins_det.Enabled	=	False
	il_Fila 					=	1
END IF
end event

type tp_1 from userobject within tab_1
event create ( )
event destroy ( )
integer x = 18
integer y = 112
integer width = 3182
integer height = 944
boolean enabled = false
long backcolor = 12632256
string text = "Selección de Muestras"
long tabtextcolor = 33554432
long tabbackcolor = 12632256
string picturename = "Table!"
long picturemaskcolor = 553648127
dw_calidades dw_calidades
end type

on tp_1.create
this.dw_calidades=create dw_calidades
this.Control[]={this.dw_calidades}
end on

on tp_1.destroy
destroy(this.dw_calidades)
end on

type dw_calidades from uo_dw within tp_1
integer x = 37
integer y = 36
integer width = 3113
integer height = 868
integer taborder = 11
string dataobject = "dw_mues_spro_ccalidadpackdetcate"
boolean hscrollbar = true
boolean hsplitscroll = true
borderstyle borderstyle = stylelowered!
end type

event clicked;call super::clicked;IF Row > 0 THEN
	This.SelectRow(0,False)
	This.SetRow(row)
	This.SelectRow(row,True)
END IF
end event

event itemfocuschanged;call super::itemfocuschanged;IF IsValid(w_main) THEN
	w_main.SetMicroHelp(dwo.Tag)
END IF
end event

event losefocus;call super::losefocus;AcceptText()

end event

event rowfocuschanged;call super::rowfocuschanged;ib_datos_ok = True

IF This.RowCount() < 1 OR CurrentRow = 0 OR ib_borrar THEN
	ib_datos_ok = False
ELSE
	il_fila = CurrentRow
END IF
end event

event type long ue_seteafila(unsignedlong wparam, long lparam);call super::ue_seteafila;il_fila	= This.GetRow()

RETURN 0
end event

event itemerror;call super::itemerror;RETURN 1
end event

event doubleclicked;call super::doubleclicked;w_maed_spro_ccalidadpackenca.TriggerEvent("ue_modifica_detalle")
end event

type tp_2 from userobject within tab_1
event create ( )
event destroy ( )
integer x = 18
integer y = 112
integer width = 3182
integer height = 944
boolean enabled = false
long backcolor = 12632256
string text = "Análisis de Firmeza"
long tabtextcolor = 33554432
long tabbackcolor = 12632256
string picturename = "ArrangeTables!"
long picturemaskcolor = 553648127
dw_analfirm dw_analfirm
end type

on tp_2.create
this.dw_analfirm=create dw_analfirm
this.Control[]={this.dw_analfirm}
end on

on tp_2.destroy
destroy(this.dw_analfirm)
end on

type dw_analfirm from uo_dw within tp_2
integer x = 37
integer y = 36
integer width = 3113
integer height = 868
integer taborder = 10
string dataobject = "dw_mues_spro_ccalidadpackanafir"
boolean hscrollbar = true
boolean hsplitscroll = true
borderstyle borderstyle = stylelowered!
end type

event clicked;call super::clicked;IF Row > 0 THEN
	il_fila = Row
END IF

RETURN 0
end event

event itemfocuschanged;call super::itemfocuschanged;IF IsValid(w_main) THEN
	w_main.SetMicroHelp(dwo.Tag)
END IF
end event

event losefocus;call super::losefocus;AcceptText()
end event

event rowfocuschanged;call super::rowfocuschanged;ib_datos_ok = True

IF This.RowCount() < 1 OR CurrentRow = 0 OR ib_borrar THEN
	ib_datos_ok = False
ELSE
	il_fila = CurrentRow
END IF
end event

event type long ue_seteafila(unsignedlong wparam, long lparam);call super::ue_seteafila;il_fila	= This.GetRow()

RETURN 0
end event

event itemerror;call super::itemerror;RETURN 1
end event

event itemchanged;call super::itemchanged;String	ls_Columna, ls_Null

SetNull(ls_Null)

ls_Columna	=	dwo.Name

CHOOSE CASE ls_Columna

	CASE "ccfi_fiecpr", "ccfi_fiecma", "ccfi_fiecmi"
		IF Not CorrespondeRango("Firmeza Ecuatorial", Data) THEN
			This.SetItem(il_fila, ls_Columna, Dec(ls_Null))
			RETURN 1
		END IF
			
	CASE "ccfi_fihopr", "ccfi_fihoma", "ccfi_fihomi"
		IF Not CorrespondeRango("Firmeza Hombros", Data) THEN
			This.SetItem(il_fila, ls_Columna, Dec(ls_Null))
			RETURN 1
		END IF
			
	CASE "ccfi_fiappr", "ccfi_fiapma", "ccfi_fiapmi"
		IF Not CorrespondeRango("Firmeza Apice", Data) THEN
			This.SetItem(il_fila, ls_Columna, Dec(ls_Null))
			RETURN 1
		END IF

END CHOOSE
end event

type tp_3 from userobject within tab_1
integer x = 18
integer y = 112
integer width = 3182
integer height = 944
boolean enabled = false
long backcolor = 12632256
string text = "Análisis de Peso"
long tabtextcolor = 33554432
long tabbackcolor = 12632256
string picturename = "DataWindow5!"
long picturemaskcolor = 536870912
dw_analpeso dw_analpeso
end type

on tp_3.create
this.dw_analpeso=create dw_analpeso
this.Control[]={this.dw_analpeso}
end on

on tp_3.destroy
destroy(this.dw_analpeso)
end on

type dw_analpeso from uo_dw within tp_3
integer x = 37
integer y = 36
integer width = 3113
integer height = 868
integer taborder = 80
string dataobject = "dw_mues_spro_ccalidadpackanapeso"
boolean hscrollbar = true
boolean hsplitscroll = true
borderstyle borderstyle = stylelowered!
end type

event clicked;call super::clicked;IF Row > 0 THEN
	il_fila = Row
END IF

RETURN 0
end event

event itemchanged;String  ls_Columna, ls_Nula

SetNull(ls_Nula)

ls_Columna = dwo.Name

CHOOSE CASE ls_Columna

	CASE "emba_codigo"
		IF NOT ExisteEmbalaje(Data, istr_Embalaje) OR &
			DuplicadoEnvase(ls_Columna, Data) THEN
			This.SetItem(il_Fila, ls_Columna, ls_Nula)
			RETURN 1
		END IF

//	CASE "enva_codigo"
//		IF NOT ExisteEnvase(istr_Envase.TipoEnvase, Integer(Data), istr_Envase) OR &
//			DuplicadoEnvase(ls_Columna, Data) THEN
//			This.SetItem(il_Fila, ls_Columna, Integer(ls_Nula))
//			This.SetItem(il_Fila, "enva_nombre", ls_Nula)
//
//			RETURN 1
//		ELSE
//			dw_1.GetChild("enva_codigo",idwc_Envase)
//			idwc_Envase.SetTransObject(SQLCA)
//			idwc_Envase.Retrieve(istr_Envase.TipoEnvase)
//			This.Object.enva_nombre[il_Fila]	=	istr_Envase.Nombre
//		END IF

END CHOOSE
end event

event itemerror;call super::itemerror;RETURN 1
end event

event itemfocuschanged;call super::itemfocuschanged;IF IsValid(w_main) THEN
	w_main.SetMicroHelp(dwo.Tag)
END IF
end event

event losefocus;call super::losefocus;AcceptText()
end event

event rowfocuschanged;call super::rowfocuschanged;ib_datos_ok = True

IF This.RowCount() < 1 OR CurrentRow = 0 OR ib_borrar THEN
	ib_datos_ok = False
ELSE
	il_fila = CurrentRow
END IF
end event

event type long ue_seteafila(unsignedlong wparam, long lparam);call super::ue_seteafila;il_fila	= This.GetRow()

RETURN 0
end event

type dw_6 from datawindow within w_maed_spro_ccalidadpackenca
integer x = 2715
integer y = 36
integer width = 553
integer height = 736
integer taborder = 70
boolean bringtotop = true
string title = "Lotes Procesados"
string dataobject = "dw_mues_lotes_procesados"
borderstyle borderstyle = stylelowered!
end type

type dw_7 from datawindow within w_maed_spro_ccalidadpackenca
boolean visible = false
integer x = 37
integer y = 808
integer width = 1705
integer height = 664
integer taborder = 70
boolean bringtotop = true
boolean titlebar = true
string dataobject = "dw_mues_spro_ccalidadpackdetdanos"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

