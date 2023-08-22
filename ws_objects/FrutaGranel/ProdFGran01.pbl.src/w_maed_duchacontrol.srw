$PBExportHeader$w_maed_duchacontrol.srw
forward
global type w_maed_duchacontrol from w_mant_encab_deta_csd
end type
type dw_3 from uo_dw within w_maed_duchacontrol
end type
end forward

global type w_maed_duchacontrol from w_mant_encab_deta_csd
integer width = 3255
integer height = 2016
string title = "CONTROL DE DUCHA"
string menuname = ""
event ue_validaregistro ( )
dw_3 dw_3
end type
global w_maed_duchacontrol w_maed_duchacontrol

type variables
Integer	ii_seleccion, il_filla
Date		id_fecini
Time  	it_horini

uo_duchacontrol		iuo_duchacontrol
uo_especie				iuo_especie
uo_grupoespecie		iuo_grupoespecie
uo_subgrupoespecie	iuo_subgrupoespecie
uo_variedades			iuo_variedades

DataWindowChild	idwc_especie, idwc_grupo, idwc_subgrupo, idwc_variedad, &
						idwc_especiealt, idwc_grupoalt, idwc_subgrupoalt, idwc_variedadalt
end variables

forward prototypes
protected function boolean wf_actualiza_db (boolean borrando)
public subroutine chequeatipoingreso ()
public subroutine habilita_encab (boolean habilita)
public subroutine habilitatipoingreso (integer ai_tipoingreso)
public subroutine bloqueamodificacion (boolean ab_bloquea, integer ai_tipobloqueo)
public subroutine muesgetchild (integer ai_especie, integer ai_especiealt, integer ai_grupo, integer ai_grupoalt, integer ai_recuperacion)
public function boolean verifcierreducha (integer ai_ducha, integer ai_estanque, date ad_fecha, time at_hora)
public function boolean duplicado (string codigo)
public function boolean duplicadopos (string codigopos)
public function boolean existeencabezado (string as_columna, string as_valor)
public subroutine habilitaingreso (string columna)
end prototypes

event ue_validaregistro();Integer	li_cont,li_cont1,ll_fila
String	ls_mensaje,ls_colu[],ls_colu1[]
	
IF Isnull(dw_1.Object.prdu_codigo[il_fila]) OR dw_1.Object.prdu_codigo[il_fila] = "" THEN
	li_cont = 15
	ls_mensaje 			= ls_mensaje + "~nCódigo Productos"
	ls_colu[li_cont]	= "prdu_codigo"
END IF
	
IF Isnull(dw_2.GetitemNumber(1,"espe_codigo")) OR dw_2.GetItemNumber(1, "espe_codigo") = 0 THEN
	li_cont ++
	ls_mensaje 			= ls_mensaje + "~nCódigo Especie"
	ls_colu[li_cont]	= "espe_codigo"
END IF

IF dw_2.Object.codu_tipova[1] = 2 OR dw_2.Object.codu_tipova[1] = 3 THEN
	IF Isnull(dw_2.GetitemNumber(1,"grva_codigo")) OR dw_2.GetitemNumber(1,"grva_codigo") = 0 THEN
		li_cont ++
		ls_mensaje 			= ls_mensaje + "~nCódigo Grupo"
		ls_colu[li_cont]	= "grva_codigo"
	END IF
ELSEIF dw_2.Object.codu_tipova[1] = 3 THEN
		 IF Isnull(dw_2.GetitemNumber(1,"grva_codsub")) OR dw_2.GetitemNumber(1,"grva_codsub") = 0 THEN
			 li_cont ++
			 ls_mensaje 		= ls_mensaje + "~nCódigo Sub-Grupo"
			 ls_colu[li_cont]	= "grva_codsub"
		 END IF
	ELSEIF dw_2.Object.codu_tipova[1] = 4 THEN
			 IF Isnull(dw_2.GetitemNumber(1,"vari_codigo")) OR dw_2.GetitemNumber(1,"vari_codigo") = 0 THEN
				 li_cont ++
				 ls_mensaje 		= ls_mensaje + "~nCódigo Variedad"
				 ls_colu[li_cont]	= "vari_codigo"
			 END IF
END IF

IF Isnull(dw_2.GetitemNumber(1,"codu_tilaca")) OR dw_2.GetItemNumber(1, "codu_tilaca") = 0 THEN
	li_cont ++
	ls_mensaje 			= ls_mensaje + "~nT° Lavado Camión"
	ls_colu[li_cont]	= "codu_tilaca"
END IF

IF Isnull(dw_2.GetitemNumber(1,"codu_tiemax")) OR dw_2.GetItemNumber(1, "codu_tiemax") = 0 THEN
	li_cont ++
	ls_mensaje 			= ls_mensaje + "~nTiempo Máximo"
	ls_colu[li_cont]	= "codu_tiemax"
END IF

IF Isnull(dw_2.GetitemNumber(1,"codu_litros")) OR dw_2.GetItemNumber(1, "codu_litros") = 0 THEN
	li_cont ++
	ls_mensaje 			= ls_mensaje + "~nLitros Preparados"
	ls_colu[li_cont]	= "codu_litros"
END IF

IF Isnull(dw_2.GetitemNumber(1,"codu_maxbul")) OR dw_2.GetItemNumber(1, "codu_maxbul") = 0 THEN
	li_cont ++
	ls_mensaje 			= ls_mensaje + "~nMax. Cant. Bultos"
	ls_colu[li_cont]	= "codu_maxbul"
END IF

IF Isnull(dw_2.GetitemNumber(1,"codu_filtro")) OR dw_2.GetItemNumber(1, "codu_filtro") = 0 THEN
	li_cont ++
	ls_mensaje 			= ls_mensaje + "~nCódigo Filtro"
	ls_colu[li_cont]	= "codu_filtro"
END IF

IF li_cont >= 15 THEN
	MessageBox("Error de Consistencia", "Falta el ingreso de :" + ls_mensaje + ".", StopSign!, Ok!)
	dw_1.SetColumn(ls_colu[15])
	dw_1.SetFocus()
	Message.DoubleParm = -1
ELSEIF li_cont > 0 AND li_cont < 10 THEN
	MessageBox("Error de Consistencia", "Falta el ingreso de :" + ls_mensaje + ".", StopSign!, Ok!)
	dw_2.SetColumn(ls_colu[1])
	dw_2.SetFocus()
	Message.DoubleParm = -1
END IF
end event

protected function boolean wf_actualiza_db (boolean borrando);Boolean	lb_AutoCommit, lb_Retorno

IF Not dw_3.uf_check_required(0) THEN RETURN False

IF Not dw_2.uf_check_required(0) THEN RETURN False

IF Not dw_1.uf_validate(0) THEN RETURN False

lb_AutoCommit		=	sqlca.AutoCommit
sqlca.AutoCommit	=	False

IF Borrando THEN
	IF dw_1.Update(True, False) = 1 THEN
		IF dw_3.Update(True,False) =	1	THEN
			IF dw_2.Update(True, False) = 1 THEN
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
ELSE
	IF dw_2.Update(True, False) = 1 THEN
		IF dw_3.Update(True,False) =	1	THEN
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

public subroutine chequeatipoingreso ();IF NOT IsNull(dw_2.Object.vari_codigo[1]) THEN
	dw_2.Object.codu_tipova[1] = 4
	HabilitaTipoIngreso(4)
	RETURN
ELSEIF dw_2.Object.grva_codsub[1] > 0 THEN
	dw_2.Object.codu_tipova[1] = 3
	HabilitaTipoIngreso(3)
	RETURN
ELSEIF NOT IsNull(dw_2.Object.grva_codigo[1]) THEN
	dw_2.Object.codu_tipova[1] = 2
	HabilitaTipoIngreso(2)
	RETURN
ELSE
	dw_2.Object.codu_tipova[1] = 1
	HabilitaTipoIngreso(1)
	RETURN
END IF
end subroutine

public subroutine habilita_encab (boolean habilita);IF Habilita THEN
	dw_2.Object.duch_codigo.Protect				=	0
	dw_2.Object.codu_nropos.Protect				=	0
	dw_2.Object.codu_fecini.Protect				=	0
	dw_2.Object.codu_horini.Protect				=	0
	dw_2.Object.duch_codigo.BackGround.Color	=	RGB(255,255,255)
	dw_2.Object.codu_nropos.BackGround.Color	=	RGB(255,255,255)
	dw_2.Object.codu_fecini.BackGround.Color	=	RGB(255,255,255)
	dw_2.Object.codu_horini.BackGround.Color	=	RGB(255,255,255)
ELSE
	dw_2.Object.duch_codigo.Protect				=	1
	dw_2.Object.codu_nropos.Protect				=	1
	dw_2.Object.codu_fecini.Protect				=	1
	dw_2.Object.codu_horini.Protect				=	1
	dw_2.Object.duch_codigo.BackGround.Color	=	RGB(192,192,192)
	dw_2.Object.codu_nropos.BackGround.Color	=	RGB(192,192,192)
	dw_2.Object.codu_fecini.BackGround.Color	=	RGB(192,192,192)
	dw_2.Object.codu_horini.BackGround.Color	=	RGB(192,192,192)
END IF
end subroutine

public subroutine habilitatipoingreso (integer ai_tipoingreso);Integer	li_Null

SetNull(li_Null)

CHOOSE CASE ai_TipoIngreso

	CASE 1 // Por Especie
		dw_2.Object.grva_codigo.Visible	=	False
		dw_2.Object.grva_codsub.Visible	=	False
		dw_2.Object.vari_codigo.Visible	=	False
		dw_2.Object.grva_codalt.Visible	=	False
		dw_2.Object.grva_subalt.Visible	=	False
		dw_2.Object.vari_codalt.Visible	=	False
		dw_2.Object.t_grupo.Visible		=	False
		dw_2.Object.t_subgrupo.Visible	=	False
		dw_2.Object.t_variedad.Visible	=	False
		dw_2.Object.t_grupoalt.Visible	=	False
		dw_2.Object.t_subgrupoalt.Visible=	False
		dw_2.Object.t_variedadalt.Visible=	False
		dw_2.Object.grva_codigo[1]			=	li_Null
		dw_2.Object.grva_codsub[1]			=	li_Null
		dw_2.Object.vari_codigo[1]			=	li_Null
		dw_2.Object.grva_codalt[1]			=	li_Null
		dw_2.Object.grva_subalt[1]			=	li_Null
		dw_2.Object.vari_codalt[1]			=	li_Null

	CASE 2 // Por Grupo
		dw_2.Object.grva_codigo.Visible	=	True
		dw_2.Object.grva_codsub.Visible	=	False
		dw_2.Object.vari_codigo.Visible	=	False
		dw_2.Object.grva_codalt.Visible	=	True		
		dw_2.Object.t_subgrupoalt.Visible=	False
		dw_2.Object.t_variedadalt.Visible=	False
		dw_2.Object.t_grupo.Visible		=	True
		dw_2.Object.t_subgrupo.Visible	=	False
		dw_2.Object.t_variedad.Visible	=	False
		dw_2.Object.t_grupoalt.Visible	=	True
		dw_2.Object.t_subgrupoalt.Visible=	False
		dw_2.Object.t_variedadalt.Visible=	False
		dw_2.Object.grva_codsub[1]			=	li_Null
		dw_2.Object.vari_codigo[1]			=	li_Null
		dw_2.Object.grva_subalt[1]			=	li_Null
		dw_2.Object.vari_codalt[1]			=	li_Null

	CASE 3 // Por Sub Grupo
		dw_2.Object.grva_codigo.Visible	=	True
		dw_2.Object.grva_codsub.Visible	=	True
		dw_2.Object.vari_codigo.Visible	=	False
		dw_2.Object.vari_codalt.Visible	=	False		
		dw_2.Object.grva_codalt.Visible	=	True
		dw_2.Object.grva_subalt.Visible	=	True
		dw_2.Object.t_grupo.Visible		=	True
		dw_2.Object.t_subgrupo.Visible	=	True
		dw_2.Object.t_grupoalt.Visible	=	True
		dw_2.Object.t_subgrupoalt.Visible=	True		
		dw_2.Object.t_variedad.Visible	=	False
		dw_2.Object.t_variedadalt.Visible=	False
		dw_2.Object.vari_codigo[1]			=	li_Null

	CASE 4 // Por Variedad
		dw_2.Object.vari_codigo.Visible	=	True
		dw_2.Object.grva_codigo.Visible	=	False
		dw_2.Object.grva_codsub.Visible	=	False
		dw_2.Object.vari_codalt.Visible	=	True
		dw_2.Object.grva_codalt.Visible	=	False
		dw_2.Object.grva_subalt.Visible	=	False
		dw_2.Object.t_variedad.Visible	=	True
		dw_2.Object.t_grupo.Visible		=	False
		dw_2.Object.t_subgrupo.Visible	=	False
		dw_2.Object.t_variedadalt.Visible	=	True
		dw_2.Object.t_grupoalt.Visible		=	False
		dw_2.Object.t_subgrupoalt.Visible	=	False
		dw_2.Object.grva_codigo[1]			=	li_Null
		dw_2.Object.grva_codsub[1]			=	li_Null
		dw_2.Object.grva_codalt[1]			=	li_Null
		dw_2.Object.grva_subalt[1]			=	li_Null

END CHOOSE
end subroutine

public subroutine bloqueamodificacion (boolean ab_bloquea, integer ai_tipobloqueo);IF ab_Bloquea THEN
	IF ai_Tipobloqueo = 1 THEN
		MessageBox("Atención", "Ya se efectuó Relleno(s) para esta Conformación " + &
						"de Ducha.~r~rSólo podrá consultar.")
	ELSEIF ai_Tipobloqueo = 2 THEN					
	MessageBox("Atención", "Ya se efectuó el Cierre para esta  " + &
					"Ducha.~r~rSólo podrá consultar.")
	END IF
	
	dw_1.Object.prdu_codigo.Protect	=	1
	dw_1.Object.bpdd_cantid.Protect	=	1
ELSE
	dw_1.Object.prdu_codigo.Protect	=	0
	dw_1.Object.bpdd_cantid.Protect	=	0
END IF

pb_eliminar.Enabled	=	Not(ab_Bloquea)
pb_grabar.Enabled		=	Not(ab_Bloquea)
pb_ins_det.Enabled	=	Not(ab_Bloquea)
pb_eli_det.Enabled	=	Not(ab_Bloquea)
pb_imprimir.Enabled	=	Not(ab_Bloquea)
dw_2.Enabled			=	Not(ab_Bloquea)

Habilita_Encab(Not(ab_Bloquea))

istr_Mant.Solo_Consulta	=	ab_Bloquea
end subroutine

public subroutine muesgetchild (integer ai_especie, integer ai_especiealt, integer ai_grupo, integer ai_grupoalt, integer ai_recuperacion);IF ai_recuperacion = 4 THEN
//Grupo
dw_2.GetChild("grva_codigo",idwc_grupo)
idwc_grupo.SetTransObject(Sqlca)
idwc_grupo.Retrieve(ai_especie)
END IF

IF ai_recuperacion = 2 THEN
	//Sub Grupo
	dw_2.GetChild("grva_codsub",idwc_subgrupo)
	idwc_subgrupo.SetTransObject(Sqlca)
	idwc_subgrupo.Retrieve(ai_especie,ai_grupo)
END IF

//Variedad
dw_2.GetChild("vari_codigo",idwc_variedad)
idwc_variedad.SetTransObject(Sqlca)
idwc_variedad.Retrieve(ai_especie)


IF ai_recuperacion = 3 THEN	
	//Sub Grupo Alternativo
	dw_2.GetChild("grva_subalt",idwc_subgrupoalt)
	idwc_subgrupoalt.SetTransObject(Sqlca)
	idwc_subgrupoalt.Retrieve(ai_especiealt,ai_grupoalt)
END IF

IF ai_Recuperacion = 1 THEN

	//Grupo Alternativo
	dw_2.GetChild("grva_codalt",idwc_grupoalt)
	idwc_grupoalt.SetTransObject(Sqlca)
	idwc_grupoalt.Retrieve(ai_especiealt)
	
	//Sub Grupo Alternativo
	dw_2.GetChild("grva_subalt",idwc_subgrupoalt)
	idwc_subgrupoalt.SetTransObject(Sqlca)
	idwc_subgrupoalt.Retrieve(ai_especiealt,ai_grupoalt)
	
	//Variedad Alternativa
	dw_2.GetChild("vari_codalt",idwc_variedadalt)
	idwc_variedadalt.SetTransObject(Sqlca)
	idwc_variedadalt.Retrieve(ai_especiealt)
END IF
end subroutine

public function boolean verifcierreducha (integer ai_ducha, integer ai_estanque, date ad_fecha, time at_hora);Date ld_Fecter

SELECT	codu_fecter
	INTO	:ld_fecter
	FROM	dba.spro_duchacontrol
	WHERE	duch_codigo	=	:ai_Ducha
	AND	codu_nropos	=	:ai_Estanque
	AND	codu_fecini	=	:ad_Fecha
	AND	codu_horini	=	:at_Hora ;

IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla Spro_duchacontrol")
	RETURN FALSE
ELSEIF sqlca.SQLCode = 100 THEN
	RETURN FALSE
ELSE
	IF ld_fecter <> Date("01/01/1900") THEN
		BloqueaModificacion(True, 2)
		RETURN TRUE
	END IF
	RETURN FALSE
	
END IF
end function

public function boolean duplicado (string codigo);Long		ll_fila
String	ls_codigo

ll_fila	= dw_1.Find("prdu_codigo = '" + codigo + "'", 1, dw_1.RowCount())


IF ll_fila > 0 and ll_fila <> il_fila THEN
	MessageBox("Error","Código de Item, ya fue Ingresado Anteriormente Para este Código de Ducha",Information!, Ok!)
	RETURN True
ELSE
	RETURN False
END IF
end function

public function boolean duplicadopos (string codigopos);Long		ll_fila
String	ls_codigo

IF Integer(istr_mant.argumento[1])	<> 0 THEN
	
	ll_fila	= dw_3.Find("duch_codigo = " + istr_mant.argumento[1] + "And codu_nropos= " + codigopos , 1, dw_3.RowCount())
	
	
	IF ll_fila > 0 and ll_fila <> il_fila THEN
		MessageBox("Error","Número de Pozo, ya fue Ingresado Anteriormente Para este Código de Ducha",Information!, Ok!)
		RETURN True
	ELSE
		RETURN False
	END IF
ELSE
	MessageBox("Error","Código de Planta No a sido Ingresado",Information!, Ok!)
	dw_3.SEtColumn("duch_codigo")
	Return True
END IF	
end function

public function boolean existeencabezado (string as_columna, string as_valor);Boolean	lb_Retorno
Integer	li_Ducha, li_Estanque, li_Cantidad
Date		ld_Fecha, ld_FechaNula
Time		lt_Hora
String   ls_fecha, ls_hora

li_Ducha		= dw_2.Object.duch_codigo[1]
li_Estanque	= dw_2.Object.codu_nropos[1]
ld_Fecha	= dw_2.Object.codu_fecini[1]
lt_Hora		= dw_2.Object.codu_horini[1]



CHOOSE CASE as_Columna
	CASE "duch_codigo"
		li_Ducha		=	Integer(as_Valor)
		
	CASE "codu_nropos"
		li_Estanque	=	Integer(as_Valor)
		
	CASE "codu_fecini"
		ld_Fecha	=	Date(Mid(as_Valor, 1, 10))
		
	CASE "codu_horini"
		ls_fecha	=  Mid(String(ld_fecha),1,10)
		ls_hora	=  Mid(as_valor,12,5)
		ls_hora  =  ls_hora+':00'
		lt_Hora	=	Time(ls_Hora)
      dw_2.Object.codu_horini[1]	=	Time(ls_Hora)
				
END CHOOSE

SELECT	Count(*)
	INTO	:li_Cantidad
	FROM	dba.spro_duchacontrol
	WHERE	duch_codigo	=	:li_Ducha
	AND	codu_nropos	=	:li_Estanque
	AND	codu_fecini	=	:ld_Fecha
	AND	codu_horini	=	:lt_Hora ;

IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla Spro_duchacontrol")
	
	RETURN TRUE
ELSEIF sqlca.SQLCode <> 100 and li_cantidad>0 THEN
	istr_mant.argumento[1]	=	String(li_Ducha)
	istr_mant.argumento[2]	=	String(li_Estanque)
	istr_mant.argumento[3]	=	String(ld_Fecha)
	istr_mant.argumento[4]	=	String(lt_Hora)

	This.TriggerEvent("ue_recuperadatos")
	
	lb_Retorno	=	True
ELSE
	lb_Retorno	=	False
END IF

RETURN lb_Retorno
end function

public subroutine habilitaingreso (string columna);Boolean	lb_Estado = True
Date		ld_Fecha
Time		lt_Hora
	
IF IsNull(dw_2.Object.duch_codigo[il_fila]) OR dw_2.Object.duch_codigo[il_fila] = 0 THEN
	lb_Estado	=	False
END IF

IF IsNull(dw_2.Object.codu_nropos[il_fila]) OR dw_2.Object.codu_nropos[il_fila] = 0 THEN
	lb_Estado	=	False
END IF

IF IsNull(dw_2.Object.codu_fecini[il_fila]) OR dw_2.Object.codu_fecini[il_fila] = ld_Fecha THEN
	lb_Estado	=	False
END IF

IF IsNull(dw_2.Object.codu_horini[il_fila]) OR dw_2.Object.codu_horini[il_fila] = lt_Hora THEN
	lb_Estado	=	False
END IF

pb_ins_det.Enabled	=	lb_Estado
end subroutine

event ue_seleccion();TriggerEvent("ue_nuevo")

OpenWithParm(w_busc_duchacontrol, istr_busq)

istr_busq	= Message.PowerObjectParm

IF istr_busq.argum[1] <> "" THEN
	istr_mant.argumento[1] = istr_busq.argum[1]
	istr_mant.argumento[2] = istr_busq.argum[2]
	istr_mant.argumento[3] = istr_busq.argum[3]
	istr_mant.argumento[4] = istr_busq.argum[4]
	
	TriggerEvent("ue_recuperadatos")
ELSE
	pb_buscar.SetFocus()
END IF
end event

event ue_recuperadatos();/*
Datos bloqueamodificacion
Relleno = 1
Cierre  = 2
*/ 

Long		ll_fila_d, ll_fila_e, respuesta,ll_fila_f
Date		ld_Fecha
Time		lt_Hora

ld_Fecha	=	Date(istr_mant.Argumento[3])
lt_Hora	=	Time(istr_mant.Argumento[4])
 
DO
	dw_2.SetRedraw(False)
	
	ll_fila_e	=	dw_2.Retrieve(Integer(istr_mant.Argumento[1]), &
										Integer(istr_mant.Argumento[2]), &
										ld_Fecha, &
										lt_Hora)
	
	IF ll_fila_e = -1 THEN
		respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
										Information!, RetryCancel!)
	ELSE
		dw_2.Object.espe_codalt.protect	=	0
		
// Recupera la especie para las getchild y verifica se especie alt esta ingresada
// 0 = No recuperaalternativo
// 1 = recupera alternativo  con subgrupo     
// 2 = Recupera subgrupo sin alternativo
// 3 = Recupera grupo con alternativo
// 4 = Recupera grupo sin alternativo


	 		IF Not IsNull(dw_2.Object.espe_codalt[1]) AND  Not IsNull(dw_2.Object.grva_codigo[1]) THEN
				muesgetchild(Integer(dw_2.Object.espe_codigo[1]),&
			             	 Integer(dw_2.Object.espe_codalt[1]),&
 							    Integer(dw_2.Object.grva_codsub[1]),&
								 Integer(dw_2.Object.grva_codsub[1]),1)
			ELSEIF  Not IsNull(dw_2.Object.grva_codsub[1]) THEN
			   muesgetchild(Integer(dw_2.Object.espe_codigo[1]), 0,&
							    Integer(dw_2.Object.grva_codsub[1]), 0, 2)
				 ELSE
				    IF Not IsNull(dw_2.Object.espe_codalt[1]) THEN 
			          muesgetchild(Integer(dw_2.Object.espe_codigo[1]), &
				                Integer(dw_2.Object.espe_codalt[1]), 0,0,3)
				    ELSE
					    muesgetchild(Integer(dw_2.Object.espe_codigo[1]),0,0,0,4)
	  		       END IF
			END IF

	

		habilitatipoingreso(Integer(dw_2.Object.codu_tipova[1]))
		
		ChequeaTipoIngreso()

		istr_Mant.Argumento[7]	= 	String(dw_2.Object.espe_codigo[1])
		istr_Mant.Argumento[8]	= 	String(dw_2.Object.grva_codigo[1])
		
		DO
			ll_fila_d	=	dw_1.Retrieve(Integer(istr_mant.Argumento[1]), &
												Integer(istr_mant.Argumento[2]), &
												ld_Fecha, lt_Hora, 1, ld_Fecha, &
												lt_Hora)
		IF ll_fila_d = -1 THEN
				respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
												Information!, RetryCancel!)
			ELSE
				DO
					ll_fila_f	= dw_3.Retrieve(Integer(istr_mant.Argumento[1]), &
														Integer(istr_mant.Argumento[2]), &
														ld_Fecha, &
														lt_Hora)
					IF ll_fila_f = -1 THEN
						respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
														Information!, RetryCancel!)		
					ELSE
						pb_eliminar.Enabled	=	True
						pb_grabar.Enabled		=	True
						pb_ins_det.Enabled	=	True
		
						IF ll_fila_d > 0 OR ll_fila_e > 0 OR ll_fila_f >0 THEN
							pb_eli_det.Enabled	=	True
							pb_imprimir.Enabled	=	True
							dw_1.SetRow(1)
							//dw_1.SelectRow(1,True)
							pb_ins_det.SetFocus()
							
							Habilita_Encab(False)
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
	END IF
	dw_2.SetRedraw(True)
LOOP WHILE respuesta = 1

IF respuesta = 2 THEN Close(This)

IF ll_fila_f > 1 THEN
	BloqueaModificacion(True, 1)
END IF

verifcierreducha(Integer(istr_mant.Argumento[1]), Integer(Istr_mant.Argumento[2]), &
					  ld_fecha, lt_hora)


end event

event ue_nuevo_detalle();IF istr_Mant.Solo_Consulta THEN RETURN

istr_mant.borra 	= False
istr_mant.agrega	= True
	
IF ii_seleccion	=	0	THEN	
	IF il_fila > 0 THEN
		pb_eli_det.Enabled	= True
		pb_grabar.Enabled		= True
	END IF
	
	il_fila = dw_1.InsertRow(0)
	
	IF il_fila>0 THEN
		Habilita_encab(FALSE)
	END IF	
	dw_1.ScrollToRow(il_fila)
	dw_1.SetRow(il_fila)
	dw_1.SetFocus()
	
	dw_1.SetColumn("prdu_codigo")
ELSE
	IF il_fila > 0 THEN
		pb_eli_det.Enabled	= True
		pb_grabar.Enabled		= True
		IF il_fila>0 THEN
			Habilita_encab(FALSE)
		END IF	
	END IF
END IF
end event

event ue_imprimir();SetPointer(HourGlass!)

Long		fila
str_info	lstr_info

lstr_info.titulo	= "DUCHAS PLANTAS"
lstr_info.copias	= 1

OpenWithParm(vinf,lstr_info)

vinf.dw_1.DataObject = "dw_info_duchaplanta"

vinf.dw_1.SetTransObject(sqlca)

fila = vinf.dw_1.Retrieve(Integer(istr_mant.argumento[1]))

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

event ue_borra_detalle();IF istr_Mant.Solo_Consulta THEN RETURN

IF ii_seleccion = 0 THEN
   IF dw_1.rowcount() < 1 THEN RETURN

   SetPointer(HourGlass!)
 
   ib_borrar = True
   w_main.SetMicroHelp("Validando la eliminación de detalle...")

   Message.DoubleParm = 0

   This.TriggerEvent ("ue_validaborrar_detalle")

   IF Message.DoubleParm = -1 THEN RETURN

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
ELSE 
     IF dw_3.rowcount() < 1 THEN RETURN

       SetPointer(HourGlass!)
 
       ib_borrar = True
       w_main.SetMicroHelp("Validando la eliminación de detalle...")

       Message.DoubleParm = 0

       This.TriggerEvent ("ue_validaborrar_detalle")
      
       IF Message.DoubleParm = -1 THEN RETURN
      
      IF MessageBox("Eliminación de Registro", "Está seguro de Eliminar este Registro", Question!, YesNo!) = 1 THEN
         IF dw_3.DeleteRow(0) = 1 THEN
	         ib_borrar = False
		      w_main.SetMicroHelp("Borrando Registro...")
		      SetPointer(Arrow!)
         ELSE
	      	ib_borrar = False
	   	   MessageBox(This.Title,"No se puede borrar actual registro.")
         END IF

         IF dw_3.RowCount() = 0 THEN
 	         pb_eliminar.Enabled = False
         ELSE
		      il_fila = dw_3.GetRow()
         END IF
     END IF
END IF
end event

on w_maed_duchacontrol.create
int iCurrent
call super::create
this.dw_3=create dw_3
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_3
end on

on w_maed_duchacontrol.destroy
call super::destroy
destroy(this.dw_3)
end on

event ue_modifica_detalle();call super::ue_modifica_detalle;//IF istr_Mant.Solo_Consulta THEN RETURN
//
//IF dw_1.RowCount() > 0 THEN
//	istr_mant.agrega	= False
//	istr_mant.borra	= False
//
//	OpenWithParm(iw_mantencion, istr_mant)
//END IF
end event

event open;call super::open;/*
Argumentos
istr_mant.argumento[1]	=	Código Ducha
istr_mant.argumento[2] 	=	Nº Pozo
istr_mant.argumento[3]	=	Fecha Inicio Y Evento
istr_mant.argumento[4]	=	Hora Inicio Y Evento
istr_mant.argumento[5]	=	Tipo Evento
istr_mant.argumento[6]	=	Cantidad Bins
istr_mant.argumento[7]	=	Codigo de Especie
istr_mant.argumento[8]	=	Codigo de Grupo Variedad
*/

iuo_duchacontrol		=	Create uo_duchacontrol
iuo_especie				=	Create uo_especie
iuo_grupoespecie		=	Create uo_grupoespecie
iuo_subgrupoespecie	=	Create uo_subgrupoespecie
iuo_variedades			=	Create uo_variedades

dw_3.SetTransObject(sqlca)
istr_mant.dw.ShareData(dw_3)

//Especie
dw_2.GetChild("espe_codigo", idwc_especie)
idwc_especie.SetTransObject(sqlca)
IF idwc_especie.Retrieve() = 0 THEN
	MessageBox("Atención","Falta Registrar Especies")
ELSE
	idwc_especie.InsertRow(0)
END IF

//Especie Alternativa
dw_2.GetChild("espe_codalt", idwc_especiealt)
idwc_especiealt.SetTransObject(sqlca)
IF idwc_especiealt.Retrieve() = 0 THEN
	MessageBox("Atención","Falta Registrar Especies")
ELSE
	idwc_especiealt.InsertRow(0)
END IF

//Grupo
dw_2.GetChild("grva_codigo",idwc_grupo)
idwc_grupo.SetTransObject(Sqlca)
idwc_grupo.InsertRow(0)


//Grupo Alternativo
dw_2.GetChild("grva_codalt",idwc_grupoalt)
idwc_grupoalt.SetTransObject(Sqlca)
idwc_grupoalt.InsertRow(0)

//Sub Grupo
dw_2.GetChild("grva_codsub",idwc_subgrupo)
idwc_subgrupo.SetTransObject(Sqlca)
idwc_subgrupo.InsertRow(0)

//Sub Grupo Alternativo
dw_2.GetChild("grva_subalt",idwc_subgrupoalt)
idwc_subgrupoalt.SetTransObject(Sqlca)
idwc_subgrupoalt.InsertRow(0)

//Variedad
dw_2.GetChild("vari_codigo",idwc_variedad)
idwc_variedad.SetTransObject(Sqlca)
idwc_variedad.InsertRow(0)

//Variedad Alternativa
dw_2.GetChild("vari_codalt",idwc_variedadalt)
idwc_variedadalt.SetTransObject(Sqlca)
idwc_variedadalt.InsertRow(0)

end event

event ue_antesguardar();Long		ll_Fila
String   ls_fecha, ls_hora

il_fila	=	1

ls_fecha	=  String(dw_2.Object.codu_fecini[1])
ls_hora	=  String(dw_2.Object.codu_horini[1])
ls_hora  =  ls_hora+':00'

dw_2.Object.codu_horini[1]	=	Time(ls_Hora)

DO WHILE il_fila <= dw_1.RowCount()
	IF dw_1.GetItemStatus(il_fila, 0, Primary!) = New! THEN
		TriggerEvent("ue_validaregistro")
		dw_1.DeleteRow(il_fila)
	ELSEIF dw_1.GetItemStatus(il_fila, 0, Primary!) = NewModified! THEN
		TriggerEvent("ue_validaregistro")
		IF Message.DoubleParm = -1 THEN RETURN
		
		dw_1.SetItem(il_fila, "duch_codigo", dw_2.Object.duch_codigo[1])
		dw_1.SetItem(il_fila, "codu_nropos", dw_2.Object.codu_nropos[1])
		dw_1.SetItem(il_fila, "codu_fecini", dw_2.Object.codu_fecini[1])
		
		dw_1.SetItem(il_fila, "codu_horini", dw_2.Object.codu_horini[1])
		dw_1.SetItem(il_fila, "bpdu_tipoev", 1)
		dw_1.SetItem(il_fila, "bpdu_feceve", dw_2.Object.codu_fecini[1])
		dw_1.SetItem(il_fila, "bpdu_horeve", dw_2.Object.codu_horini[1])		
		
	END IF
	
	il_fila ++
LOOP

IF dw_2.GetItemStatus(1, 0, Primary!) = New! OR &
	dw_2.GetItemStatus(1, 0, Primary!) = NewModified! THEN
	ll_Fila	=	dw_3.InsertRow(0)
	
	dw_3.SetItem(ll_Fila, "duch_codigo", dw_2.Object.duch_codigo[1])
	dw_3.SetItem(ll_Fila, "codu_nropos", dw_2.Object.codu_nropos[1])
	dw_3.SetItem(ll_Fila, "codu_fecini", dw_2.Object.codu_fecini[1])
	dw_3.SetItem(ll_Fila, "codu_horini", dw_2.Object.codu_horini[1])
	dw_3.SetItem(ll_Fila, "bpdu_tipoev", 1)
	dw_3.SetItem(ll_Fila, "bpdu_feceve", dw_2.Object.codu_fecini[1])
	dw_3.SetItem(ll_Fila, "bpdu_horeve", dw_2.Object.codu_horini[1])		
	dw_3.SetItem(ll_Fila, "bpdu_canbin", 0)
END IF
end event

event ue_nuevo();Long		ll_modif1, ll_modif2,ll_modif3
String   ls_hora
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
		
			IF dw_1.RowCount() > 0 OR dw_3.RowCount() > 0 THEN
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
dw_3.Reset()

dw_2.Object.grva_codigo.Visible	=	False
dw_2.Object.grva_codsub.Visible	=	False
dw_2.Object.vari_codigo.Visible	=	False
dw_2.Object.grva_codalt.Visible	=	False
dw_2.Object.grva_subalt.Visible	=	False
dw_2.Object.vari_codalt.Visible	=	False
dw_2.Object.t_grupo.Visible		=	False
dw_2.Object.t_subgrupo.Visible	=	False
dw_2.Object.t_variedad.Visible	=	False
dw_2.Object.t_grupoALT.Visible	=	False
dw_2.Object.t_subgrupoalt.Visible=	False
dw_2.Object.t_variedadalt.Visible=	False

dw_2.Object.espe_codalt.protect	=	1

pb_eli_det.Enabled		=	False
pb_ins_det.Enabled		=	False
pb_grabar.Enabled			=	False
pb_eliminar.Enabled		=	False
pb_imprimir.Enabled		=	False
dw_2.Enabled				=	True

dw_1.Object.prdu_codigo.Protect	=	0
dw_1.Object.bpdd_cantid.Protect	=	0

dw_2.SetRedraw(False)
dw_2.Reset()
dw_2.InsertRow(0)

idwc_grupo.SetTransObject(Sqlca)
idwc_grupo.InsertRow(0)

dw_2.Object.codu_fecini[1]		=	Date(Today())
ls_hora                       =  MID(String(Now()),1,5)
ls_hora								=	ls_hora+':00'
dw_2.Object.codu_horini[1]		=	Time(ls_hora)
dw_2.Object.duch_ventana[1]	=	1

dw_2.SetRedraw(True)
Habilita_Encab(True)
dw_2.SetFocus()

dw_2.Object.codu_tipova[1] = 1
end event

event ue_validaborrar();call super::ue_validaborrar;//IF iuo_duchaplanta.ofp_validaborrarducha(SQLCa,Integer(istr_mant.argumento[1]),0)	=	0	THEN			
//	MessageBox("Atención"," No Se Puede Borrar El Regitro,Esta Referenciada En Tabla SPRO_DUCHACONTROL",Exclamation!)
//	Message.DoubleParm = -1
//END IF
end event

event ue_borrar();IF istr_Mant.Solo_Consulta THEN RETURN

IF dw_3.RowCount() < 1 OR dw_2.Rowcount() < 1 THEN RETURN

SetPointer(HourGlass!)

ib_borrar = True
w_main.SetMicroHelp("Validando la eliminación...")

Message.DoubleParm = 0

This.TriggerEvent ("ue_validaborrar")

IF Message.DoubleParm = -1 THEN RETURN

IF dw_3.RowCount() > 0 THEN dw_3.RowsMove(1, dw_3.RowCount(), Primary!, dw_3, 1, Delete!)

IF dw_1.RowCount() > 0 THEN dw_1.RowsMove(1, dw_1.RowCount(), Primary!, dw_1, 1, Delete!)

IF dw_2.DeleteRow(0) = 1 THEN
	ib_borrar	=	False
	
	w_main.SetMicroHelp("Borrando Registro...")
	
	IF wf_actualiza_db(True) THEN
		w_main.SetMicroHelp("Registro Borrado...")
		This.TriggerEvent("ue_nuevo")
		SetPointer(Arrow!)
	ELSE
		w_main.SetMicroHelp("Registro no Borrado...")
	END IF
END IF
end event

type dw_1 from w_mant_encab_deta_csd`dw_1 within w_maed_duchacontrol
integer x = 41
integer y = 1128
integer width = 1673
integer height = 644
integer taborder = 30
boolean titlebar = false
string title = "Composición"
string dataobject = "dw_mues_spro_bitaproducduchadet"
boolean hscrollbar = false
end type

event dw_1::itemchanged;String	ls_null

SetNull(ls_null)

CHOOSE CASE GetColumnName()

	CASE "prdu_codigo"	
		IF Duplicado(data) THEN
			This.SetItem(row, "prdu_codigo",ls_null)
			RETURN 1
		END IF
		
	CASE "bpdd_cantid"
		IF dwo.Type = 'column' THEN
			IF NOT This.uf_validate(row) THEN
				THIS.SetItem(row, "bpdd_cantid", Dec(ls_Null))
				RETURN 1
			END IF
		END IF
END CHOOSE
end event

event dw_1::rowfocuschanged;//ib_datos_ok = True
//
//IF This.RowCount() < 1 OR CurrentRow = 0 OR ib_borrar THEN
//	ib_datos_ok = False
//ELSE
//	il_fila = CurrentRow
//END IF
end event

event dw_1::getfocus;//
end event

event dw_1::doubleclicked;//
end event

event dw_1::clicked;//
end event

event dw_1::constructor;call super::constructor;This.uf_add_validation('bpdd_cantid > 0 and bpdd_cantid < 99999.999','Valor fuera de rango para la cantidad')
end event

type dw_2 from w_mant_encab_deta_csd`dw_2 within w_maed_duchacontrol
integer x = 41
integer y = 60
integer width = 2569
integer height = 1036
integer taborder = 10
string dataobject = "dw_mant_duchacontrol"
end type

event dw_2::itemchanged;Integer		li_especiealt
String		ls_Columna, ls_Null
Date			ldt_Fecha
Time			ldt_Hora

SetNull(ls_Null)

ls_Columna	= dwo.Name

CHOOSE CASE ls_Columna

	CASE "duch_codigo", "codu_nropos", "codu_fecini", "codu_horini"
	   ExisteEncabezado(ls_Columna, Data)

	CASE 	"codu_tipova"
		IF Data = "1"	THEN
			HabilitaTipoIngreso(1)
		ELSEIF Data = "2"	THEN
			HabilitaTipoIngreso(2)
		ELSEIF Data = "3"	THEN
			HabilitaTipoIngreso(3)
		ELSE
			HabilitaTipoIngreso(4)
		END IF

	CASE "espe_codigo"
	   IF NOT iuo_especie.Existe(Integer(Data), True, SqlCa) THEN
			This.SetItem(il_Fila, ls_Columna, Integer(ls_Null))
			RETURN 1
		ELSE
			istr_Mant.Argumento[7]	=	Data
			/*
			*/
			This.GetChild("grva_codigo",idwc_grupo)
			idwc_grupo.SetTransObject(Sqlca)
			idwc_grupo.Reset()
			idwc_grupo.Retrieve(Integer(istr_Mant.Argumento[7]))
			idwc_grupo.InsertRow(0)
			/**/
			This.GetChild("grva_codsub",idwc_subgrupo)
			idwc_subgrupo.SetTransObject(Sqlca)
			idwc_subgrupo.Reset()
			idwc_subgrupo.Retrieve(Integer(istr_Mant.Argumento[7]),0)
			idwc_subgrupo.InsertRow(0)
			/**/
			This.GetChild("vari_codigo",idwc_variedad)
			idwc_variedad.SetTransObject(Sqlca)
			idwc_variedad.Reset()
			idwc_variedad.Retrieve(Integer(istr_Mant.Argumento[7]))
			idwc_variedad.InsertRow(0)
			/**/
			dw_2.Object.espe_codalt[row]		= Integer(ls_Null)
			dw_2.Object.espe_codalt.protect	= 0
		END IF

	CASE "espe_codalt"
	   IF NOT iuo_especie.Existe(Integer(Data), True, SqlCa) THEN
			This.SetItem(il_Fila, ls_Columna, Integer(ls_Null))
			RETURN 1
		ELSEIF Integer(Istr_Mant.Argumento[7]) = Integer(Data) THEN
					Messagebox("Error","La Especie se encuentra seleccionada", Exclamation!)
					dw_2.Object.espe_codalt[row] = Integer(ls_Null)
					RETURN 1
				ELSE
					li_especiealt = Integer(Data)
					/**/
					This.GetChild("grva_codalt",idwc_grupoalt)
					idwc_grupoalt.SetTransObject(Sqlca)
					idwc_grupoalt.Reset()
					idwc_grupoalt.Retrieve(li_especiealt)
					idwc_grupoalt.InsertRow(0)
					/**/
					This.GetChild("grva_subalt",idwc_subgrupoalt)
					idwc_subgrupoalt.SetTransObject(Sqlca)
					idwc_subgrupoalt.Reset()
					idwc_subgrupoalt.Retrieve(li_especiealt,0)
					idwc_subgrupoalt.InsertRow(0)
					/**/
					This.GetChild("vari_codalt",idwc_variedadalt)
					idwc_variedadalt.SetTransObject(Sqlca)
					idwc_variedadalt.Reset()
					idwc_variedadalt.Retrieve(li_especiealt)
					idwc_variedadalt.InsertRow(0)
		END IF

	CASE "grva_codigo"
	   IF NOT iuo_grupoespecie.Existe(Integer(istr_Mant.Argumento[7]), &
			Integer(Data), True, SqlCa) THEN
			This.SetItem(il_Fila, ls_Columna, Integer(ls_Null))
			RETURN 1
		ELSE
			istr_Mant.Argumento[8]	=	Data
			/**/			
			This.GetChild("grva_codsub",idwc_subgrupo)
			idwc_subgrupo.SetTransObject(Sqlca)
			idwc_subgrupo.Reset()
			idwc_subgrupo.Retrieve(Integer(istr_Mant.Argumento[7]),Integer(istr_Mant.Argumento[8]))
			idwc_subgrupo.InsertRow(0)
		END IF

	CASE "grva_codsub"
	   IF NOT iuo_subgrupoespecie.Existe(Integer(istr_Mant.Argumento[7]), &
			Integer(istr_Mant.Argumento[8]),Integer(Data), True, SqlCa) THEN
			This.SetItem(il_Fila, ls_Columna, Integer(ls_Null))
			RETURN 1
		END IF

	CASE "vari_codigo"
	   IF NOT iuo_variedades.Existe(Integer(istr_Mant.Argumento[7]), &
			Integer(Data), True, SqlCa) THEN
			This.SetItem(il_Fila, ls_Columna, Integer(ls_Null))
			RETURN 1
		END IF

END CHOOSE

HabilitaIngreso(ls_Columna)
end event

type pb_nuevo from w_mant_encab_deta_csd`pb_nuevo within w_maed_duchacontrol
integer x = 2953
integer y = 380
integer height = 124
integer taborder = 70
end type

type pb_eliminar from w_mant_encab_deta_csd`pb_eliminar within w_maed_duchacontrol
integer x = 2953
integer y = 308
integer taborder = 0
end type

type pb_grabar from w_mant_encab_deta_csd`pb_grabar within w_maed_duchacontrol
integer x = 2953
integer y = 544
integer taborder = 50
end type

type pb_imprimir from w_mant_encab_deta_csd`pb_imprimir within w_maed_duchacontrol
boolean visible = false
integer x = 2953
integer y = 724
integer taborder = 60
end type

type pb_salir from w_mant_encab_deta_csd`pb_salir within w_maed_duchacontrol
integer x = 2953
integer y = 904
integer taborder = 80
end type

type pb_ins_det from w_mant_encab_deta_csd`pb_ins_det within w_maed_duchacontrol
integer x = 2661
integer y = 1496
integer taborder = 20
end type

type pb_eli_det from w_mant_encab_deta_csd`pb_eli_det within w_maed_duchacontrol
integer x = 2661
integer y = 1664
integer taborder = 40
end type

type pb_buscar from w_mant_encab_deta_csd`pb_buscar within w_maed_duchacontrol
integer x = 2953
integer y = 168
integer taborder = 0
end type

type dw_3 from uo_dw within w_maed_duchacontrol
boolean visible = false
integer x = 1746
integer y = 1120
integer width = 855
integer height = 644
integer taborder = 0
string title = "Pozo"
string dataobject = "dw_mues_spro_bitaproducducha"
boolean hscrollbar = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event itemchanged;call super::itemchanged;Integer	li_null

SetNull(li_null)

CHOOSE CASE GetColumnName()

	CASE "codu_nropos"	
		IF Duplicadopos(data) THEN
			This.SetItem(il_fila, "codu_nropos",li_null)
			RETURN 1
		END IF
		
END CHOOSE
end event

event clicked;ii_seleccion	=	1
IF Row > 0 THEN
	il_fila = Row
END IF

RETURN 0

end event

