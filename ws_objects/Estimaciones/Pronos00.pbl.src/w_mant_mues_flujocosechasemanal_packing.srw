$PBExportHeader$w_mant_mues_flujocosechasemanal_packing.srw
$PBExportComments$Ingreso Cosecha Packing Particular
forward
global type w_mant_mues_flujocosechasemanal_packing from w_mant_directo
end type
type dw_3 from datawindow within w_mant_mues_flujocosechasemanal_packing
end type
type dw_4 from uo_dw within w_mant_mues_flujocosechasemanal_packing
end type
type dw_2 from uo_dw within w_mant_mues_flujocosechasemanal_packing
end type
end forward

global type w_mant_mues_flujocosechasemanal_packing from w_mant_directo
integer width = 4037
integer height = 1976
windowstate windowstate = maximized!
dw_3 dw_3
dw_4 dw_4
dw_2 dw_2
end type
global w_mant_mues_flujocosechasemanal_packing w_mant_mues_flujocosechasemanal_packing

type variables
Integer ii_semana, ii_Calidad

DataWindowChild	idwc_Especie, idwc_Variedad, idwc_Productores, idwc_Predio, idwc_Cuartel

uo_Especie 			iuo_Especie
uo_Variedades 		iuo_Variedad
uo_Productores 	iuo_Productor
uo_ProdPredio		iuo_Predio
uo_ProdCuarteles	iuo_Cuartel
uo_NroSemana		iuo_semana
end variables

forward prototypes
public subroutine habilitaencab (boolean habilita)
public subroutine totalcosecha (integer ai_fila)
public function boolean wf_fechaingreso (date ad_fecha)
protected function boolean wf_actualiza_db ()
public subroutine wf_cargaestimacion ()
public subroutine wf_bloqueacolumnas (boolean ab_bloquea)
public subroutine wf_distribuciondiaria (long total, integer dias, ref long resultado[])
end prototypes

public subroutine habilitaencab (boolean habilita);If Habilita Then	
	dw_2.Object.todosvari.Protect 				= 0 
	dw_2.Object.todosCuartel.Protect			= 0 
	
	dw_2.Object.espe_codigo.Protect 			= 0
	dw_2.Object.prod_codigo.Protect 			= 0
	dw_2.Object.prpr_codigo.Protect 			= 0
	
	dw_2.Object.espe_codigo.Color 			= 0
	dw_2.Object.vari_codigo.Color 				= 0
	dw_2.Object.prod_codigo.Color 			= 0
	dw_2.Object.prpr_codigo.Color 			= 0
	dw_2.Object.prcc_codigo.Color 			= 0
	
	dw_2.Object.espe_codigo.BackGround.Color = RGB(255,255,255)
	dw_2.Object.prod_codigo.BackGround.Color = RGB(255,255,255)
	dw_2.Object.prpr_codigo.BackGround.Color = RGB(255,255,255)
	dw_2.Object.prcc_codigo.BackGround.Color = RGB(255,255,255)
	dw_2.Object.vari_codigo.BackGround.Color = RGB(255,255,255)
Else
	dw_2.Object.espe_codigo.Protect 					= 1
	dw_2.Object.vari_codigo.Protect 					= 1
	dw_2.Object.prod_codigo.Protect 					= 1
	dw_2.Object.prpr_codigo.Protect 					= 1
	dw_2.Object.prcc_codigo.Protect 					= 1
	dw_2.Object.todosvari.Protect 						= 1
	dw_2.Object.todosCuartel.Protect 					= 1
	
	dw_2.Object.espe_codigo.BackGround.Color	= 553846127
	dw_2.Object.vari_codigo.BackGround.Color		= 553846127
	dw_2.Object.prod_codigo.BackGround.Color		= 553846127
	dw_2.Object.prpr_codigo.BackGround.Color		= 553846127
	dw_2.Object.prcc_codigo.BackGround.Color		= 553846127
	
	dw_2.Object.espe_codigo.Color					= RGB(255,255,255)
	dw_2.Object.vari_codigo.Color						= RGB(255,255,255)
	dw_2.Object.prod_codigo.Color					= RGB(255,255,255)
	dw_2.Object.prpr_codigo.Color						= RGB(255,255,255)
	dw_2.Object.prcc_codigo.Color						= RGB(255,255,255)
End If	
end subroutine

public subroutine totalcosecha (integer ai_fila);Integer	li_productor, li_especie, li_variedad, li_temporada, li_planta, li_agronomo, li_semana, li_semana_inicio, li_Calidad, li_Cuartel
Long	  	ll_predio, ll_TotaReal
Date		ld_Fecha
String		ls_Calibre

li_semana	 	=	dw_2.Object.nume_semana[1]
li_productor 	=	dw_1.Object.prod_codigo[ai_fila]	
li_especie   		=	dw_1.Object.espe_codigo[ai_fila]
li_variedad  		=	dw_1.Object.vari_codigo[ai_fila]
li_Calidad  		=	dw_1.Object.cacm_codigo[ai_fila]
li_temporada	=	gstr_tempo.temporada
ll_predio	 		=	dw_1.Object.prpr_codigo[ai_fila]
li_Cuartel		=	dw_1.Object.prcc_codigo[ai_fila]
ls_Calibre		=	dw_1.Object.vaca_calibr[ai_fila]
li_planta			=	dw_1.Object.plde_codigo[ai_fila]
li_agronomo  	=	dw_1.Object.agro_codigo[ai_fila]
ld_Fecha			=	Date(dw_2.Object.fech_lunsem[1])

/**/
SELECT SUM(esps_cadia1 + esps_cadia2 + esps_cadia3 + esps_cadia4 + esps_cadia5 + esps_cadia6 + esps_cadia7)
       INTO :ll_TotaReal
  FROM dbo.estimprodsemana
  WHERE pate_tempor	= :li_temporada
    AND agro_codigo  		= :li_agronomo
	 AND prod_codigo  	= :li_productor
	 AND prpr_codigo		= :ll_predio
	 And prcc_codigo		= :li_cuartel
	 And vaca_calibr		= :ls_Calibre
	 AND plde_codigo  	= :li_planta
	 AND espe_codigo  	= :li_especie
	 AND vari_codigo  		= :li_variedad
	 AND cacm_codigo 	= :li_Calidad
	 And esps_fecini 		< :ld_Fecha;

IF sqlca.sqlcode = -1 THEN
	F_errorBaseDatos(sqlca,"Lectura de Tabla estimación producción semana")
	ll_totaReal = 0
END IF		

IF isnull(ll_totaReal)  THEN ll_TotaReal = 0

dw_1.Object.esps_cajcos[ai_fila] = ll_TotaReal
end subroutine

public function boolean wf_fechaingreso (date ad_fecha);Boolean	lb_Retorno = True
Integer	li_Fila
String		ls_Columna


If dw_2.AcceptText() = -1 Then lb_Retorno = False

For li_Fila = 0 To 22
	If li_Fila >= 0 And li_Fila <= 6 Then 
		ls_Columna = 'Cajas_Dia0' + String(li_Fila + 1 - 0)
	End If
	
	If li_Fila >= 7 And li_Fila <= 13 Then 
		ls_Columna = 'Cajas_Dia1' + String(li_Fila + 1 - 7)
	End If
	
	If li_Fila >= 14 And li_Fila <= 20 Then 
		ls_Columna = 'Cajas_Dia2' + String(li_Fila + 1 - 14)
	End If
	
	If li_Fila >= 21 And li_Fila <= 21 Then 
		ls_Columna = 'Cajas_Sem04'
	End If

	If li_Fila >= 22 And li_Fila <= 22 Then 
		ls_Columna = 'Cajas_Sem05'
		li_Fila = li_Fila + 6
	End If
	
//	If RelativeDate(ad_Fecha, li_Fila)  <= Date(dw_2.Object.fech_actual[1]) Then
//		dw_1.Modify(ls_Columna + ".Protect = 1")
//	Else
//		dw_1.Modify(ls_Columna + ".Protect = 0")
//	End If
Next

Return lb_Retorno
end function

protected function boolean wf_actualiza_db ();Boolean	lb_AutoCommit, lb_Retorno
DateTime	ldt_FechaHora

ldt_FechaHora		=	F_FechaHora()
dw_4.GrupoFecha	=	ldt_FechaHora

IF Not dw_4.uf_check_required(0) THEN RETURN False
IF Not dw_4.uf_validate(0) THEN RETURN False

lb_AutoCommit		=	sqlca.AutoCommit
sqlca.AutoCommit	=	False

IF dw_4.Update(True, False) = 1 then 
	Commit;
	
	IF sqlca.SQLCode <> 0 THEN
		F_ErrorBaseDatos(sqlca, This.Title)
		lb_Retorno	=	False
	ELSE
		lb_Retorno	=	True
			
		dw_4.ResetUpdate()
	END IF
ELSE
	RollBack;
	
	IF sqlca.SQLCode <> 0 THEN F_ErrorBaseDatos(sqlca, This.Title)
	
	lb_Retorno	=	False
END IF

sqlca.AutoCommit	=	lb_AutoCommit

RETURN lb_Retorno
end function

public subroutine wf_cargaestimacion ();Long	ll_Fila, ll_New, ll_Busca, ll_Semana, ll_Cajas[]
String	ls_Busca

If dw_1.RowCount() = 0 Then Return

dw_4.SetTransObject(Sqlca)
dw_4.Retrieve()

For ll_Fila = 1 To dw_1.RowCount()
	/*Primera Semana*/	
	If (dw_1.Object.esps_semana[ll_Fila] + 0) > F_NroSemanaAno(Date(String(gstr_Tempo.Temporada) +'0101')) Then
		ll_Semana = ((dw_1.Object.esps_semana[ll_Fila] + 0) - F_NroSemanaAno(Date(String(gstr_Tempo.Temporada) +'0101')))
	Else
		ll_Semana = dw_1.Object.esps_semana[ll_Fila] + 0
	End If

	ls_Busca = 'esps_semana = ' + String(ll_Semana) + ' And pate_tempor = ' + String(dw_1.Object.pate_tempor[ll_Fila]) + &
			' And agro_codigo = ' + String(dw_1.Object.agro_codigo[ll_Fila]) + ' And prod_codigo = ' + String(dw_1.Object.prod_codigo[ll_Fila]) + &
			' And prpr_codigo  = ' + String(dw_1.Object.prpr_codigo[ll_Fila]) + ' And prcc_codigo = ' + String(dw_1.Object.prcc_codigo[ll_Fila]) + &
			' And plde_codigo = ' + String(dw_1.Object.plde_codigo[ll_Fila]) + &
			' And espe_codigo = ' + String(dw_1.Object.espe_codigo[ll_Fila]) + ' And vari_codigo = ' + String(dw_1.Object.vari_codigo[ll_Fila]) + &
			' And cacm_codigo = ' + String(dw_1.Object.cacm_codigo[ll_Fila]) + ' And vaca_calibr = "' + String(dw_1.Object.vaca_calibr[ll_Fila]) + '"'
			
	ll_Busca = dw_4.Find(ls_Busca, 1, dw_4.RowCount())
	
	If ll_Busca > 0 Then
		dw_4.Object.esps_cadia1[ll_Busca] = dw_1.Object.Cajas_Dia01[ll_Fila]
		dw_4.Object.esps_cadia2[ll_Busca] = dw_1.Object.Cajas_Dia02[ll_Fila]
		dw_4.Object.esps_cadia3[ll_Busca] = dw_1.Object.Cajas_Dia03[ll_Fila]
		dw_4.Object.esps_cadia4[ll_Busca] = dw_1.Object.Cajas_Dia04[ll_Fila]
		dw_4.Object.esps_cadia5[ll_Busca] = dw_1.Object.Cajas_Dia05[ll_Fila]
		dw_4.Object.esps_cadia6[ll_Busca] = dw_1.Object.Cajas_Dia06[ll_Fila]
		dw_4.Object.esps_cadia7[ll_Busca] = dw_1.Object.Cajas_Dia07[ll_Fila]
	Else
		ll_New	=	dw_4.InsertRow(0)
		dw_4.Object.esps_semana[ll_New]	= ll_Semana
		dw_4.Object.pate_tempor[ll_New]		= dw_1.Object.pate_tempor[ll_Fila]
		dw_4.Object.agro_codigo[ll_New]		= dw_1.Object.agro_codigo[ll_Fila]
		dw_4.Object.prod_codigo[ll_New]		= dw_1.Object.prod_codigo[ll_Fila]
		dw_4.Object.prpr_codigo[ll_New]		= dw_1.Object.prpr_codigo[ll_Fila]
		dw_4.Object.plde_codigo[ll_New]		= dw_1.Object.plde_codigo[ll_Fila]
		dw_4.Object.espe_codigo[ll_New]		= dw_1.Object.espe_codigo[ll_Fila]
		dw_4.Object.vari_codigo[ll_New]		= dw_1.Object.vari_codigo[ll_Fila]
		dw_4.Object.cacm_codigo[ll_New]	= dw_1.Object.cacm_codigo[ll_Fila]
		dw_4.Object.vaca_calibr[ll_New]		= dw_1.Object.vaca_calibr[ll_Fila]
		dw_4.Object.prcc_codigo[ll_New]		= dw_1.Object.prcc_codigo[ll_Fila]

		dw_4.Object.esps_fecini[ll_New]		= dw_1.Object.Fecha_Dia01[ll_Fila]
		dw_4.Object.esps_cadia1[ll_New] = dw_1.Object.Cajas_Dia01[ll_Fila]
		dw_4.Object.esps_cadia2[ll_New] = dw_1.Object.Cajas_Dia02[ll_Fila]
		dw_4.Object.esps_cadia3[ll_New] = dw_1.Object.Cajas_Dia03[ll_Fila]
		dw_4.Object.esps_cadia4[ll_New] = dw_1.Object.Cajas_Dia04[ll_Fila]
		dw_4.Object.esps_cadia5[ll_New] = dw_1.Object.Cajas_Dia05[ll_Fila]
		dw_4.Object.esps_cadia6[ll_New] = dw_1.Object.Cajas_Dia06[ll_Fila]
		dw_4.Object.esps_cadia7[ll_New] = dw_1.Object.Cajas_Dia07[ll_Fila]
	End If
	/*Segunda Semana*/
	If (dw_1.Object.esps_semana[ll_Fila] + 1) > F_NroSemanaAno(Date(String(gstr_Tempo.Temporada) +'0101')) Then
		ll_Semana = ((dw_1.Object.esps_semana[ll_Fila] + 1) - F_NroSemanaAno(Date(String(gstr_Tempo.Temporada) +'0101')))
	Else
		ll_Semana = dw_1.Object.esps_semana[ll_Fila] + 1
	End If
	ls_Busca = 'esps_semana = ' + String(ll_Semana) + ' And pate_tempor = ' + String(dw_1.Object.pate_tempor[ll_Fila]) + &
			' And agro_codigo = ' + String(dw_1.Object.agro_codigo[ll_Fila]) + ' And prod_codigo = ' + String(dw_1.Object.prod_codigo[ll_Fila]) + &
			' And prpr_codigo  = ' + String(dw_1.Object.prpr_codigo[ll_Fila]) + ' And prcc_codigo = ' + String(dw_1.Object.prcc_codigo[ll_Fila]) + &
			' And plde_codigo = ' + String(dw_1.Object.plde_codigo[ll_Fila]) + &
			' And espe_codigo = ' + String(dw_1.Object.espe_codigo[ll_Fila]) + ' And vari_codigo = ' + String(dw_1.Object.vari_codigo[ll_Fila]) + &
			' And cacm_codigo = ' + String(dw_1.Object.cacm_codigo[ll_Fila]) + ' And vaca_calibr = "' + String(dw_1.Object.vaca_calibr[ll_Fila]) + '"'
			
	ll_Busca = dw_4.Find(ls_Busca, 1, dw_4.RowCount())
	
	If ll_Busca > 0 Then
		dw_4.Object.esps_cadia1[ll_Busca] = dw_1.Object.Cajas_Dia11[ll_Fila]
		dw_4.Object.esps_cadia2[ll_Busca] = dw_1.Object.Cajas_Dia12[ll_Fila]
		dw_4.Object.esps_cadia3[ll_Busca] = dw_1.Object.Cajas_Dia13[ll_Fila]
		dw_4.Object.esps_cadia4[ll_Busca] = dw_1.Object.Cajas_Dia14[ll_Fila]
		dw_4.Object.esps_cadia5[ll_Busca] = dw_1.Object.Cajas_Dia15[ll_Fila]
		dw_4.Object.esps_cadia6[ll_Busca] = dw_1.Object.Cajas_Dia16[ll_Fila]
		dw_4.Object.esps_cadia7[ll_Busca] = dw_1.Object.Cajas_Dia17[ll_Fila]
	Else
		ll_New	=	dw_4.InsertRow(0)
		dw_4.Object.esps_semana[ll_New]	= ll_Semana
		dw_4.Object.pate_tempor[ll_New]		= dw_1.Object.pate_tempor[ll_Fila]
		dw_4.Object.agro_codigo[ll_New]		= dw_1.Object.agro_codigo[ll_Fila]
		dw_4.Object.prod_codigo[ll_New]		= dw_1.Object.prod_codigo[ll_Fila]
		dw_4.Object.prpr_codigo[ll_New]		= dw_1.Object.prpr_codigo[ll_Fila]
		dw_4.Object.plde_codigo[ll_New]		= dw_1.Object.plde_codigo[ll_Fila]
		dw_4.Object.espe_codigo[ll_New]		= dw_1.Object.espe_codigo[ll_Fila]
		dw_4.Object.vari_codigo[ll_New]		= dw_1.Object.vari_codigo[ll_Fila]
		dw_4.Object.cacm_codigo[ll_New]	= dw_1.Object.cacm_codigo[ll_Fila]
		dw_4.Object.vaca_calibr[ll_New]		= dw_1.Object.vaca_calibr[ll_Fila]
		dw_4.Object.prcc_codigo[ll_New]		= dw_1.Object.prcc_codigo[ll_Fila]
		
		dw_4.Object.esps_fecini[ll_New]	= dw_1.Object.Fecha_Dia11[ll_Fila]
		dw_4.Object.esps_cadia1[ll_New] = dw_1.Object.Cajas_Dia11[ll_Fila]
		dw_4.Object.esps_cadia2[ll_New] = dw_1.Object.Cajas_Dia12[ll_Fila]
		dw_4.Object.esps_cadia3[ll_New] = dw_1.Object.Cajas_Dia13[ll_Fila]
		dw_4.Object.esps_cadia4[ll_New] = dw_1.Object.Cajas_Dia14[ll_Fila]
		dw_4.Object.esps_cadia5[ll_New] = dw_1.Object.Cajas_Dia15[ll_Fila]
		dw_4.Object.esps_cadia6[ll_New] = dw_1.Object.Cajas_Dia16[ll_Fila]
		dw_4.Object.esps_cadia7[ll_New] = dw_1.Object.Cajas_Dia17[ll_Fila]
		
	End If
	/*Tercera*/
	If (dw_1.Object.esps_semana[ll_Fila] + 2) > F_NroSemanaAno(Date(String(gstr_Tempo.Temporada) +'0101')) Then
		ll_Semana = ((dw_1.Object.esps_semana[ll_Fila] + 2) - F_NroSemanaAno(Date(String(gstr_Tempo.Temporada) +'0101')))
	Else
		ll_Semana = dw_1.Object.esps_semana[ll_Fila] + 2
	End If
	
	ls_Busca = 'esps_semana = ' + String(ll_Semana) + ' And pate_tempor = ' + String(dw_1.Object.pate_tempor[ll_Fila]) + &
			' And agro_codigo = ' + String(dw_1.Object.agro_codigo[ll_Fila]) + ' And prod_codigo = ' + String(dw_1.Object.prod_codigo[ll_Fila]) + &
			' And prpr_codigo  = ' + String(dw_1.Object.prpr_codigo[ll_Fila]) + ' And prcc_codigo = ' + String(dw_1.Object.prcc_codigo[ll_Fila]) + &
			' And plde_codigo = ' + String(dw_1.Object.plde_codigo[ll_Fila]) + &
			' And espe_codigo = ' + String(dw_1.Object.espe_codigo[ll_Fila]) + ' And vari_codigo = ' + String(dw_1.Object.vari_codigo[ll_Fila]) + &
			' And cacm_codigo = ' + String(dw_1.Object.cacm_codigo[ll_Fila]) + ' And vaca_calibr = "' + String(dw_1.Object.vaca_calibr[ll_Fila]) + '"'
			
	ll_Busca = dw_4.Find(ls_Busca, 1, dw_4.RowCount())
	
	wf_DistribucionDiaria(dw_1.Object.Cajas_Sem03[ll_Fila], 0, ll_Cajas[])
	
	If ll_Busca > 0 Then
		dw_4.Object.esps_cadia1[ll_Busca] = ll_Cajas[1]
		dw_4.Object.esps_cadia2[ll_Busca] = ll_Cajas[2]
		dw_4.Object.esps_cadia3[ll_Busca] = ll_Cajas[3]
		dw_4.Object.esps_cadia4[ll_Busca] = ll_Cajas[4]
		dw_4.Object.esps_cadia5[ll_Busca] = ll_Cajas[5]
		dw_4.Object.esps_cadia6[ll_Busca] = ll_Cajas[6]
		dw_4.Object.esps_cadia7[ll_Busca] = ll_Cajas[7]
	Else
		ll_New	=	dw_4.InsertRow(0)
		dw_4.Object.esps_semana[ll_New]	= ll_Semana
		dw_4.Object.pate_tempor[ll_New]		= dw_1.Object.pate_tempor[ll_Fila]
		dw_4.Object.agro_codigo[ll_New]		= dw_1.Object.agro_codigo[ll_Fila]
		dw_4.Object.prod_codigo[ll_New]		= dw_1.Object.prod_codigo[ll_Fila]
		dw_4.Object.prpr_codigo[ll_New]		= dw_1.Object.prpr_codigo[ll_Fila]
		dw_4.Object.plde_codigo[ll_New]		= dw_1.Object.plde_codigo[ll_Fila]
		dw_4.Object.espe_codigo[ll_New]		= dw_1.Object.espe_codigo[ll_Fila]
		dw_4.Object.vari_codigo[ll_New]		= dw_1.Object.vari_codigo[ll_Fila]
		dw_4.Object.cacm_codigo[ll_New]	= dw_1.Object.cacm_codigo[ll_Fila]
		dw_4.Object.vaca_calibr[ll_New]		= dw_1.Object.vaca_calibr[ll_Fila]
		dw_4.Object.prcc_codigo[ll_New]		= dw_1.Object.prcc_codigo[ll_Fila]
		
		dw_4.Object.esps_fecini[ll_New]	= dw_1.Object.Fecha_Sem03[ll_Fila]
		dw_4.Object.esps_cadia1[ll_New] = ll_Cajas[1]
		dw_4.Object.esps_cadia2[ll_New] = ll_Cajas[2]
		dw_4.Object.esps_cadia3[ll_New] = ll_Cajas[3]
		dw_4.Object.esps_cadia4[ll_New] = ll_Cajas[4]
		dw_4.Object.esps_cadia5[ll_New] = ll_Cajas[5]
		dw_4.Object.esps_cadia6[ll_New] = ll_Cajas[6]
		dw_4.Object.esps_cadia7[ll_New] = ll_Cajas[7]
	End If
	
	/*Cuarta*/
	If (dw_1.Object.esps_semana[ll_Fila] + 3) > F_NroSemanaAno(Date(String(gstr_Tempo.Temporada) +'0101')) Then
		ll_Semana = ((dw_1.Object.esps_semana[ll_Fila] + 3) - F_NroSemanaAno(Date(String(gstr_Tempo.Temporada) +'0101')))
	Else
		ll_Semana = dw_1.Object.esps_semana[ll_Fila] + 3
	End If
	
	ls_Busca = 'esps_semana = ' + String(ll_Semana) + ' And pate_tempor = ' + String(dw_1.Object.pate_tempor[ll_Fila]) + &
			' And agro_codigo = ' + String(dw_1.Object.agro_codigo[ll_Fila]) + ' And prod_codigo = ' + String(dw_1.Object.prod_codigo[ll_Fila]) + &
			' And prpr_codigo  = ' + String(dw_1.Object.prpr_codigo[ll_Fila]) + ' And prcc_codigo = ' + String(dw_1.Object.prcc_codigo[ll_Fila]) + &
			' And plde_codigo = ' + String(dw_1.Object.plde_codigo[ll_Fila]) + &
			' And espe_codigo = ' + String(dw_1.Object.espe_codigo[ll_Fila]) + ' And vari_codigo = ' + String(dw_1.Object.vari_codigo[ll_Fila]) + &
			' And cacm_codigo = ' + String(dw_1.Object.cacm_codigo[ll_Fila]) + ' And vaca_calibr = "' + String(dw_1.Object.vaca_calibr[ll_Fila]) + '"'
			
	ll_Busca = dw_4.Find(ls_Busca, 1, dw_4.RowCount())
	
	wf_DistribucionDiaria(dw_1.Object.Cajas_Sem04[ll_Fila], 0, ll_Cajas[])
	
	If ll_Busca > 0 Then
		dw_4.Object.esps_cadia1[ll_Busca] = ll_Cajas[1]
		dw_4.Object.esps_cadia2[ll_Busca] = ll_Cajas[2]
		dw_4.Object.esps_cadia3[ll_Busca] = ll_Cajas[3]
		dw_4.Object.esps_cadia4[ll_Busca] = ll_Cajas[4]
		dw_4.Object.esps_cadia5[ll_Busca] = ll_Cajas[5]
		dw_4.Object.esps_cadia6[ll_Busca] = ll_Cajas[6]
		dw_4.Object.esps_cadia7[ll_Busca] = ll_Cajas[7]
	Else
		ll_New	=	dw_4.InsertRow(0)
		dw_4.Object.esps_semana[ll_New]	= ll_Semana
		dw_4.Object.pate_tempor[ll_New]		= dw_1.Object.pate_tempor[ll_Fila]
		dw_4.Object.agro_codigo[ll_New]		= dw_1.Object.agro_codigo[ll_Fila]
		dw_4.Object.prod_codigo[ll_New]		= dw_1.Object.prod_codigo[ll_Fila]
		dw_4.Object.prpr_codigo[ll_New]		= dw_1.Object.prpr_codigo[ll_Fila]
		dw_4.Object.plde_codigo[ll_New]		= dw_1.Object.plde_codigo[ll_Fila]
		dw_4.Object.espe_codigo[ll_New]		= dw_1.Object.espe_codigo[ll_Fila]
		dw_4.Object.vari_codigo[ll_New]		= dw_1.Object.vari_codigo[ll_Fila]
		dw_4.Object.cacm_codigo[ll_New]	= dw_1.Object.cacm_codigo[ll_Fila]
		dw_4.Object.vaca_calibr[ll_New]		= dw_1.Object.vaca_calibr[ll_Fila]
		dw_4.Object.prcc_codigo[ll_New]		= dw_1.Object.prcc_codigo[ll_Fila]
		
		dw_4.Object.esps_fecini[ll_New]	= dw_1.Object.Fecha_Sem04[ll_Fila]
		dw_4.Object.esps_cadia1[ll_New] = ll_Cajas[1]
		dw_4.Object.esps_cadia2[ll_New] = ll_Cajas[2]
		dw_4.Object.esps_cadia3[ll_New] = ll_Cajas[3]
		dw_4.Object.esps_cadia4[ll_New] = ll_Cajas[4]
		dw_4.Object.esps_cadia5[ll_New] = ll_Cajas[5]
		dw_4.Object.esps_cadia6[ll_New] = ll_Cajas[6]
		dw_4.Object.esps_cadia7[ll_New] = ll_Cajas[7]
	End If
	/*Quinta*/
	If (dw_1.Object.esps_semana[ll_Fila] + 4) > F_NroSemanaAno(Date(String(gstr_Tempo.Temporada) +'0101')) Then
		ll_Semana = ((dw_1.Object.esps_semana[ll_Fila] + 4) - F_NroSemanaAno(Date(String(gstr_Tempo.Temporada) +'0101')))
	Else
		ll_Semana = dw_1.Object.esps_semana[ll_Fila] + 4
	End If
	
	ls_Busca = 'esps_semana = ' + String(ll_Semana) + ' And pate_tempor = ' + String(dw_1.Object.pate_tempor[ll_Fila]) + &
			' And agro_codigo = ' + String(dw_1.Object.agro_codigo[ll_Fila]) + ' And prod_codigo = ' + String(dw_1.Object.prod_codigo[ll_Fila]) + &
			' And prpr_codigo  = ' + String(dw_1.Object.prpr_codigo[ll_Fila]) + ' And prcc_codigo = ' + String(dw_1.Object.prcc_codigo[ll_Fila]) + &
			' And plde_codigo = ' + String(dw_1.Object.plde_codigo[ll_Fila]) + &
			' And espe_codigo = ' + String(dw_1.Object.espe_codigo[ll_Fila]) + ' And vari_codigo = ' + String(dw_1.Object.vari_codigo[ll_Fila]) + &
			' And cacm_codigo = ' + String(dw_1.Object.cacm_codigo[ll_Fila]) + ' And vaca_calibr = "' + String(dw_1.Object.vaca_calibr[ll_Fila]) + '"'
			
	ll_Busca = dw_4.Find(ls_Busca, 1, dw_4.RowCount())
	wf_DistribucionDiaria(dw_1.Object.Cajas_Sem05[ll_Fila], 0, ll_Cajas[])
	
	If ll_Busca > 0 Then
		dw_4.Object.esps_cadia1[ll_Busca] = ll_Cajas[1]
		dw_4.Object.esps_cadia2[ll_Busca] = ll_Cajas[2]
		dw_4.Object.esps_cadia3[ll_Busca] = ll_Cajas[3]
		dw_4.Object.esps_cadia4[ll_Busca] = ll_Cajas[4]
		dw_4.Object.esps_cadia5[ll_Busca] = ll_Cajas[5]
		dw_4.Object.esps_cadia6[ll_Busca] = ll_Cajas[6]
		dw_4.Object.esps_cadia7[ll_Busca] = ll_Cajas[7]
	Else
		ll_New	=	dw_4.InsertRow(0)
		dw_4.Object.esps_semana[ll_New]	= ll_Semana
		dw_4.Object.pate_tempor[ll_New]		= dw_1.Object.pate_tempor[ll_Fila]
		dw_4.Object.agro_codigo[ll_New]		= dw_1.Object.agro_codigo[ll_Fila]
		dw_4.Object.prod_codigo[ll_New]		= dw_1.Object.prod_codigo[ll_Fila]
		dw_4.Object.prpr_codigo[ll_New]		= dw_1.Object.prpr_codigo[ll_Fila]
		dw_4.Object.plde_codigo[ll_New]		= dw_1.Object.plde_codigo[ll_Fila]
		dw_4.Object.espe_codigo[ll_New]		= dw_1.Object.espe_codigo[ll_Fila]
		dw_4.Object.vari_codigo[ll_New]		= dw_1.Object.vari_codigo[ll_Fila]
		dw_4.Object.cacm_codigo[ll_New]	= dw_1.Object.cacm_codigo[ll_Fila]
		dw_4.Object.vaca_calibr[ll_New]		= dw_1.Object.vaca_calibr[ll_Fila]
		dw_4.Object.prcc_codigo[ll_New]		= dw_1.Object.prcc_codigo[ll_Fila]
		
		dw_4.Object.esps_fecini[ll_New]	= dw_1.Object.Fecha_Sem05[ll_Fila]
		dw_4.Object.esps_cadia1[ll_New] = ll_Cajas[1]
		dw_4.Object.esps_cadia2[ll_New] = ll_Cajas[2]
		dw_4.Object.esps_cadia3[ll_New] = ll_Cajas[3]
		dw_4.Object.esps_cadia4[ll_New] = ll_Cajas[4]
		dw_4.Object.esps_cadia5[ll_New] = ll_Cajas[5]
		dw_4.Object.esps_cadia6[ll_New] = ll_Cajas[6]
		dw_4.Object.esps_cadia7[ll_New] = ll_Cajas[7]
	End If
Next

Return

end subroutine

public subroutine wf_bloqueacolumnas (boolean ab_bloquea);//
end subroutine

public subroutine wf_distribuciondiaria (long total, integer dias, ref long resultado[]);Long	ll_Valor = 0, ll_Diferencia = 0, ll_Fila

If IsNull(Dias) Or Dias = 0 Then Dias = 1

ll_Valor		= Total / Dias
ll_Diferencia	= Total - (ll_Valor * Dias)

For ll_Fila = 1 To Dias
	Resultado[ll_Fila] = ll_Valor
	If ll_Fila = Dias Then Resultado[ll_Fila] = ll_Valor + ll_Diferencia
Next

For ll_Fila = UpperBound(Resultado) + 1 To 7
	Resultado[ll_Fila] = 0
Next 

Return
end subroutine

on w_mant_mues_flujocosechasemanal_packing.create
int iCurrent
call super::create
this.dw_3=create dw_3
this.dw_4=create dw_4
this.dw_2=create dw_2
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_3
this.Control[iCurrent+2]=this.dw_4
this.Control[iCurrent+3]=this.dw_2
end on

on w_mant_mues_flujocosechasemanal_packing.destroy
call super::destroy
destroy(this.dw_3)
destroy(this.dw_4)
destroy(this.dw_2)
end on

event open;call super::open;dw_2.Getchild("prod_codigo", idwc_Productores)
idwc_productores.SetTransObject(SQLCA)
If idwc_productores.Retrieve(-1) = 0 Then	idwc_productores.InsertRow(0)

dw_2.GetChild('prpr_codigo', idwc_predio)
idwc_predio.SetTransObject(SqlCa)
idwc_Predio.Retrieve(-1)

dw_2.GetChild('prcc_codigo', idwc_Cuartel)
idwc_Cuartel.SetTransObject(Sqlca)
idwc_Cuartel.Retrieve(-1, -1)

dw_2.Getchild("espe_codigo",idwc_Especie)
idwc_Especie.SetTransObject(SQLCA)
IF idwc_Especie.Retrieve() = 0 THEN idwc_Especie.Insertrow(0)
dw_2.Object.espe_codigo[1] = 11

dw_2.Getchild("vari_codigo",idwc_Variedad)
idwc_Variedad.SetTransObject(SQLCA)
idwc_Variedad.Retrieve(11)
idwc_Variedad.Insertrow(0)

dw_2.SetTransObject(Sqlca)
dw_2.InsertRow(0)

dw_2.Object.fech_actual[1] = Datetime(Date(String(Today(),'dd/mm/yyyy')))

iuo_Especie	 	= 	Create	uo_Especie
iuo_Variedad	=	Create	uo_Variedades
iuo_Productor	=	Create	uo_Productores
iuo_Predio		=	Create	uo_ProdPredio
iuo_Cuartel		=	Create	uo_ProdCuarteles
iuo_Semana		=	Create	uo_NroSemana

iuo_Variedad.Variedad	=	-1
iuo_Cuartel.Cuartel		=	-1
iuo_Especie.Codigo		=	11

is_ultimacol = 'nume_semana'
end event

event ue_guardar;IF dw_1.AcceptText() = -1 THEN RETURN

SetPointer(HourGlass!)

w_main.SetMicroHelp("Grabando información...")

message.doubleparm = 0

TriggerEvent("ue_antesguardar")

If Message.DoubleParm = -1 Then Return
wf_CargaEstimacion()

If wf_actualiza_db() Then
	w_main.SetMicroHelp("Información Grabada.")
	pb_imprimir.Enabled	= True
	gs_graba = 'Grabado'
Else
	w_main.SetMicroHelp("No se puede Grabar información.")
	Message.DoubleParm = -1
	Return 
End If
end event

event resize;call super::resize;dw_2.x = ((This.WorkSpaceWidth() - 351) / 2) - (dw_2.Width / 2)
end event

type st_encabe from w_mant_directo`st_encabe within w_mant_mues_flujocosechasemanal_packing
boolean visible = false
integer x = 9
integer y = 4
integer width = 87
integer height = 68
end type

type pb_nuevo from w_mant_directo`pb_nuevo within w_mant_mues_flujocosechasemanal_packing
integer x = 3616
integer taborder = 60
end type

event pb_nuevo::clicked;call super::clicked;String	ls_Null

SetNull(ls_Null)
pb_grabar.Enabled 	= False

dw_2.Object.prod_codigo[1]	= Integer(ls_Null)
dw_2.Object.prpr_codigo[1] 	= Integer(ls_Null)
dw_2.Object.prcc_codigo[1] 	= Integer(ls_Null)
dw_2.Object.espe_codigo[1] 	= Integer(ls_Null)
dw_2.Object.vari_codigo[1] 	= Integer(ls_Null)
dw_2.Object.nume_semana[1] = Integer(ls_Null)
dw_2.Object.todoscuartel[1] 	= 1
dw_2.Object.todosvari[1] 		= 1

HabilitaEncab(True)

end event

type pb_lectura from w_mant_directo`pb_lectura within w_mant_mues_flujocosechasemanal_packing
boolean visible = false
integer x = 3611
end type

type pb_eliminar from w_mant_directo`pb_eliminar within w_mant_mues_flujocosechasemanal_packing
boolean visible = false
integer x = 3611
integer taborder = 80
end type

type pb_insertar from w_mant_directo`pb_insertar within w_mant_mues_flujocosechasemanal_packing
boolean visible = false
integer x = 3611
integer taborder = 70
end type

type pb_salir from w_mant_directo`pb_salir within w_mant_mues_flujocosechasemanal_packing
integer x = 3611
integer taborder = 110
end type

type pb_imprimir from w_mant_directo`pb_imprimir within w_mant_mues_flujocosechasemanal_packing
boolean visible = false
integer x = 3611
integer taborder = 100
end type

type pb_grabar from w_mant_directo`pb_grabar within w_mant_mues_flujocosechasemanal_packing
integer x = 3611
integer taborder = 90
boolean enabled = true
end type

type dw_1 from w_mant_directo`dw_1 within w_mant_mues_flujocosechasemanal_packing
integer x = 87
integer y = 628
integer width = 3451
integer height = 1152
integer taborder = 50
string dataobject = "dw_gene_pentasemanal"
boolean hscrollbar = true
boolean hsplitscroll = true
end type

event dw_1::itemchanged;call super::itemchanged;String ls_columna, ls_Null

SetNull(ls_Null)
ls_Columna	=	dwo.Name

Choose Case ls_Columna
	Case ''

END CHOOSE
end event

event dw_1::itemerror;RETURN 1
end event

event dw_1::clicked;call super::clicked;String	ls_old_sort, ls_column, ls_color_old
Char		lc_sort

If IsNull(dwo) Then Return

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
	Else
		This.SetSort(ls_column+" A")
		This.Modify(Left(ls_old_sort, Len(ls_old_sort) - 2) + "_t.Color = " + ls_color_old)
	End If
	
	This.Modify(dwo.Name + ".Color = " + String(RGB(255,255,0)))
	
	This.Sort()
End If

end event

type dw_3 from datawindow within w_mant_mues_flujocosechasemanal_packing
boolean visible = false
integer x = 87
integer y = 1468
integer width = 361
integer height = 284
integer taborder = 110
boolean bringtotop = true
string title = "none"
string dataobject = "dw_carga_pronosticopacking"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_4 from uo_dw within w_mant_mues_flujocosechasemanal_packing
boolean visible = false
integer x = 466
integer y = 1468
integer width = 361
integer height = 284
integer taborder = 11
boolean bringtotop = true
string dataobject = "dw_carga_estimprodsemana"
end type

type dw_2 from uo_dw within w_mant_mues_flujocosechasemanal_packing
integer x = 155
integer y = 60
integer width = 3008
integer height = 560
integer taborder = 11
string dataobject = "dw_mant_flujocosechaenca"
boolean vscrollbar = false
boolean border = false
end type

event itemchanged;call super::itemchanged;Integer	li_Semana, li_dias, li_SemAno
String		ls_columna, ls_Null
Long		ll_Fila

SetNull(ls_Null)

ls_Columna	=	dwo.Name

Choose Case ls_Columna
	Case "prod_codigo"
		If Not iuo_Productor.Existe(Long(Data), True, SQLCA) Then
			This.SetItem(Row, ls_Columna, Integer(ls_Null))
		   	Return 1
		Else
			dw_2.GetChild('prpr_codigo', idwc_predio)
			idwc_predio.SetTransObject(SqlCa)
			idwc_Predio.Retrieve(iuo_Productor.Codigo)
			
			dw_2.GetChild('prcc_codigo', idwc_Cuartel)
			idwc_Cuartel.SetTransObject(Sqlca)
			idwc_Cuartel.Retrieve(iuo_Productor.Codigo, -1)
		End If
		
	Case "prpr_codigo"
		If Not iuo_Predio.Existe(Long(Data), iuo_Productor.Codigo, True, SQLCA) Then
			This.SetItem(Row, ls_Columna, Integer(ls_Null))
			Return 1
		Else			
			dw_2.GetChild('prcc_codigo', idwc_Cuartel)
			idwc_Cuartel.SetTransObject(Sqlca)
			idwc_Cuartel.Retrieve(iuo_Productor.Codigo, iuo_Predio.Codigo)
		End If
		
	Case "prcc_codigo"
		If Not iuo_Cuartel.Existe(iuo_Productor.Codigo, iuo_Predio.Codigo, Long(Data), True, SQLCA) Then
			This.SetItem(Row, ls_Columna, Integer(ls_Null))
			Return 1
		Else			
			This.Object.espe_codigo[Row]	= iuo_Cuartel.Especie
			This.Object.vari_codigo[Row]	= iuo_Cuartel.Variedad
			This.Object.TodosVari[Row]	= 0
			This.Object.vari_codigo.Protect	= 0
		End If
		
	Case "todoscuartel"
		If Data = '1' Then
			This.SetItem(Row, 'prcc_codigo', Integer(ls_Null))
			this.Object.prcc_codigo.Protect  = 1
			iuo_Cuartel.Cuartel				 = -1
		ELSE
			this.Object.prcc_codigo.Protect  = 0
		End If
		
	Case "espe_codigo"
		If Not iuo_Especie.Existe(integer(data),True,SQLCA) Then
			This.SetItem(Row, ls_Columna, Integer(ls_Null))
			Return 1
		ELSE
			dw_2.Getchild("vari_codigo",idwc_Variedad)
			idwc_Variedad.SetTransObject(SQLCA)
			If idwc_Variedad.Retrieve(iuo_Especie.Codigo) = 0 Then
				idwc_Variedad.Insertrow(0)
			End If	
		End If
		
	Case "vari_codigo"
		If Not iuo_Variedad.Existe(iuo_Especie.Codigo, Integer(data),True,SQLCA) Then
			This.SetItem(Row, ls_Columna, Integer(ls_Null))
			Return 1
		End If
		
	Case "todosvari"
		If Data = '1' Then
			This.SetItem(Row, 'vari_codigo', Integer(ls_Null))
			this.Object.vari_codigo.Protect  = 1
		ELSE
			this.Object.vari_codigo.Protect  = 0
		End If
		
	Case "nume_semana"		
		If dw_2.Object.todosvari[1] = 1 Then iuo_Variedad.Variedad = -1
		ii_Semana = Integer(data)
		
		If iuo_Semana.Semana(Integer(Data), gstr_tempo.temporada, iuo_Especie.Codigo) Then
			li_Semana	=	F_NroSemanaAno(iuo_Semana.Lunes)
			If Integer(data) = 53 Then
				If 	li_Semana = 52 Then
					This.SetItem(Row, ls_Columna, Integer(ls_Null))
					MessageBox('Alerta', 'Este año no posee semana 53.', Exclamation!, OK!)
					Return 1
				End If
			End If
			
			dw_2.Object.fech_lunsem[1] = Datetime(iuo_Semana.Lunes)
			dw_2.Object.fech_actual[1] = Datetime(iuo_Semana.Fecha)
			
			li_dias =  DaysAfter(iuo_Semana.Lunes,iuo_Semana.Fecha) 
			li_SemAno = F_NroSemanaAno(Date(String(gstr_Tempo.Temporada) +'0101'))
				
			ll_fila	= dw_1.Retrieve(li_SemAno, gstr_tempo.temporada, -1, iuo_Especie.Codigo, iuo_Variedad.Variedad, &
							Integer(Data), iuo_Semana.Lunes, iuo_Productor.Codigo, iuo_Predio.Codigo, iuo_Cuartel.Cuartel)
			
			Habilitaencab(False)
			
			For ll_fila = 1 To dw_1.RowCount()
				TotalCosecha(ll_Fila)
			Next
			wf_fechaingreso(iuo_Semana.Lunes)
		End If
End Choose
end event

event itemerror;call super::itemerror;Return 1
end event

event itemfocuschanged;call super::itemfocuschanged;String	ls_Columna

ls_Columna  = dwo.Name

Choose Case ls_Columna
	Case "nume_semana"
		This.ScrollToRow(1)
		This.SetRow(1)
		
End Choose
end event

