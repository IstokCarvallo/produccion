$PBExportHeader$w_plan_pronostico_cosecha.srw
forward
global type w_plan_pronostico_cosecha from w_mant_directo
end type
type dw_2 from datawindow within w_plan_pronostico_cosecha
end type
type cb_estandar from commandbutton within w_plan_pronostico_cosecha
end type
type cb_produccion from commandbutton within w_plan_pronostico_cosecha
end type
end forward

global type w_plan_pronostico_cosecha from w_mant_directo
integer width = 5339
integer height = 2040
string title = "Ficha Pronóstico"
windowstate windowstate = maximized!
event ue_correo ( )
dw_2 dw_2
cb_estandar cb_estandar
cb_produccion cb_produccion
end type
global w_plan_pronostico_cosecha w_plan_pronostico_cosecha

type variables
uo_Agronomo				iuo_Agronomo
uo_especie          			iuo_especie
uo_variedades       		iuo_variedades
uo_productores		  		iuo_productores
uo_predios					iuo_predios
uo_agronomo_productor	iuo_agroespeprod
uo_nrosemana				iuo_semana
uo_Plantadesp				iuo_Planta

DataWindowChild 			idwc_especie, idwc_variedad, idwc_predio, idwc_productor,idwc_pack, idwc_agro, &
							idwc_cierre, idwc_cacm, idwc_cacm2, idwc_cacm3
					 
Long			il_fila_antes, il_tempo_antes, il_alto_dw1
String			is_nom_agr, is_Archivo,is_Directorio
Decimal{2}	id_pesocos, id_pesostd
Boolean		ib_cambio_seleccion
end variables

forward prototypes
public subroutine buscapeso (integer ai_especie, integer ai_variedad)
public subroutine buscapesovariedad (integer ai_especie, integer ai_variedad)
public subroutine habilitaenca (boolean ab_habilita)
public subroutine buscanombres (integer ssa)
protected function boolean wf_actualiza_db ()
public subroutine asignadatosmedicion (string as_columna, string as_valor)
public subroutine existepronostico (string as_columna, string as_valor)
public subroutine obtienepesocosecha (integer ai_especie, integer ai_variedad)
public function boolean wf_traspasadistribucion ()
public function integer wf_cargacuarteles (long al_productor, integer ai_predio, integer ai_especie, integer ai_variedad)
public function boolean wf_validadistribucionestandar (long al_productor, integer ai_predio)
public function boolean wf_validasemana (datetime ad_fecha, integer ai_semana, integer ai_especie, integer row)
public function boolean wf_validacalibre (long al_productor, integer ai_predio, integer ai_cuartel, integer ai_especie, integer ai_variedad)
public function boolean wf_validacondicion (long al_productor, integer ai_predio, integer ai_cuartel, integer ai_especie, integer ai_variedad)
end prototypes

event ue_correo();Long		ll_Fila
Integer	li_Agronomo
DateTime	ld_Fecha
DataStore	lds_1

ld_Fecha		=	F_FechaHora()
li_Agronomo	=	Integer(istr_mant.argumento[5])

IF is_Archivo <> is_directorio+'\BITPRON'+String(li_Agronomo,'00')+'.psr' THEN
	
	is_Archivo			=	is_directorio+'\BITPRON'+String(li_Agronomo,'00')+'.psr'
	
	lds_1	=	Create DataStore
	
	lds_1.DataObject	=	"dw_info_bitacorapronostico"
	
	lds_1.SetTransObject(SQLCA)
	
	ll_fila = lds_1.Retrieve(ld_Fecha,li_Agronomo)
	
	IF ll_fila = -1 THEN
		MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
							"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
	ELSEIF ll_fila = 0 THEN
		MessageBox( "No Existe información", "No existe información para Agrónomo y Fecha actual.", &
						StopSign!, Ok!)
	ELSE
		lds_1.Modify("raz_social.text = '" + gstr_apl.Razon_Social + "'")
		lds_1.Modify("nom_empresa.text = '" + gstr_apl.Nom_Empresa + "'")
		lds_1.Modify("referencia.text = '" + gstr_apl.Referencia + "'")
		
		FileDelete(is_Archivo)
		
		lds_1.SaveAs(is_Archivo,PSReport!,True)
		
	END IF
END IF

OpenWithParm(w_correo, is_Archivo)


end event

public subroutine buscapeso (integer ai_especie, integer ai_variedad);Integer li_tipoen, li_envase

//Busca Peso Estándar

SELECT	enva_tipoen, enva_codigo
	into	:li_tipoen, :li_envase
	FROM	dbo.variedades
	WHERE	espe_codigo = :ai_especie    
	AND	vari_codigo = :ai_variedad;
 
IF sqlca.sqlcode = -1 THEN
	F_errorBaseDatos(sqlca,"Lectura de Tabla Variedades")
	
ELSEIF sqlca.sqlcode <> 100 THEN

	SELECT	enva_pesone
		into	:id_pesostd
 	 	FROM 	dbo.envases
		WHERE	enva_tipoen = :li_tipoen    
		AND	enva_codigo = :li_envase;
		 
	IF sqlca.sqlcode = -1 THEN
		F_errorBaseDatos(sqlca,"Lectura de Tabla Envases")
	ELSE	
		IF Isnull(id_pesostd) THEN
		   id_pesostd=0
	   END If
   END IF	
END IF
end subroutine

public subroutine buscapesovariedad (integer ai_especie, integer ai_variedad);id_pesostd = 0

SELECT	vari_pnestd INTO	:id_pesostd
	FROM	dbo.variedades
	WHERE	espe_codigo	=	:ai_Especie
	AND	vari_codigo	=	:ai_variedad;
	
If IsNull(id_pesoStd) or id_pesostd = 0 Then
	MessageBox("Atención","Peso Estándar no está asignado a la Variedad")
	id_pesostd = 0
End If
end subroutine

public subroutine habilitaenca (boolean ab_habilita);If ab_habilita Then
	
	dw_2.Object.espe_codigo.Protect 				=	0
	dw_2.Object.vari_codigo.Protect 				=	0
	dw_2.Object.prpr_codigo.Protect 				=	0
	dw_2.Object.prod_codigo.Protect 				=	0
	dw_2.Object.plde_codigo.Protect 				=	0
	dw_2.Object.rdte_seinco.Protect 				=	0
	dw_2.Object.rdte_tipmed.Protect 				=	0
	dw_2.Object.rdte_pesenv.Protect 				=	0

	dw_2.Object.espe_codigo.Color	=	0
	dw_2.Object.vari_codigo.Color		=	0
	dw_2.Object.prpr_codigo.Color	=	0
	dw_2.Object.prod_codigo.Color	=	0
	dw_2.Object.plde_codigo.Color		=	0
	dw_2.Object.rdte_seinco.Color		=	0
	dw_2.Object.rdte_tipmed.Color		=	0	
	dw_2.Object.rdte_pesenv.Color	=	0
	
	dw_2.Object.espe_codigo.BackGround.Color=	RGB(255,255,255)
	dw_2.Object.vari_codigo.BackGround.Color	=	RGB(255,255,255)
	dw_2.Object.prpr_codigo.BackGround.Color	=	RGB(255,255,255)
	dw_2.Object.prod_codigo.BackGround.Color	=	RGB(255,255,255)
	dw_2.Object.plde_codigo.BackGround.Color	=	RGB(255,255,255)
	dw_2.Object.rdte_seinco.BackGround.Color	=	RGB(255,255,255)
	dw_2.Object.rdte_tipmed.BackGround.Color	=	RGB(255,255,255)
	dw_2.Object.rdte_pesenv.BackGround.Color	=	RGB(255,255,255)
	
	If gstr_agro.administrador=0 or isnull(gstr_agro.codigoagronomo) Then
		dw_2.Object.agro_codigo.Protect 				=	1
		dw_2.Object.agro_codigo.Color				=	RGB(255,255,255)
      	dw_2.Object.agro_codigo.BackGround.Color	=	553648127
	Else
		dw_2.Object.agro_codigo.Protect 				=	0
		dw_2.Object.agro_codigo.Color				=	0
		dw_2.Object.agro_codigo.BackGround.Color	=	RGB(255,255,255)
	End If
Else
	dw_2.Object.espe_codigo.Protect 				=	1
	dw_2.Object.vari_codigo.Protect 				=	1
	dw_2.Object.prpr_codigo.Protect 				=	1
	dw_2.Object.agro_codigo.Protect 				=	1
	dw_2.Object.prod_codigo.Protect 				=	1
	dw_2.Object.plde_codigo.Protect 				=	1
	dw_2.Object.rdte_seinco.Protect 				=	1
	dw_2.Object.rdte_tipmed.Protect 				=	1
	dw_2.Object.rdte_pesenv.Protect 				=	1
	
	dw_2.Object.espe_codigo.Color	=	RGB(255,255,255)
	dw_2.Object.vari_codigo.Color		=	RGB(255,255,255)
	dw_2.Object.prpr_codigo.Color	=	RGB(255,255,255)
	dw_2.Object.agro_codigo.Color	=	RGB(255,255,255)
	dw_2.Object.prod_codigo.Color	=	RGB(255,255,255)
	dw_2.Object.plde_codigo.Color		=	RGB(255,255,255)
	dw_2.Object.rdte_seinco.Color		=	RGB(255,255,255)
	dw_2.Object.rdte_tipmed.Color		=	RGB(255,255,255)
	dw_2.Object.rdte_pesenv.Color	=	RGB(255,255,255)
	
	dw_2.Object.espe_codigo.BackGround.Color=	553648127
	dw_2.Object.vari_codigo.BackGround.Color	=	553648127
	dw_2.Object.prpr_codigo.BackGround.Color	=	553648127
	dw_2.Object.agro_codigo.BackGround.Color	=	553648127
	dw_2.Object.prod_codigo.BackGround.Color	=	553648127
	dw_2.Object.plde_codigo.BackGround.Color	=	553648127
	dw_2.Object.rdte_seinco.BackGround.Color	=	553648127
	dw_2.Object.rdte_tipmed.BackGround.Color	=	553648127
	dw_2.Object.rdte_pesenv.BackGround.Color	=	553648127
End If
end subroutine

public subroutine buscanombres (integer ssa);
end subroutine

protected function boolean wf_actualiza_db ();Boolean	lb_AutoCommit, lb_Retorno

IF Not dw_1.uf_check_required(0) THEN RETURN False
IF Not dw_1.uf_validate(0) THEN RETURN False

lb_AutoCommit		=	sqlca.AutoCommit
sqlca.AutoCommit	=	False

IF dw_2.Update(True, False) = 1 THEN 
	IF dw_1.Update(True,False) = 1 THEN
		Commit;
		IF sqlca.SQLCode <> 0 THEN
			F_ErrorBaseDatos(sqlca, This.Title)
			lb_Retorno	=	False
		ELSE
			lb_Retorno	=	True
			dw_2.ResetUpdate()
			dw_1.ResetUpdate()
		END IF
	ELSE
		RollBack;
		IF sqlca.SQLCode <> 0 THEN F_ErrorBaseDatos(sqlca, This.Title)
		lb_Retorno	=	False
	END IF
ELSE
	RollBack;
	IF sqlca.SQLCode <> 0 THEN F_ErrorBaseDatos(sqlca, This.Title)
	lb_Retorno	=	False
END IF
	  
sqlca.AutoCommit	=	lb_AutoCommit

RETURN lb_Retorno
end function

public subroutine asignadatosmedicion (string as_columna, string as_valor);String	ls_Columna
Long		ll_Fila

CHOOSE CASE as_Columna
	CASE "rdte_tipmed"
		ls_columna	=	"tipo_med"
		
	CASE "rdte_pesenv"
		ls_columna	=	"peso_bins"
		
END CHOOSE

FOR ll_Fila = 1 TO dw_1.RowCount()
	dw_1.SetItem(ll_Fila,ls_columna,Dec(as_valor))
NEXT

RETURN
end subroutine

public subroutine existepronostico (string as_columna, string as_valor);Integer		li_Productor, li_Especie, li_Variedad, li_Packing, &
				li_Agronomo, li_tipmed
Decimal{2}	ld_pesenv
Long			ll_cajpro, ll_cajcal, ll_Fila, ll_Predio, ll_bincal, ll_binpro

li_Productor		=	dw_2.Object.prod_codigo[1]
ll_Predio			=	dw_2.Object.prpr_codigo[1]
li_Especie		=	dw_2.Object.espe_codigo[1]
li_Variedad		=	dw_2.Object.vari_codigo[1]

CHOOSE CASE	as_columna
	CASE "prod_codigo"
		li_Productor	=	Integer(as_Valor)
		
	CASE "prpr_codigo"
		ll_Predio		=	Long(as_Valor)
		
	CASE "espe_codigo"
		li_Especie		=	Integer(as_Valor)
		
	CASE "vari_codigo"
		li_Variedad		=	Integer(as_Valor)
		
END CHOOSE

IF li_Productor<>0 AND Isnull(li_Productor)=FALSE AND &
	ll_Predio<>0 AND Isnull(ll_Predio)=FALSE AND &
   li_Especie<>0 AND Isnull(li_Especie)=FALSE AND &
	li_Variedad<>0 AND Isnull(li_Variedad)=FALSE THEN

	SELECT	rdte_pesenv, rdte_tipmed
		INTO	:ld_pesenv, :li_tipmed 
		FROM	dbo.pron_resuldistempenca
		WHERE prpr_codigo = :ll_Predio
		AND	espe_codigo = :li_Especie
		AND	vari_codigo = :li_Variedad
		And	prod_codigo = :li_Productor
		AND	pate_tempor = :gstr_tempo.Temporada - 1;
		
	IF sqlca.sqlcode = -1 THEN
		F_errorBaseDatos(sqlca,"Lectura de Tabla Encabezado de Pronóstico (Temporada Anterior)")
	END IF

	SELECT plde_codigo, rdte_binpro, rdte_bincal, rdte_cajpro, rdte_cajcal
		INTO	:li_Packing, :ll_binpro, :ll_bincal, :ll_cajpro, :ll_cajcal
		FROM	dbo.pron_resuldistempenca
		WHERE	prpr_codigo = :ll_Predio
		AND	espe_codigo = :li_Especie
		AND	vari_codigo = :li_Variedad
		And	prod_codigo = :li_Productor
		AND	pate_tempor = :gstr_tempo.Temporada;
	
	IF sqlca.sqlcode = -1 THEN
		F_errorBaseDatos(sqlca,"Lectura de Tabla Encabezado de Pronóstico")
	ELSEIF sqlca.sqlcode = 0 THEN
		dw_2.SetRedraw(False)
		
		ll_Fila = dw_2.Retrieve(li_Productor,ll_Predio,li_Especie,li_Variedad,gstr_Tempo.Temporada)
		
		IF ll_Fila = 0 THEN
			MessageBox("Atención","Falta asociar el Productor / Especie al Agrónomo.")
			dw_2.InsertRow(0)
			dw_2.SetRedraw(True)
			RETURN
		ELSE
			IF Isnull(dw_2.Object.rdte_pesenv[1]) OR dw_2.Object.rdte_pesenv[1] = 0 THEN
				ObtienePesoCosecha(li_Especie,li_Variedad)
				dw_2.Object.rdte_pesenv[1]	=	id_pesocos
			ELSE
				id_pesocos	=	dw_2.Object.rdte_pesenv[1]
			END IF
			
			IF Isnull(dw_2.Object.rdte_tipmed[1]) OR dw_2.Object.rdte_tipmed[1] = 0 THEN
				dw_2.Object.rdte_tipmed[1]	=	li_tipmed
			END IF
			
			IF NOT Isnull(ll_cajpro) AND ll_cajpro > 0 AND ll_cajcal <> ll_cajpro THEN
				ib_cambio_seleccion	=	True
			END IF
	
			dw_2.SetRedraw(True)
			
			li_Agronomo	=	dw_2.Object.agro_codigo[1]
			iuo_Planta.Existe(li_Packing, True, Sqlca)
			
			HabilitaEnca(False)
			
			TriggerEvent("ue_recuperadatos")
		END IF
	END IF		
END IF

RETURN
end subroutine

public subroutine obtienepesocosecha (integer ai_especie, integer ai_variedad);id_pesocos	=	0

SELECT	pcva_pescos INTO	:id_pesocos
	FROM	dbo.pron_pesocosvariedad
	WHERE	espe_codigo	=	:ai_Especie
	AND	vari_codigo	=	:ai_variedad;
	
If IsNull(id_pesoCos) Or id_pesocos = 0 Then
	MessageBox("Atención","Peso de Cosecha no está asignado a la Variedad")
	id_pesocos = 0
Else
	dw_2.Object.rdte_pesenv[1] = id_pesocos
End If
end subroutine

public function boolean wf_traspasadistribucion ();Boolean	lb_Retorno = True

Long		ll_Productor, ll_Predio, li_Especie, li_Variedad

ll_Productor		= dw_2.Object.prod_codigo[1]
ll_Predio			= dw_2.Object.prpr_codigo[1]
li_Especie		= dw_2.Object.espe_codigo[1]
li_Variedad		= dw_2.Object.vari_codigo[1]

Declare Traspasa Procedure For dbo.Pron_DistribucionEstandar
	@Temporada	=	:gstr_tempo.temporada,
	@Productor		=	:ll_Productor,
	@Predio			=	:ll_Predio,
	@Especie		=	:li_Especie,
	@Variedad		=	:li_Variedad
	Using SQLCA ;

Execute Traspasa;	

If sqlca.SQLCode = -1 Then
	F_ErrorBaseDatos(SQLCA,"Problema en Carga de Distribucion Estandar" )
	lb_Retorno = False
End If

Close Traspasa;
Commit;

Return lb_Retorno
end function

public function integer wf_cargacuarteles (long al_productor, integer ai_predio, integer ai_especie, integer ai_variedad);Long   		ll_Fila, ll_FilaDeta, ll_exisFil, ll_FilaNueva, ll_Codigo
DataStore	ds_Carga

ds_Carga	=	Create DataStore

dw_1.Retrieve(al_Productor, ai_Predio, ai_Especie, ai_Variedad, gstr_Tempo.Temporada)

ds_Carga.Dataobject =	'dw_carga_cuarteles'
ds_Carga.SetTransObject(SQLca)

ll_Fila	=	ds_Carga.Retrieve(al_Productor, ai_Predio, ai_Especie, ai_Variedad)

For ll_Fila = 1 To ds_Carga.RowCount()
	ll_Codigo		=	ds_Carga.Object.prcc_codigo[ll_Fila]
	
	ll_FilaDeta	= dw_1.Find("prcc_codigo = " + String(ll_Codigo), 1, dw_1.RowCount())
	 	 				
	If ll_FilaDeta = 0 Then
		ll_FilaNueva = dw_1.InsertRow(0)

		dw_1.Object.prod_codigo[ll_FilaNueva] 	=	al_Productor
         dw_1.Object.prpr_codigo[ll_FilaNueva] 	=	ai_Predio
         dw_1.Object.espe_codigo[ll_FilaNueva] 	=	ai_Especie
         dw_1.Object.vari_codigo[ll_FilaNueva] 	=	ai_Variedad
         dw_1.Object.pate_tempor[ll_FilaNueva] 	=	gstr_Tempo.Temporada
         dw_1.Object.prcc_codigo[ll_FilaNueva] 	=	ll_Codigo
         dw_1.Object.prcc_superf[ll_FilaNueva] 	=	ds_Carga.Object.prcc_superf[ll_Fila]
         dw_1.Object.vari_nombre[ll_FilaNueva] 	=	ds_Carga.Object.vari_nombre[ll_Fila]
         dw_1.Object.prcc_nombre[ll_FilaNueva] 	=	ds_Carga.Object.prcc_nombre[ll_Fila]
         dw_1.Object.prcc_anopla[ll_FilaNueva] 	=	ds_Carga.Object.prcc_anopla[ll_Fila]
         dw_1.Object.patr_nombre[ll_FilaNueva] 	=	ds_Carga.Object.patr_nombre[ll_Fila]
         dw_1.Object.siri_nombre[ll_FilaNueva] 	=	ds_Carga.Object.siri_nombre[ll_Fila]
         dw_1.Object.prcc_semini[ll_FilaNueva] 	=	ds_Carga.Object.prcc_semini[ll_Fila]
		dw_1.Object.prcc_fecini[ll_FilaNueva] 	=	ds_Carga.Object.prcc_fecini[ll_Fila]
	Else
		If IsNull(dw_1.Object.prcc_semini[ll_FilaDeta]) Or IsNull(dw_1.Object.prcc_fecini[ll_FilaDeta]) &
				Or dw_1.Object.prcc_fecini[ll_FilaDeta] < ds_Carga.Object.prcc_fecini[ll_Fila] Then
			dw_1.Object.prcc_semini[ll_FilaDeta] =	ds_Carga.Object.prcc_semini[ll_Fila]
			dw_1.Object.prcc_fecini[ll_FilaDeta] 	=	ds_Carga.Object.prcc_fecini[ll_Fila]
		End If
	End If
Next

Destroy ds_Carga

ll_exisFil = dw_1.RowCount()

Return ll_exisFil
end function

public function boolean wf_validadistribucionestandar (long al_productor, integer ai_predio);Boolean		lb_Retorno = True
Long			ll_Fila
String			ls_Mensaje, ls_Mensaje_Detalle = ''
DateTime	ld_Fecha

ls_Mensaje	= 'Productor: ' + iuo_productores.Nombre + '~nPredio: ' + iuo_Predios.Nombre

For ll_Fila = 1 To dw_1.RowCount()
	If Not IsNull(dw_1.Object.prcc_fecini[ll_Fila]) And (dw_1.Object.prcc_fecini[ll_Fila] <> ld_Fecha) Then
		If Not wf_ValidaSemana(dw_1.Object.prcc_fecini[ll_Fila], dw_1.Object.prcc_semini[ll_Fila], dw_1.Object.espe_codigo[ll_Fila], ll_Fila) Then
			ls_Mensaje_Detalle += '~nCuartel: ' + dw_1.Object.prcc_nombre[ll_Fila] + '. No posee Semana de Inicio, para efectuar Distribucion Estandar.'
			lb_Retorno = False
		End If
	Else
		ls_Mensaje_Detalle += '~nCuartel: ' + dw_1.Object.prcc_nombre[ll_Fila] + '. No posee Semana de Inicio, Favor ingresar en Ficha de Cuarteles.'
		lb_Retorno = False
	End If
	
	If Not wf_ValidaCondicion(al_Productor, ai_Predio, dw_1.Object.prcc_codigo[ll_Fila], dw_1.Object.espe_codigo[1], dw_1.Object.vari_codigo[1]) Then
		ls_Mensaje_Detalle += '~nCuartel: ' + dw_1.Object.prcc_nombre[ll_Fila] + '. No posee Distribucion de Condicion, para efectuar Distribucion Estandar.'
		lb_Retorno = False
	End If
	
	If Not wf_ValidaCalibre(al_Productor, ai_Predio, dw_1.Object.prcc_codigo[ll_Fila], dw_1.Object.espe_codigo[1], dw_1.Object.vari_codigo[1]) Then
		ls_Mensaje_Detalle += '~nCuartel: ' + dw_1.Object.prcc_nombre[ll_Fila] + '. No posee Distribucion de Calibres, para efectuar Distribucion Estandar.'
		lb_Retorno = False
	End If
Next

If Len(ls_Mensaje_Detalle) > 0 Then MessageBox('Atencion',ls_Mensaje + ls_Mensaje_Detalle, Exclamation!, OK!)

Return lb_Retorno
end function

public function boolean wf_validasemana (datetime ad_fecha, integer ai_semana, integer ai_especie, integer row);Boolean			lb_Retorno = True
uo_nrosemana	luo_semana

luo_semana	=	Create uo_nrosemana
	
If Not luo_semana.of_TemporadaActual(ad_Fecha, gstr_tempo.Temporada, ai_Especie, SQLCA) Then
	If luo_semana.Semana(ai_Semana,gstr_tempo.Temporada, ai_Especie) Then
		dw_1.Object.prcc_fecini[Row] = luo_semana.Lunes
	Else
		lb_Retorno = False
	End If
//Else
//	lb_Retorno = False
End If		

Destroy	luo_semana

Return 	lb_Retorno 
end function

public function boolean wf_validacalibre (long al_productor, integer ai_predio, integer ai_cuartel, integer ai_especie, integer ai_variedad);Boolean	lb_Retorno = True
Long		ll_Cantidad, ll_Porcentaje

SELECT 	IsNull(Count(prcc_codigo), 0),  Sum(IsNull(cacu_pordis, 0)) 
	INTO	:ll_Cantidad, :ll_Porcentaje
	FROM	dbo.pron_calibrecuartel
	WHERE	prod_codigo	=	:al_Productor
	    AND  prpr_codigo = :ai_Predio
		And prcc_codigo in(-1, :ai_Cuartel)
		And espe_codigo = :ai_Especie
		And vari_codigo = :ai_Variedad
	USING	Sqlca;

If Sqlca.SQLCode = -1 Then
	F_ErrorBaseDatos(Sqlca, "Lectura de Tabla Calibre Cuartel")
	lb_Retorno = False
ElseIf Sqlca.SQLCode = 100 Then
	lb_Retorno =  False
Else
	If ll_Cantidad = 0 Then lb_Retorno = False
	If ll_Porcentaje < 100 Then lb_Retorno = False
End If

Return lb_Retorno
end function

public function boolean wf_validacondicion (long al_productor, integer ai_predio, integer ai_cuartel, integer ai_especie, integer ai_variedad);Boolean	lb_Retorno = True
Long		ll_Cantidad, ll_Porcentaje

SELECT 	IsNull(Count(prcc_codigo), 0),  Sum(IsNull(cocu_pordis, 0)) 
	INTO	:ll_Cantidad, :ll_Porcentaje
	FROM	dbo.pron_condicioncuartel
	WHERE	prod_codigo	=	:al_Productor
	    AND  prpr_codigo = :ai_Predio
		And prcc_codigo in(-1, :ai_Cuartel)
		And espe_codigo = :ai_Especie
		And vari_codigo = :ai_Variedad
	USING	Sqlca;

If Sqlca.SQLCode = -1 Then
	F_ErrorBaseDatos(Sqlca, "Lectura de Tabla Condicion Cuartel")
	lb_Retorno = False
ElseIf Sqlca.SQLCode = 100 Then
	lb_Retorno =  False
Else
	If ll_Cantidad = 0 Then lb_Retorno = False
	If ll_Porcentaje < 100 Then lb_Retorno = False
End If

Return lb_Retorno
end function

on w_plan_pronostico_cosecha.create
int iCurrent
call super::create
this.dw_2=create dw_2
this.cb_estandar=create cb_estandar
this.cb_produccion=create cb_produccion
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_2
this.Control[iCurrent+2]=this.cb_estandar
this.Control[iCurrent+3]=this.cb_produccion
end on

on w_plan_pronostico_cosecha.destroy
call super::destroy
destroy(this.dw_2)
destroy(this.cb_estandar)
destroy(this.cb_produccion)
end on

event open;Integer li_null
isnull(li_null)

x				= 0
y				= 0
This.Width	= dw_1.width + 540
This.Height	= 2500
im_menu	= m_principal

This.ParentWindow().ToolBarVisible	=	True
im_menu.Item[1].Item[6].Enabled		=	True
im_menu.Item[7].Visible					=	False
This.Icon									=	Gstr_apl.Icono

istr_mant.UsuarioSoloConsulta	=	OpcionSoloConsulta()
istr_mant.Solo_Consulta			=	istr_mant.UsuarioSoloConsulta

istr_mant.argumento[1] = ""
istr_mant.argumento[2] = ""
istr_mant.argumento[3] = ""
istr_mant.argumento[4] = ""
istr_mant.argumento[5] = ""
istr_mant.argumento[6] = ""

istr_busq	=	Message.PowerObjectParm

iuo_Agronomo			=	Create uo_Agronomo
iuo_especie				=	Create uo_Especie
iuo_variedades			=	Create uo_Variedades
iuo_productores      	=	Create uo_Productores
iuo_predios				=	Create uo_Predios
iuo_agroespeprod 		=	Create uo_Agronomo_Productor
iuo_semana				=	Create uo_Nrosemana
iuo_Planta				=	Create uo_Plantadesp

is_directorio = GetCurrentDirectory()

//agronomo
dw_2.GetChild("agro_codigo", idwc_agro)
idwc_agro.SetTransObject(sqlca)
idwc_agro.Retrieve()

//Productor//
dw_2.GetChild("prod_codigo", idwc_productor)
idwc_productor.SetTransObject(sqlca)
idwc_productor.Retrieve(-1)

//Predio//
dw_2.GetChild("prpr_codigo", idwc_predio)
idwc_predio.SetTransObject(sqlca)
idwc_predio.retrieve(-1)

// Especie//
dw_2.GetChild("espe_codigo", idwc_especie)
idwc_especie.SetTransObject(sqlca)
idwc_especie.Retrieve(-1, -1)

//Variedad//
dw_2.GetChild("vari_codigo", idwc_variedad)
idwc_variedad.SetTransObject(sqlca)
idwc_variedad.Retrieve(-1, -1, -1)

//planta//
dw_2.GetChild("plde_codigo", idwc_pack)
idwc_pack.SetTransObject(sqlca)
idwc_pack.Retrieve(-1)

dw_2.GetChild("rdte_cierre", idwc_cierre)
idwc_Cierre.SetTransObject(sqlca)
idwc_Cierre.Retrieve(-1)

dw_1.SetTransObject(sqlca)
dw_2.SetTransObject(sqlca)

dw_1.Modify("datawindow.message.title='Error '+ is_titulo")

dw_2.InsertRow(0)
dw_2.SetItem(1,"pate_tempor",gstr_tempo.Temporada)

If istr_busq.Argum[1] <> "aa" AND istr_busq.Argum[1] <> "" Then
	dw_2.object.agro_codigo[1] = integer(istr_Busq.Argum[1])
	iuo_Agronomo.Existe(Integer(istr_Busq.Argum[1]), False, Sqlca)
	
	dw_2.object.prod_codigo[1] = integer(istr_Busq.Argum[2])
	istr_mant.argumento[3]     = istr_Busq.Argum[2]
	
	iuo_Productores.Existe(integer(istr_Busq.Argum[2]),False,sqlca)
	
	dw_2.GetChild("prpr_codigo",idwc_predio)
	idwc_predio.SetTransObject(SqlCa)
	If idwc_predio.Retrieve(iuo_productores.Codigo) = 0 Then
		dw_2.setitem( 1,'prpr_codigo', li_null)
	Else
		idwc_predio.SetTransObject(SqlCa)
		idwc_predio.InsertRow(0)
	End If
	
	dw_2.object.prpr_codigo[1] = Long(istr_Busq.Argum[3])
	istr_mant.argumento[4]     = istr_Busq.Argum[3]
	
	iuo_Predios.existe(iuo_Productores.Codigo, Integer(istr_Busq.Argum[3]), Sqlca, True)
	
	If Integer(istr_Busq.Argum[4]) <> 0 Then
		dw_2.object.espe_codigo[1] = Integer(istr_Busq.Argum[4])
		istr_mant.argumento[1]     = istr_Busq.Argum[4]

		iuo_Especie.Existe(Integer(istr_Busq.Argum[4]),False,sqlca)
			
		dw_2.GetChild("vari_codigo",idwc_variedad)
		idwc_variedad.SetTransObject(SqlCa)

 		If idwc_variedad.Retrieve(-1, -1, integer(istr_Busq.Argum[4])) = 0 Then
			dw_2.Object.vari_codigo[1] = li_Null
		Else
			idwc_variedad.SetTransObject(SqlCa)
			idwc_variedad.InsertRow(0)
		End If

 		If idwc_cacm.Retrieve(integer(istr_Busq.Argum[4])) = 0 Then
			idwc_cacm.InsertRow(0)
		End If

 		If idwc_cacm2.Retrieve(integer(istr_Busq.Argum[4])) = 0 Then
			idwc_cacm2.InsertRow(0)
		End If
 		If idwc_cacm3.Retrieve(integer(istr_Busq.Argum[4])) = 0 Then
			idwc_cacm3.InsertRow(0)
		End If

		If Integer(istr_Busq.Argum[5]) <> 0 Then
			dw_2.object.vari_codigo[1] = integer(istr_Busq.Argum[5])
			istr_mant.argumento[2]     = istr_Busq.Argum[5]
			
			iuo_variedades.existe(Integer(istr_Busq.Argum[4]),Integer(istr_Busq.Argum[5]),False,Sqlca)
			BuscapesoVariedad(Integer(istr_Busq.Argum[4]),Integer(istr_Busq.Argum[5]))
			ObtienePesoCosecha(Integer(istr_Busq.Argum[4]),Integer(istr_Busq.Argum[5]))
		End If
		
		If Integer(istr_Busq.Argum[4]) <> 0 and Integer(istr_Busq.Argum[5]) <> 0 Then 
			ExistePronostico("","")
		End If
   End If
	
Else
	If gstr_agro.CodigoAgronomo<>0 And Not IsNull(gstr_agro.CodigoAgronomo) Then
		dw_2.Object.agro_codigo[1] 	=	gstr_agro.CodigoAgronomo
		istr_mant.argumento[5] 			=	string(gstr_agro.CodigoAgronomo)
		idwc_productor.Retrieve(gstr_agro.CodigoAgronomo)
		
		If gstr_agro.Administrador = 0 Or IsNull(gstr_agro.CodigoAgronomo) Then
			dw_2.Object.agro_codigo.Protect = 1
			dw_2.Object.agro_codigo.BackGround.Color	=	553648127
		End If
	End If
End If	

il_tempo_antes = gstr_tempo.Temporada - 1
end event

event ue_recuperadatos;Long	 		ll_fila, respuesta

If IsNull(dw_2.Object.plde_codigo[1]) Then
	MessageBox('Error',"Favor ingresar Packing de cosecha.", StopSign!, OK!)
	Return 
End If

//If IsNull(dw_2.Object.rdte_seinco[1]) Then
//	MessageBox('Error',"Favor ingresar semana de inicio de consecha.", StopSign!, OK!)
//	Return 
//End If

DO
	ll_fila	= wf_CargaCuarteles(iuo_Productores.Codigo, iuo_Predios.Codigo, iuo_Especie.Codigo, iuo_Variedades.Variedad)
	//dw_1.Retrieve(iuo_Productores.Codigo, iuo_Predios.Codigo, iuo_Especie.Codigo, iuo_Variedades.Variedad, gstr_Tempo.Temporada)
					 	
	If ll_fila = -1 Then
		respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", Information!, RetryCancel!)
	ElseIf ll_fila = 0 Then
		MessageBox('Atencion...', 'NO existen Cuarteles para seleccion.', Information!, OK!)
	ElseIf ll_fila > 0 Then
		il_fila				= 1
		il_fila_antes		= 1
		
		dw_1.SetRow(1)
		dw_1.SetFocus()
		
		pb_grabar.Enabled	=	Not istr_Mant.Solo_Consulta
		pb_eliminar.Enabled	=	Not istr_Mant.Solo_Consulta
		pb_imprimir.Enabled	=	True
		Habilitaenca(FALSE)
	End If
	
LOOP WHILE respuesta = 1
If respuesta = 2 Then Close(This)
end event

event ue_imprimir;SetPointer(HourGlass!)

Long		fila, ll_fila

str_info	lstr_info
lstr_info.titulo	= "FICHA PRONOSTICO"
lstr_info.copias	= 1

OpenWithParm(vinf,lstr_info)
vinf.dw_1.DataObject = "dw_info_ficha_pronostico_comp"
vinf.dw_1.SetTransObject(sqlca)

fila = vinf.dw_1.Retrieve(integer(istr_mant.argumento[3]),Long(istr_mant.argumento[4]),&
                          integer(istr_mant.argumento[1]),integer(istr_mant.argumento[2]), gstr_tempo.temporada)

If fila = -1 Then
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ElseIf fila = 0 Then
	MessageBox( "No Existe información", "No existe información para este informe.",  StopSign!, Ok!)
Else
	vinf.dw_1.ModIfy("especie.text = '" + iuo_Especie.Nombre + "'")
	vinf.dw_1.ModIfy("variedad.text = '" + iuo_variedades.NombreVariedad + "'")
	vinf.dw_1.ModIfy("productor.text = '" + iuo_Productores.Nombre + "'")	
	vinf.dw_1.ModIfy("predio.text = '" + iuo_Predios.Nombre + "'")
	vinf.dw_1.ModIfy("agronomo.text = '" + is_nom_agr + "'")
	vinf.dw_1.ModIfy("packing.text = '" + iuo_Planta.Nombre + "'")
	
	F_Membrete(vinf.dw_1)
	vinf.dw_1.ModIfy('DataWindow.Print.Preview = Yes')
	vinf.dw_1.ModIfy('DataWindow.Print.Preview.Zoom = 75')

	vinf.Visible	= True
	vinf.Enabled	= True
End If

SetPointer(Arrow!)
end event

event ue_antesguardar;Long	ll_Fila

If IsNull(dw_2.Object.plde_codigo[1]) OR dw_2.Object.plde_codigo[1]	= 0 Then
	MessageBox("Atención","Debe especIficar el Packing de la Producción.")
	dw_2.SetColumn("plde_codigo")
	Message.DoubleParm = -1
	Return
End If

If IsNull(dw_2.Object.espe_codigo[1]) OR dw_2.Object.espe_codigo[1]	= 0 Then
	MessageBox("Atención","Debe especIficar la Especie.")
	dw_2.SetColumn("espe_codigo")
	Message.DoubleParm = -1
	Return
End If

If IsNull(dw_2.Object.prpr_codigo[1]) OR dw_2.Object.prpr_codigo[1]	= 0 Then
	MessageBox("Atención","Debe especIficar el Predio.")
	dw_2.SetColumn("prpr_codigo")
	Message.DoubleParm = -1
	Return
End If

If IsNull(dw_2.Object.vari_codigo[1]) OR dw_2.Object.vari_codigo[1]	= 0 Then
	MessageBox("Atención","Debe especIficar la Variedad.")
	dw_2.SetColumn("vari_codigo")
	Message.DoubleParm = -1
	Return
End If

If IsNull(dw_2.Object.prod_codigo[1]) OR dw_2.Object.prod_codigo[1]	= 0 Then
	MessageBox("Atención","Debe especIficar el Productor.")
	dw_2.SetColumn("prod_codigo")
	Message.DoubleParm = -1
	Return
End If

If IsNull(dw_2.Object.rdte_seinco[1]) OR dw_2.Object.rdte_seinco[1]	= 0 Then
	MessageBox("Atención","Debe especIficar la Semana Inicio Cosecha.")
	dw_2.SetColumn("rdte_seinco")
	Message.DoubleParm = -1
	Return
End If

If IsNull(dw_2.Object.agro_codigo[1]) OR dw_2.Object.agro_codigo[1]	= 0 Then
	MessageBox("Atención","Debe especIficar el Agronomo.")
	dw_2.SetColumn("agro_codigo")
	Message.DoubleParm = -1
	Return
End If

If dw_1.GetItemStatus(0, 'prcc_cajtot', Primary!) = DataModified! Then
	dw_2.Object.rdte_fecmod[1]	= 	Today()
	dw_2.Object.rdte_usuario[1]	=	gstr_us.Nombre
	dw_2.Object.rdte_computer[1]	=	gstr_us.Computador
	dw_2.Object.rdte_accion[1]		=	'UPD'
End If
end event

event ue_guardar;IF dw_1.AcceptText() = -1 THEN RETURN

SetPointer(HourGlass!)

w_main.SetMicroHelp("Grabando información...")

TriggerEvent("ue_antesguardar")

IF Message.DoubleParm = -1 THEN RETURN

IF wf_actualiza_db() THEN
	w_main.SetMicroHelp("Información Grabada.")
	pb_imprimir.Enabled	= True
ELSE
	w_main.SetMicroHelp("No se puede Grabar información.")
	Message.DoubleParm = -1
	RETURN
END IF
end event

event ue_borrar;Long	ll_Fila	

IF dw_2.Object.rdte_estado[1] = 1 THEN 
	MessageBox(This.Title,"No se puede borrar actual registro,  ya que estado es Definitivo.")
	RETURN
END IF

SetPointer(HourGlass!)

ib_borrar = True
w_main.SetMicroHelp("Validando la eliminación...")

Message.DoubleParm = 0

This.TriggerEvent ("ue_validaborrar")

IF Message.DoubleParm = -1 THEN RETURN

IF MessageBox("Eliminación de Registro", "Está seguro de Eliminar este Pronóstico", Question!, YesNo!) = 1 THEN
	FOR ll_Fila = 1 to dw_1.RowCount()
		dw_1.Object.prcc_cajcuar[ll_Fila] = 0
	NEXT	
END IF
end event

event resize;call super::resize;Integer		li_posic_x, li_posic_y, li_visible, &
				li_Ancho = 300, li_Alto = 245, li_Siguiente = 255, maximo

dw_1.Resize(This.WorkSpaceWidth() - 510,This.WorkSpaceHeight() - dw_1.y - 75)

maximo	= dw_1.width

If dw_2.width > maximo Then maximo = dw_2.width

li_posic_x				=	This.WorkSpaceWidth() - 370
li_posic_y				=	st_encabe.y

pb_lectura.x				= li_posic_x
pb_lectura.y				= li_posic_y
pb_lectura.width		= li_Ancho
pb_lectura.height		= li_Alto
li_posic_y 				+= li_Siguiente

dw_2.x					= 37 + Round((maximo - dw_2.width) / 2, 0)
dw_2.y					= 37

dw_1.x					= 37 + Round((maximo - dw_1.width) / 2, 0)
dw_1.y					= 64 + dw_2.Height

dw_1.height				= This.WorkSpaceHeight() - dw_1.y - 41


If pb_nuevo.Visible Then
	pb_nuevo.x				=	li_posic_x
	pb_nuevo.y				=	li_posic_y
	pb_nuevo.width		=	li_Ancho
	pb_nuevo.height		=	li_Alto
	li_visible 				++
	li_posic_y 				+= li_Siguiente
End If

If pb_insertar.Visible Then
	pb_insertar.x			=	li_posic_x
	pb_insertar.y			=	li_posic_y
	pb_insertar.width		=	li_Ancho
	pb_insertar.height	=	li_Alto
	li_visible ++
	li_posic_y += li_Siguiente
End If

If pb_eliminar.Visible Then
	pb_eliminar.x			=	li_posic_x
	pb_eliminar.y			=	li_posic_y
	pb_eliminar.width		=	li_Ancho
	pb_eliminar.height	=	li_Alto
	li_visible ++
	li_posic_y += li_Siguiente
End If

If pb_grabar.Visible Then
	pb_grabar.x				=	li_posic_x
	pb_grabar.y				=	li_posic_y
	pb_grabar.width		=	li_Ancho
	pb_grabar.height		=	li_Alto
	li_visible ++
	li_posic_y += li_Siguiente
End If

If pb_imprimir.Visible Then
	pb_imprimir.x			=	li_posic_x
	pb_imprimir.y			=	li_posic_y
	pb_imprimir.width		=	li_Ancho
	pb_imprimir.height	=	li_Alto
	li_visible ++
	li_posic_y += li_Siguiente
End If

cb_Estandar.x	=	li_posic_x
cb_Estandar.y	=	li_posic_y
li_posic_y +=  cb_Estandar.Height + 5

cb_Produccion.x	=	li_posic_x
cb_Produccion.y	=	li_posic_y

pb_Salir.x				=	li_posic_x
pb_Salir.y				=	dw_1.y + dw_1.Height - li_Siguiente
pb_Salir.width			=	li_Ancho
pb_Salir.height			=	li_Alto
end event

type st_encabe from w_mant_directo`st_encabe within w_plan_pronostico_cosecha
boolean visible = false
integer x = 55
integer y = 20
integer width = 2839
integer height = 452
integer taborder = 10
end type

type pb_nuevo from w_mant_directo`pb_nuevo within w_plan_pronostico_cosecha
integer x = 4279
integer y = 428
integer taborder = 120
end type

event pb_nuevo::clicked;call super::clicked;dw_2.Reset()
dw_2.InsertRow(0)
dw_2.SetItem(1,"pate_tempor",gstr_tempo.Temporada)

istr_mant.argumento[1]	=	'0'
istr_mant.argumento[2]	=	'0'
istr_mant.argumento[3]	=	'0'
istr_mant.argumento[4]	=	'0'

SetNull(iuo_Agronomo.Codigo)
SetNull(iuo_Productores.Codigo)
SetNull(iuo_Predios.Codigo)

pb_grabar.enabled		=	False
pb_imprimir.enabled		=	False

il_Fila					=	0

Habilitaenca(True)

If gstr_agro.codigoagronomo<>0 and isnull(gstr_agro.codigoagronomo)=False Then
	dw_2.Object.agro_codigo[1] 	=	gstr_agro.codigoagronomo
	istr_mant.argumento[5] 			=	string(gstr_agro.codigoagronomo)
	If gstr_agro.administrador=0 or isnull(gstr_agro.codigoagronomo) Then
		dw_2.Object.agro_codigo.Protect = 1
	End If
End If
end event

type pb_lectura from w_mant_directo`pb_lectura within w_plan_pronostico_cosecha
integer x = 4279
integer y = 128
integer taborder = 30
boolean default = true
end type

type pb_eliminar from w_mant_directo`pb_eliminar within w_plan_pronostico_cosecha
integer x = 4274
integer y = 1684
integer taborder = 0
string picturename = "\Desarrollo 17\Imagenes\Botones\Basura.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\Basura-bn.png"
end type

type pb_insertar from w_mant_directo`pb_insertar within w_plan_pronostico_cosecha
boolean visible = false
integer x = 4288
integer y = 1340
integer taborder = 0
end type

type pb_salir from w_mant_directo`pb_salir within w_plan_pronostico_cosecha
integer x = 4279
integer y = 1532
integer taborder = 180
end type

type pb_imprimir from w_mant_directo`pb_imprimir within w_plan_pronostico_cosecha
integer x = 4283
integer y = 780
integer taborder = 160
fontcharset fontcharset = ansi!
string facename = "Tahoma"
end type

type pb_grabar from w_mant_directo`pb_grabar within w_plan_pronostico_cosecha
integer x = 4279
integer y = 596
integer taborder = 140
end type

type dw_1 from w_mant_directo`dw_1 within w_plan_pronostico_cosecha
integer x = 55
integer y = 516
integer width = 3982
integer height = 1100
integer taborder = 40
string dataobject = "dw_plan_pronostico_cosecha_cuartel"
boolean hscrollbar = true
boolean hsplitscroll = true
end type

event dw_1::itemchanged;call super::itemchanged;String		ls_Columna
Dec{2}	ld_Valor	

ls_Columna	=	dwo.Name

Choose Case ls_Columna
	Case "prcc_cajhec"
		This.Object.prcc_cajcuar[Row] = Real(Data) * This.Object.prcc_superf[Row]
		This.Object.prcc_cajtot[Row] = Real(Data) * This.Object.prcc_superf[Row]

	Case 'prcc_cajcuar'
		This.Object.prcc_cajhec[Row] = Real(Data) / This.Object.prcc_superf[Row]
		This.Object.prcc_cajtot[Row] = Real(Data)
		
	Case 'prcc_nropta'
		ld_Valor	= (Real(Data) * (This.Object.prcc_kunif[Row] / 100) * This.Object.prcc_frubue[Row] * &
										This.Object.prcc_pesrac[Row] * (This.Object.prcc_porpro[Row] / 100)) / (1000 * This.Object.prcc_pesnor[Row])

		This.Object.prcc_cajcuar[Row] = ld_Valor
		This.Object.prcc_cajtot[Row] = ld_Valor
		
		This.Object.prcc_cajhec[Row] = ld_Valor / This.Object.prcc_superf[Row]
		
	Case 'prcc_frubue'
		ld_Valor = (Real(Data) * (This.Object.prcc_kunif[Row] / 100) * This.Object.prcc_nropta[Row] * &
										This.Object.prcc_pesrac[Row] * (This.Object.prcc_porpro[Row] / 100)) / (1000 * This.Object.prcc_pesnor[Row])
		This.Object.prcc_cajcuar[Row] = ld_Valor
		This.Object.prcc_cajtot[Row] = ld_Valor
		This.Object.prcc_cajhec[Row] = ld_Valor / This.Object.prcc_superf[Row]
		
	Case 'prcc_porpro'
		ld_Valor = ((Real(Data) / 100) * (This.Object.prcc_kunif[Row]  / 100)* This.Object.prcc_frubue[Row] * &
										This.Object.prcc_pesrac[Row] * This.Object.prcc_nropta[Row]) / (1000 * This.Object.prcc_pesnor[Row])
		This.Object.prcc_cajcuar[Row] = ld_valor
		This.Object.prcc_cajtot[Row] = ld_Valor
		This.Object.prcc_cajhec[Row] = ld_Valor / This.Object.prcc_superf[Row]
		
	Case 'prcc_kunif'
		ld_Valor = ((Real(Data)  / 100) * This.Object.prcc_nropta[Row] * This.Object.prcc_frubue[Row] * &
										This.Object.prcc_pesrac[Row] * (This.Object.prcc_porpro[Row] / 100)) / (1000 * This.Object.prcc_pesnor[Row])
		This.Object.prcc_cajcuar[Row] =  ld_Valor
		This.Object.prcc_cajtot[Row] = ld_Valor
		This.Object.prcc_cajhec[Row] = ld_Valor / This.Object.prcc_superf[Row]
		
	Case 'prcc_pesrac'
		ld_Valor = (Real(Data) * (This.Object.prcc_kunif[Row] / 100)* This.Object.prcc_frubue[Row] * &
										This.Object.prcc_nropta[Row] * (This.Object.prcc_porpro[Row] / 100)) / (1000 * This.Object.prcc_pesnor[Row])
		This.Object.prcc_cajcuar[Row] =  ld_Valor
		This.Object.prcc_cajtot[Row] = ld_Valor
		This.Object.prcc_cajhec[Row] = ld_Valor / This.Object.prcc_superf[Row]
	
	Case 'prcc_pesnor'
		If IsNull(Data) Then Data = '1'
		ld_valor = ( This.Object.prcc_nropta[Row] * (This.Object.prcc_kunif[Row] / 100) * This.Object.prcc_frubue[Row] * &
										This.Object.prcc_pesrac[Row] * (This.Object.prcc_porpro[Row] / 100)) / (1000 * Real(Data))
		This.Object.prcc_cajcuar[Row] = ld_Valor 
		This.Object.prcc_cajtot[Row] = ld_Valor
		This.Object.prcc_cajhec[Row] = ld_Valor / This.Object.prcc_superf[Row]

End Choose 
end event

event dw_1::dwnkey;This.SetRedraw(False)

CHOOSE CASE Key
	CASE KeyRightArrow!, KeyLeftArrow!
		This.SetRedraw(True)
		RETURN -1
		
	CASE KeyDownArrow!, KeyUpArrow!
		Message.DoubleParm = 0
		
		Parent.TriggerEvent("ue_validaregistro")
		
		IF Message.DoubleParm = -1 THEN
			This.SetRedraw(True)
			RETURN -1
		END IF
		
	CASE KeyTab!
		IF is_ultimacol = This.GetColumnName() AND il_fila = dw_1.RowCount() THEN
			Message.DoubleParm = 0
			
			Parent.TriggerEvent("ue_validaregistro")
			
			IF Message.DoubleParm = -1 THEN
				This.SetRedraw(True)
				RETURN -1
			END IF
		END IF

END CHOOSE

This.SetRedraw(True)

RETURN 0
end event

type dw_2 from datawindow within w_plan_pronostico_cosecha
integer x = 59
integer y = 20
integer width = 3054
integer height = 460
integer taborder = 20
boolean bringtotop = true
string title = "none"
string dataobject = "dw_mues_plan_pronos_cosecha_enca"
boolean border = false
boolean livescroll = true
end type

event itemchanged;String 	ls_columna, ls_Null
Integer	ll_fila, li_productor, li_semana
Long		respuesta,ll_cajas,ll_bins
Dec{2}	ld_ton, ld_poremb, ld_kilos,ld_pesenv

SetNull(ls_Null)

ls_columna = dwo.name 

Choose Case ls_columna
	Case 	"agro_codigo"
		If Not iuo_Agronomo.Existe(integer(Data), True, Sqlca) Then
			dw_2.Setitem(Row, ls_Columna, Integer(ls_Null))
			Return 1
		ELSE	
			If Not iuo_AgroEspeProd.existe(iuo_Agronomo.Codigo, Integer(istr_mant.argumento[1]), &
		                                  iuo_Productores.Codigo, iuo_Predios.Codigo, True,sqlca) Then
				This.Setitem(Row,ls_Columna, Integer(ls_Null))
				Return 1
			End If
			
			This.GetChild("prod_codigo", idwc_productor)
			idwc_productor.SetTransObject(sqlca)
			idwc_productor.Retrieve(iuo_Agronomo.Codigo)
		End If
	
	Case "espe_codigo"
		If Not iuo_Especie.Existe(Integer(data),True,sqlca) Then
			This.SetItem(Row,"vari_codigo", Integer(ls_Null))
			dw_2.Setitem(Row, ls_Columna, Integer(ls_Null))
			Return 1
		ELSE
			If Not iuo_AgroEspeProd.Existe(iuo_Agronomo.Codigo, iuo_Especie.Codigo, &
			                               iuo_Productores.Codigo, iuo_Predios.Codigo,True,sqlca) Then
				This.Setitem(Row, ls_Columna, Integer(ls_Null))
				Return 1
			End If
			
			This.GetChild("vari_codigo", idwc_variedad)
			idwc_variedad.SetTransObject(Sqlca)
			If idwc_variedad.Retrieve(iuo_Productores.Codigo, iuo_Predios.Codigo, iuo_Especie.Codigo) = 0 Then
				MessageBox("Atención","Falta Registrar Variedades asociadas a la Especie.")
				idwc_variedad.Insertrow(0)
				Return 1
			End If

			dw_2.GetChild("rdte_cierre", idwc_cierre)
			idwc_cierre.SetTransObject(sqlca)
			If idwc_cierre.Retrieve(integer(data))= 0 Then
				MessageBox("Atención","Falta Registrar Cierres asociados a la Especie.")
				idwc_cierre.insertrow(0)
				Return 1
			End If

			ExistePronostico(ls_Columna,data)
		End If 
	
	Case "prod_codigo"	
		If Not iuo_Productores.Existe(Long(Data),True,sqlca) Then
			This.SetItem(Row,"prpr_codigo", Integer(ls_Null))
			This.SetItem(Row, ls_Columna, Long(ls_Null))
			Return 1
		ELSE
			If Not iuo_AgroEspeProd.existe(iuo_Agronomo.Codigo, iuo_Especie.Codigo, &
			                               iuo_Productores.Codigo, iuo_Predios.Codigo,True,sqlca) Then
				This.Setitem(Row, ls_Columna, Integer(ls_Null))
				Return 1
			End If
			
			This.GetChild("plde_codigo", idwc_pack)
			idwc_pack.SetTransObject(sqlca)
			If idwc_pack.Retrieve(iuo_Productores.Codigo) = 0 Then 
				MessageBox("Atención","Falta Registrar Packing Asociados al Productor.")
				idwc_pack.InsertRow(0)
				Return 1
			End If
			
			This.GetChild("prpr_codigo",idwc_predio)
			idwc_predio.settransobject(sqlca)
			If idwc_predio.Retrieve(iuo_Productores.Codigo) = 0 Then
				MessageBox("Atención","Falta Registrar Predios asociados al Productor.")
				idwc_predio.InsertRow(0)
				Return 1
			End If
			
			This.GetChild("espe_codigo", idwc_especie)
			idwc_especie.SetTransObject(sqlca)
			idwc_especie.Retrieve(iuo_Agronomo.Codigo, Long(Data))

			ExistePronostico(ls_Columna,data)
		End If
		 
	Case "vari_codigo"	
		If Not iuo_Variedades.Existe(iuo_Especie.Codigo,Integer(data),True,Sqlca) Then
			This.SetItem(Row, ls_Columna, Integer(ls_Null))
			Return 1
		ELSE
	 		ObtienePesoCosecha(iuo_Especie.Codigo, iuo_Variedades.Variedad)
			BuscaPesoVariedad(iuo_Especie.Codigo, iuo_Variedades.Variedad)
			
			If id_pesocos = 0 or id_pesostd = 0 Then
				This.SetItem(Row, ls_Columna, Integer(ls_Null))
				Return 1
			Else
				This.Object.rdte_pesenv[1] = id_pesocos
			End If
			ExistePronostico(ls_Columna, Data)
		End If 
	
	Case "prpr_codigo"	
		If Not iuo_Predios.existe(iuo_Productores.Codigo, Integer(Data), sqlca,True) Then
			This.SetItem(Row, ls_Columna, Integer(ls_Null))
			Return 1
		ELSE
			li_productor = GrupoProductor(Long(Data))
			
			If li_productor = 0 Then
				MessageBox("Atención","Predio no posee Grupo Productor Asociado.")
				This.SetItem(Row, ls_Columna, Long(ls_Null))
				Return 1
			ELSE
				If iuo_productores.Existe(iuo_Productores.Codigo,True,sqlca) Then				
					istr_mant.argumento[3] = String(iuo_Productores.Codigo)
					dw_2.Object.prod_codigo[1] = iuo_Productores.Codigo
				End If	
				
				This.Object.plde_codigo[1] = iuo_Predios.Packing
			End If	
			
			If Not iuo_AgroEspeProd.existe(iuo_Agronomo.Codigo, iuo_Especie.Codigo, &
			                               iuo_Productores.Codigo, iuo_Predios.Codigo,True,sqlca) Then
				This.Setitem(Row, ls_Columna, Long(ls_Null))
				Return 1
			End If
			ExistePronostico(ls_Columna,data)
		End If 
		
	Case 	"plde_codigo"
		If Not iuo_Planta.Existe(Long(Data), True, Sqlca) Then
			This.Setitem(Row, ls_Columna, Long(ls_Null))
			istr_mant.argumento[6]=""
			Return 1
		 ELSE
			This.Object.plde_capemb[Row] = iuo_Planta.Capacidad
			This.Object.plde_diapro[Row] = iuo_Planta.DiaProceso
	 		istr_mant.argumento[6] = Data
		 End If
		
	Case 'rdte_seinco'
		If iuo_Semana.Semana(Integer(Data),gstr_tempo.Temporada, dw_2.Object.espe_codigo[1]) Then				
			li_Semana	=	F_NroSemanaAno(iuo_Semana.Lunes)
			If Integer(data) = 53 Then
				If 	li_Semana = 52 Then
					This.SetItem(Row, ls_Columna, Integer(ls_Null))
					MessageBox('Alerta', 'Este año no posee semana 53.', Exclamation!, OK!)
					Return 1
				End If
			End If
			This.Object.rdte_feinco[1] = iuo_Semana.Lunes
		End If
End Choose
end event

event itemerror;Return 1
end event

type cb_estandar from commandbutton within w_plan_pronostico_cosecha
integer x = 4293
integer y = 1048
integer width = 302
integer height = 112
integer taborder = 170
boolean bringtotop = true
integer textsize = -7
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
string text = "Estandar"
end type

event clicked;Long		ll_Productor, ll_Predio, li_Especie, li_Variedad

SetPointer(HourGlass!)

Parent.TriggerEvent("ue_guardar")

ll_Productor		= dw_2.Object.prod_codigo[1]
ll_Predio			= dw_2.Object.prpr_codigo[1]
li_Especie		= dw_2.Object.espe_codigo[1]
li_Variedad		= dw_2.Object.vari_codigo[1]

If Not wf_ValidaDistribucionEstandar(ll_Productor, ll_Predio) Then Return

If MessageBox('Atencion...', "Si existe distribucion se eliminara y cargara la nueva. Desea Continuar", Exclamation!, YesNo!, 2) = 2 Then 
	SetPointer(Arrow!)
	Return
End If

Parent.TriggerEvent("ue_guardar")
If Message.DoubleParm = -1 Then Return

Declare Traspasa Procedure For dbo.Pron_DistribucionEstandar
	@Temporada	=	:gstr_tempo.temporada,
	@Productor		=	:ll_Productor,
	@Predio			=	:ll_Predio,
	@Especie		=	:li_Especie,
	@Variedad		=	:li_Variedad
	Using SQLCA ;

Execute Traspasa;	

If sqlca.SQLCode = -1 Then F_ErrorBaseDatos(SQLCA,"Problema en Carga de Distribucion Estandar" )
Close Traspasa;
Commit;

MessageBox('Atencion...', "Proceso terminado con exito.", Exclamation!, OK!) 

SetPointer(Arrow!)
end event

type cb_produccion from commandbutton within w_plan_pronostico_cosecha
integer x = 4293
integer y = 1172
integer width = 302
integer height = 112
integer taborder = 180
boolean bringtotop = true
integer textsize = -7
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
string text = "Producción"
end type

event clicked;SetPointer(HourGlass!)

Parent.TriggerEvent("ue_guardar")

IF Message.DoubleParm = -1 THEN RETURN

If MessageBox('Atencion...', "Si existe distribucion se eliminara y cargara la nueva. Desea Continuar", Exclamation!, YesNo!, 2) = 2 Then 
	SetPointer(Arrow!)
	Return
End If

istr_Mant.Argumento[1] = String(dw_2.Object.prod_codigo[1])
istr_Mant.Argumento[2] = String(dw_2.Object.prpr_codigo[1])
istr_Mant.Argumento[3] = String(dw_2.Object.espe_codigo[1])
istr_Mant.Argumento[4] = String(dw_2.Object.vari_codigo[1])
 
OpenWithParm(w_selec_distribtemp, istr_Mant)

istr_Mant	= Message.PowerObjectParm

If istr_mant.respuesta <> 0 Then MessageBox('Atencion...', "Proceso terminado con exito.", Exclamation!, OK!) 

SetPointer(Arrow!)
end event

