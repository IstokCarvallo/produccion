$PBExportHeader$uo_parammadurez.sru
$PBExportComments$Objeto de Validación de Parámetros de Madurez según Especie y Variedad para Control de Calidad
forward
global type uo_parammadurez from nonvisualobject
end type
end forward

global type uo_parammadurez from nonvisualobject
end type
global uo_parammadurez uo_parammadurez

type variables
Integer	Especie, Variedad, Color, SolidosSol, PresEcuatorial,&
			PresHombros, PresApice, Almidon, Acuoso, Mohoso, Acidez, Harinosidad, &
			Grasitud, SolFinales, MateriaSeca, Calibre, Aceite, Humedad
			
Decimal{2}	solsolmin, solsolmax, preecumin, preecumax, prehommin, prehommax,&
				preapimin, preapimax, tesalmmin, tesalmmax, acidezmin, acidezmax,&
				solfinmin, solfinmax, matsecmin, matsecmax, aceitemin, aceitemax,&
				humedamin, humedamax
end variables

forward prototypes
public function boolean existe (integer ai_especie, integer ai_grupo, integer ai_subgrupo, integer ai_variedad, boolean ab_mensaje, transaction at_transaccion)
public function boolean corresponderango (string as_grupocolumnas, string as_valor)
end prototypes

public function boolean existe (integer ai_especie, integer ai_grupo, integer ai_subgrupo, integer ai_variedad, boolean ab_mensaje, transaction at_transaccion);Boolean	lb_Retorno = True

SELECT	espe_codigo, vari_codigo, copam_grucal, copam_colorf, copam_solsol, copam_preecu,
			copam_prehom, copam_preapi, copam_tesalm, copam_coracu, copam_cormoh,
			copam_acidez, copam_harino, copam_grasit, copam_solfin, copam_matsec,
			copam_aceite, copam_humeda,
			IsNull(copam_ssrmin,0), IsNull(copam_ssrmax,0),
			IsNull(copam_permin,0), IsNull(copam_permax,0),
			IsNull(copam_phrmin,0), IsNull(copam_phrmax,0),
			IsNull(copam_parmin,0), IsNull(copam_parmax,0),
			IsNull(copam_tarmin,0), IsNull(copam_tarmax,0),
			IsNull(copam_acrmin,0), IsNull(copam_acrmax,0),
			IsNull(copam_sfrmin,0), IsNull(copam_sfrmax,0),
			IsNull(copam_msrmin,0), IsNull(copam_msrmax,0),
			IsNull(copam_actmin,0), IsNull(copam_actmax,0),
			IsNull(copam_hummin,0), IsNull(copam_hummax,0)
	INTO	:Especie, :Variedad, :Calibre, :Color, :SolidosSol, :PresEcuatorial,
			:PresHombros, :PresApice, :Almidon, :Acuoso, :Mohoso, :Acidez, 
			:Harinosidad, :Grasitud,  :SolFinales, :MateriaSeca,
			:Aceite, :Humedad,
			:solsolmin, :solsolmax, :preecumin, :preecumax,
			:prehommin, :prehommax,	:preapimin, :preapimax,
			:tesalmmin, :tesalmmax,	:acidezmin, :acidezmax,
			:solfinmin, :solfinmax,	:matsecmin, :matsecmax,
			:aceitemin, :aceitemax, :humedamin, :humedamax
	FROM	dba.spro_contparamadurez
	WHERE	espe_codigo	=	:ai_Especie
	AND   isnull(grva_codigo,-1) = :ai_grupo
	AND   isnull(grva_codsub,-1) = :ai_grupo
	AND	isnull(vari_codigo,-1) = :ai_Variedad
	USING at_Transaccion;

IF at_Transaccion.SQLCode = -1 THEN
	F_ErrorBaseDatos(at_Transaccion, "Lectura de Tabla Color de Fondo")

	lb_Retorno	=	False
ELSEIF at_Transaccion.SQLCode = 100 THEN
	IF ab_Mensaje THEN
		MessageBox("Atención", "Parámetros de Madurez de Especie " + String(ai_Especie, '00') + &
		  			  ", no ha sido registrado.~r~rIngrese o seleccione otro Código.")
	END IF
	lb_Retorno	=	False
END IF

RETURN lb_Retorno
end function

public function boolean corresponderango (string as_grupocolumnas, string as_valor);Boolean		lb_Retorno = False
Decimal{2}	ld_ValMinimo, ld_ValMaximo

CHOOSE CASE as_GrupoColumnas
	CASE "Sólido Soluble"
		ld_ValMinimo = solsolmin
		ld_ValMaximo = solsolmax

	CASE "Presión Ecuatorial"
		ld_ValMinimo = preecumin
		ld_ValMaximo = preecumax

	CASE "Presión Hombros"
		ld_ValMinimo = prehommin
		ld_ValMaximo = prehommax

	CASE "Presión Apice"
		ld_ValMinimo = preapimin
		ld_ValMaximo = preapimax

	CASE "Test Almidón"
		ld_ValMinimo = tesalmmin
		ld_ValMaximo = tesalmmax

  CASE "Acidez"
		ld_ValMinimo = acidezmin
		ld_ValMaximo = acidezmax

  CASE "Solidos Finales"
		ld_ValMinimo = solfinmin
		ld_ValMaximo = solfinmax

  CASE "Materia Seca"
		ld_ValMinimo = matsecmin
		ld_ValMaximo = matsecmax

  CASE "Aceite"
		ld_ValMinimo = aceitemin
		ld_ValMaximo = aceitemax
		
  CASE "Humedad"
		ld_ValMinimo = humedamin
		ld_ValMaximo = humedamax
		
END CHOOSE

IF Dec(as_Valor) <> 0 AND Dec(as_Valor) < ld_ValMinimo OR Dec(as_Valor) > ld_ValMaximo THEN
		MessageBox("Atención", "Valor registrado no corresponde al definido ~r" + &
						"en Parámetros de Madurez Cosecha " + &
						"para Especie y Variedad indicadas.~r~rIngrese otro Valor.")
ELSE
	lb_Retorno	=	True
END IF

RETURN lb_Retorno
end function

on uo_parammadurez.create
call super::create
TriggerEvent( this, "constructor" )
end on

on uo_parammadurez.destroy
TriggerEvent( this, "destructor" )
call super::destroy
end on

