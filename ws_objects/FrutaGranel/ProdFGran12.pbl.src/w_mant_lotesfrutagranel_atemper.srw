$PBExportHeader$w_mant_lotesfrutagranel_atemper.srw
forward
global type w_mant_lotesfrutagranel_atemper from w_mant_detalle_csd
end type
end forward

global type w_mant_lotesfrutagranel_atemper from w_mant_detalle_csd
integer width = 3579
integer height = 1676
end type
global w_mant_lotesfrutagranel_atemper w_mant_lotesfrutagranel_atemper

type variables
DataWindowChild	idwc_variedad, idwc_predio, idwc_cuartel, idwc_periodofrio, idwc_camaras, &
						idwc_agronomo, idwc_color
						
						
str_mant				istr_mantdes
Boolean				ib_verde, ib_saltofila
end variables

forward prototypes
public function boolean diasdesverd (integer ai_columna, string as_valor)
public subroutine calculafechaestimada (integer ai_columna, string ai_horas)
public subroutine habilitalote ()
end prototypes

public function boolean diasdesverd (integer ai_columna, string as_valor);DateTime		ldt_fechaentrada, ldt_fechasalida
Date			ld_fecha
Time			lt_hora
Decimal		ld_horas

ldt_fechaentrada	=	DateTime(dw_1.Object.lode_fecing[dw_1.GetRow()], &
										dw_1.Object.lode_horing[dw_1.GetRow()])
										

ld_fecha	=	dw_1.Object.lode_fesare[dw_1.GetRow()]
lt_hora	=	dw_1.Object.lode_hosare[dw_1.GetRow()]

CHOOSE CASE ai_columna
	CASE 1
		ld_fecha	=	Date(as_valor)
		
	CASE 2
		lt_hora	=	Time(as_valor)
		
END CHOOSE

IF IsNull(ld_fecha) OR IsNull(lt_hora) THEN
	Return True
END IF

ldt_fechasalida	=	DateTime(ld_fecha, lt_hora)

IF ldt_fechasalida < ldt_fechaentrada THEN
	MessageBox("Error de Datos", "La Fecha y Hora de salida no puede ser menor que " + &
										  "La Fecha y Hora de entrada", StopSign!)
	Return False
	
END IF

DECLARE hrsdif PROCEDURE FOR dba.fgran_horasdiferencia  
        @fechae = :ldt_fechaentrada,   
        @fechas = :ldt_fechasalida  
		  USING sqlca;

EXECUTE hrsdif;

FETCH hrsdif INTO :ld_horas;

CLOSE hrsdif;

dw_1.Object.lode_diades[dw_1.GetRow()]	=	ld_horas

Return True
end function

public subroutine calculafechaestimada (integer ai_columna, string ai_horas);Integer	li_horas, li_temp
DateTime	ldt_fechaingreso, ldt_estimadasalida
Time		lt_hora
Date		ld_fecha

IF ai_columna	=	4 THEN
	IF IsNull(dw_1.Object.lode_fecing[dw_1.GetRow()]) OR &
		IsNull(Time(ai_horas)) THEN
		Return
		
	END IF
	
	ldt_fechaingreso	=	DateTime(dw_1.Object.lode_fecing[dw_1.GetRow()], &
											Time(ai_horas))
										
ELSEIF ai_columna	=	5 THEN
	IF IsNull(Date(ai_horas)) OR &
		IsNull(dw_1.Object.lode_horing[dw_1.GetRow()]) THEN
		Return
		
	END IF
	
	ldt_fechaingreso	=	DateTime(Date(ai_horas), &
											dw_1.Object.lode_horing[dw_1.GetRow()])
										
ELSE
	IF IsNull(dw_1.Object.lode_fecing[dw_1.GetRow()]) OR &
		IsNull(dw_1.Object.lode_horing[dw_1.GetRow()]) THEN
		Return
		
	END IF
	
	ldt_fechaingreso	=	DateTime(dw_1.Object.lode_fecing[dw_1.GetRow()], &
											dw_1.Object.lode_horing[dw_1.GetRow()])
END IF

IF IsNull(ldt_fechaingreso) THEN
	REturn
	
END IF

CHOOSE CASE ai_columna
	CASE 0
		li_temp = Integer(ai_horas)
		IF IsNull(li_temp) THEN li_temp = 0
		li_horas	=	li_horas + li_temp
		
		li_temp	=	dw_1.Object.lode_mohod1[dw_1.GetRow()]
		IF IsNull(li_temp) THEN li_temp = 0
		li_horas	=	li_horas + li_temp
		
		li_temp	=	dw_1.Object.lode_mohod2[dw_1.GetRow()]
		IF IsNull(li_temp) THEN li_temp = 0
		li_horas	=	li_horas + li_temp
		
		li_temp	=	dw_1.Object.lode_mohod3[dw_1.GetRow()]
		IF IsNull(li_temp) THEN li_temp = 0
		li_horas	=	li_horas + li_temp
		
	CASE 1
		li_temp	=	dw_1.Object.lode_hordes[dw_1.GetRow()]
		IF IsNull(li_temp) THEN li_temp = 0
		li_horas	=	li_horas + li_temp
		
		li_temp	=	Integer(ai_horas)
		IF IsNull(li_temp) THEN li_temp = 0
		li_horas	=	li_horas + li_temp
		
		li_temp	=	dw_1.Object.lode_mohod2[dw_1.GetRow()]
		IF IsNull(li_temp) THEN li_temp = 0
		li_horas	=	li_horas + li_temp
		
		li_temp	=	dw_1.Object.lode_mohod3[dw_1.GetRow()]
		IF IsNull(li_temp) THEN li_temp = 0
		li_horas	=	li_horas + li_temp
		
	CASE 2
		li_temp	=	dw_1.Object.lode_hordes[dw_1.GetRow()]
		IF IsNull(li_temp) THEN li_temp = 0
		li_horas	=	li_horas + li_temp
		
		li_temp	=	dw_1.Object.lode_mohod1[dw_1.GetRow()]
		IF IsNull(li_temp) THEN li_temp = 0
		li_horas	=	li_horas + li_temp
		
		li_temp	=	Integer(ai_horas)
		IF IsNull(li_temp) THEN li_temp = 0
		li_horas	=	li_horas + li_temp
		
		li_temp	=	dw_1.Object.lode_mohod3[dw_1.GetRow()]
		IF IsNull(li_temp) THEN li_temp = 0
		li_horas	=	li_horas + li_temp
		
	CASE 3
		li_temp	=	dw_1.Object.lode_hordes[dw_1.GetRow()]
		IF IsNull(li_temp) THEN li_temp = 0
		li_horas	=	li_horas + li_temp
		
		li_temp	=	dw_1.Object.lode_mohod1[dw_1.GetRow()]
		IF IsNull(li_temp) THEN li_temp = 0
		li_horas	=	li_horas + li_temp
		
		li_temp	=	dw_1.Object.lode_mohod2[dw_1.GetRow()]
		IF IsNull(li_temp) THEN li_temp = 0
		li_horas	=	li_horas + li_temp
		
		li_temp	=	Integer(ai_horas)
		IF IsNull(li_temp) THEN li_temp = 0
		li_horas	=	li_horas + li_temp
		
	CASE ELSE
		li_temp	=	dw_1.Object.lode_hordes[dw_1.GetRow()]
		IF IsNull(li_temp) THEN li_temp = 0
		li_horas	=	li_horas + li_temp
		
		li_temp	=	dw_1.Object.lode_mohod1[dw_1.GetRow()]
		IF IsNull(li_temp) THEN li_temp = 0
		li_horas	=	li_horas + li_temp
		
		li_temp	=	dw_1.Object.lode_mohod2[dw_1.GetRow()]
		IF IsNull(li_temp) THEN li_temp = 0
		li_horas	=	li_horas + li_temp
		
		li_temp	=	dw_1.Object.lode_mohod3[dw_1.GetRow()]
		IF IsNull(li_temp) THEN li_temp = 0
		li_horas	=	li_horas + li_temp
END CHOOSE


DECLARE sumahoras PROCEDURE FOR dba.fgran_sumahoras  
		@horas = :li_horas,   
		@fecha = :ldt_fechaingreso
		using sqlca;

Execute sumahoras;

Fetch sumahoras into :ldt_estimadasalida;

Close sumahoras;

lt_hora	=	Time(ldt_estimadasalida)
ld_fecha	=	Date(ldt_estimadasalida)

dw_1.Object.lode_fesaes[dw_1.GetRow()]	=	ld_fecha
dw_1.Object.lode_hosaes[dw_1.GetRow()]	=	lt_hora
	
end subroutine

public subroutine habilitalote ();IF istr_mant.Argumento[30] = '1' THEN
	CHOOSE CASE dw_1.Object.lode_estado[il_fila]
		CASE 0
			dw_1.Object.ccag_codigo.Protect	=	1
			dw_1.Object.lode_fecing.Protect	=	1
			dw_1.Object.lode_horing.Protect	=	1
			dw_1.Object.ccev_codigo.Protect	=	1			
			dw_1.Object.lode_hocuhu.Protect=	1
			dw_1.Object.lode_observ.Protect	=	0
			dw_1.Object.lode_obsdre.Protect	=	0
			dw_1.Object.lode_hordes.Protect	=	0

			dw_1.Object.ccag_codigo.Color	=	RGB(255, 255, 255)
			dw_1.Object.lode_fecing.Color		=	RGB(255, 255, 255)
			dw_1.Object.lode_horing.Color		=	RGB(255, 255, 255)
			dw_1.Object.ccev_codigo.Color		=	RGB(255, 255, 255)
			dw_1.Object.lode_hocuhu.Color	=	RGB(255, 255, 255)
			
			dw_1.Object.lode_observ.Color		=	0
			dw_1.Object.lode_obsdre.Color	=	0
			dw_1.Object.lode_hordes.Color	=	0
			
			
			dw_1.Object.ccag_codigo.BackGround.Color	=	553648127
			dw_1.Object.lode_fecing.BackGround.Color	=	553648127
			dw_1.Object.lode_horing.BackGround.Color	=	553648127
			dw_1.Object.ccev_codigo.BackGround.Color	=	553648127
			dw_1.Object.lode_hocuhu.BackGround.Color=	553648127
			
			dw_1.Object.lode_observ.BackGround.Color	=	RGB(255, 255, 255)
			dw_1.Object.lode_obsdre.BackGround.Color	=	RGB(255, 255, 255)
			dw_1.Object.lode_hordes.BackGround.Color	=	RGB(255, 255, 255)

			dw_1.Object.lode_mohod1.Protect	=	1
			dw_1.Object.lode_mohod2.Protect	=	1
			dw_1.Object.lode_mohod3.Protect	=	1
			dw_1.Object.lode_fesare.Protect	=	1
			dw_1.Object.lode_hosare.Protect	=	1
			
			dw_1.Object.lode_mohod1.Color	=	RGB(255, 255, 255)
			dw_1.Object.lode_mohod2.Color	=	RGB(255, 255, 255)
			dw_1.Object.lode_mohod3.Color	=	RGB(255, 255, 255)
			dw_1.Object.lode_fesare.Color		=	RGB(255, 255, 255)
			dw_1.Object.lode_hosare.Color	=	RGB(255, 255, 255)
			
			dw_1.Object.lode_mohod1.BackGround.Color	=	553648127
			dw_1.Object.lode_mohod2.BackGround.Color	=	553648127
			dw_1.Object.lode_mohod3.BackGround.Color	=	553648127
			dw_1.Object.lode_fesare.BackGround.Color		=	553648127
			dw_1.Object.lode_hosare.BackGround.Color		=	553648127
			
		CASE 1
			IF IsNull(dw_1.Object.lode_mohod1[il_fila]) THEN
				dw_1.Object.lode_mohod1.Protect					=	0
				dw_1.Object.lode_mohod1.Color					=	0
				dw_1.Object.lode_mohod1.BackGround.Color	=	RGB(255, 255, 255)
			ELSE
				dw_1.Object.lode_mohod1.Protect					=	1
				dw_1.Object.lode_mohod1.BackGround.Color	=	RGB(255,255,255)
				dw_1.Object.lode_mohod1.BackGround.Color	=	553648127
			END IF
				
			IF IsNull(dw_1.Object.lode_mohod2[il_fila]) THEN
				dw_1.Object.lode_mohod2.Protect					=	0
				dw_1.Object.lode_mohod2.Color					=	0
				dw_1.Object.lode_mohod2.BackGround.Color	=	RGB(255, 255, 255)
			ELSE
				dw_1.Object.lode_mohod2.Protect					=	1
				dw_1.Object.lode_mohod2.BackGround.Color	=	RGB(255,255,255)
				dw_1.Object.lode_mohod2.BackGround.Color	=	553648127
			END IF
				
			IF IsNull(dw_1.Object.lode_mohod3[il_fila]) THEN
				dw_1.Object.lode_mohod3.Protect					=	0
				dw_1.Object.lode_mohod3.Color					=	0
				dw_1.Object.lode_mohod3.BackGround.Color	=	RGB(255, 255, 255)
			ELSE
				dw_1.Object.lode_mohod3.Protect					=	1
				dw_1.Object.lode_mohod3.BackGround.Color	=	RGB(255,255,255)
				dw_1.Object.lode_mohod3.BackGround.Color	=	553648127
			END IF
			
			dw_1.Object.lode_fesare.Protect	=	0
			dw_1.Object.lode_hosare.Protect	=	0
			dw_1.Object.lode_fesare.Color		=	0
			dw_1.Object.lode_hosare.Color	=	0
			dw_1.Object.lode_fesare.BackGround.Color	=	RGB(255, 255, 255)
			dw_1.Object.lode_hosare.BackGround.Color	=	RGB(255, 255, 255)
			
			dw_1.Object.ccag_codigo.Protect	=	1
			dw_1.Object.lode_observ.Protect	=	1
			dw_1.Object.lode_obsdre.Protect	=	1
			dw_1.Object.lode_hocuhu.Protect	=	1
			dw_1.Object.lode_fecing.Protect	=	1
			dw_1.Object.lode_horing.Protect	=	1
			dw_1.Object.lode_hordes.Protect	=	1
			dw_1.Object.ccev_codigo.Protect	=	1
			
			dw_1.Object.ccag_codigo.BackGround.Color	=	RGB(255,255,255)
			dw_1.Object.lode_observ.BackGround.Color	=	RGB(255,255,255)
			dw_1.Object.lode_obsdre.BackGround.Color	=	RGB(255,255,255)
			dw_1.Object.lode_hocuhu.BackGround.Color=	RGB(255,255,255)
			dw_1.Object.lode_fecing.BackGround.Color	=	RGB(255,255,255)
			dw_1.Object.lode_horing.BackGround.Color	=	RGB(255,255,255)
			dw_1.Object.lode_hordes.BackGround.Color	=	RGB(255,255,255)
			dw_1.Object.ccev_codigo.BackGround.Color	=	RGB(255,255,255)
			
			dw_1.Object.ccag_codigo.BackGround.Color	=	553648127
			dw_1.Object.lode_observ.BackGround.Color	=	553648127
			dw_1.Object.lode_obsdre.BackGround.Color	=	553648127
			dw_1.Object.lode_hocuhu.BackGround.Color=	553648127
			dw_1.Object.lode_fecing.BackGround.Color	=	553648127
			dw_1.Object.lode_horing.BackGround.Color	=	553648127
			dw_1.Object.lode_hordes.BackGround.Color	=	553648127
			dw_1.Object.ccev_codigo.BackGround.Color	=	553648127
			
		CASE 2
			dw_1.Object.ccag_codigo.Protect	=	1
			dw_1.Object.lode_observ.Protect	=	1
			dw_1.Object.lode_obsdre.Protect	=	1
			dw_1.Object.lode_hocuhu.Protect	=	1
			dw_1.Object.lode_fecing.Protect	=	1
			dw_1.Object.lode_horing.Protect	=	1
			dw_1.Object.lode_hordes.Protect	=	1
			dw_1.Object.lode_mohod1.Protect	=	1
			dw_1.Object.lode_mohod2.Protect	=	1
			dw_1.Object.lode_mohod3.Protect	=	1
			dw_1.Object.lode_fesare.Protect	=	1
			dw_1.Object.lode_hosare.Protect	=	1
			dw_1.Object.ccev_codigo.Protect	=	1

			dw_1.Object.ccag_codigo.Color	=	RGB(255,255,255)
			dw_1.Object.lode_observ.Color		=	RGB(255,255,255)
			dw_1.Object.lode_obsdre.Color	=	RGB(255,255,255)
			dw_1.Object.lode_hocuhu.Color	=	RGB(255,255,255)
			dw_1.Object.lode_fecing.Color		=	RGB(255,255,255)
			dw_1.Object.lode_horing.Color		=	RGB(255,255,255)
			dw_1.Object.lode_hordes.Color	=	RGB(255,255,255)
			dw_1.Object.lode_mohod1.Color	=	RGB(255,255,255)
			dw_1.Object.lode_mohod2.Color	=	RGB(255,255,255)
			dw_1.Object.lode_mohod3.Color	=	RGB(255,255,255)
			dw_1.Object.lode_fesare.Color		=	RGB(255,255,255)
			dw_1.Object.lode_hosare.Color	=	RGB(255,255,255)
			dw_1.Object.ccev_codigo.Color		=	RGB(255,255,255)
			
			
			dw_1.Object.ccag_codigo.BackGround.Color	=	553648127
			dw_1.Object.lode_observ.BackGround.Color	=	553648127
			dw_1.Object.lode_obsdre.BackGround.Color	=	553648127
			dw_1.Object.lode_hocuhu.BackGround.Color=	553648127
			dw_1.Object.lode_fecing.BackGround.Color	=	553648127
			dw_1.Object.lode_horing.BackGround.Color	=	553648127
			dw_1.Object.lode_hordes.BackGround.Color	=	553648127
			dw_1.Object.lode_mohod1.BackGround.Color	=	553648127
			dw_1.Object.lode_mohod2.BackGround.Color	=	553648127
			dw_1.Object.lode_mohod3.BackGround.Color	=	553648127
			dw_1.Object.lode_fesare.BackGround.Color	=	553648127
			dw_1.Object.lode_hosare.BackGround.Color	=	553648127
			dw_1.Object.ccev_codigo.BackGround.Color	=	553648127
	END CHOOSE
ELSE
		dw_1.Object.ccag_codigo.Protect	=	1
		dw_1.Object.lode_fecing.Protect	=	1
		dw_1.Object.lode_horing.Protect	=	1
		dw_1.Object.ccev_codigo.Protect	=	1
		dw_1.Object.lode_hocuhu.Protect=	1
		dw_1.Object.lode_observ.Protect	=	0
		dw_1.Object.lode_obsdre.Protect	=	0
		dw_1.Object.lode_hordes.Protect	=	0
		
		dw_1.Object.ccag_codigo.Color=	Rgb(255,255,255)
		dw_1.Object.lode_fecing.Color	=	Rgb(255,255,255)
		dw_1.Object.lode_horing.Color	=	Rgb(255,255,255)
		dw_1.Object.ccev_codigo.Color	=	Rgb(255,255,255)
		dw_1.Object.lode_hocuhu.Color=	Rgb(255,255,255)
		
		dw_1.Object.lode_observ.Color		=	0
		dw_1.Object.lode_obsdre.Color	=	0
		dw_1.Object.lode_hordes.Color	=	0

		
		dw_1.Object.ccag_codigo.BackGround.Color	=	553648127
		dw_1.Object.lode_fecing.BackGround.Color	=	553648127
		dw_1.Object.lode_horing.BackGround.Color	=	553648127
		dw_1.Object.ccev_codigo.BackGround.Color	=	553648127
		dw_1.Object.lode_hocuhu.BackGround.Color	=	553648127
		
		dw_1.Object.lode_observ.BackGround.Color	=	RGB(255, 255, 255)
		dw_1.Object.lode_obsdre.BackGround.Color	=	RGB(255, 255, 255)
		dw_1.Object.lode_hordes.BackGround.Color	=	RGB(255, 255, 255)	
END IF

IF IsNull(dw_1.Object.ccag_codigo[il_fila]) OR dw_1.Object.ccag_codigo[il_fila] = 0 THEN
	dw_1.Object.ccag_codigo.Protect	=	0
	dw_1.Object.lode_observ.Protect	=	0
	dw_1.Object.lode_obsdre.Protect	=	0
	dw_1.Object.lode_hocuhu.Protect	=	0
	dw_1.Object.lode_hordes.Protect	=	0
	dw_1.Object.ccev_codigo.Protect	=	0

	dw_1.Object.ccag_codigo.Color	=	0
	dw_1.Object.lode_observ.Color		=	0	
	dw_1.Object.lode_obsdre.Color	=	0
	dw_1.Object.lode_hocuhu.Color	=	0
	dw_1.Object.lode_hordes.Color	=	0
	dw_1.Object.ccev_codigo.Color		=	0
	
	dw_1.Object.ccag_codigo.BackGround.Color	=	RGB(255, 255, 255)
	dw_1.Object.lode_observ.BackGround.Color	=	RGB(255, 255, 255)
	dw_1.Object.lode_obsdre.BackGround.Color	=	RGB(255, 255, 255)
	dw_1.Object.lode_hocuhu.BackGround.Color=	RGB(255, 255, 255)
	dw_1.Object.lode_hordes.BackGround.Color	=	RGB(255, 255, 255)
	dw_1.Object.ccev_codigo.BackGround.Color	=	RGB(255, 255, 255)
END IF
end subroutine

on w_mant_lotesfrutagranel_atemper.create
call super::create
end on

on w_mant_lotesfrutagranel_atemper.destroy
call super::destroy
end on

event ue_recuperadatos;call super::ue_recuperadatos;Time			lt_hora
Date			ld_fecha
DateTime		ldt_FechaSistema
Integer		li_estado

dw_1.GetChild("vari_codigo", idwc_variedad)
idwc_variedad.SetTransObject(SQLCa)
idwc_variedad.Retrieve(dw_1.Object.lote_espcod[il_fila])

dw_1.GetChild("prbr_codpre", idwc_predio)
idwc_predio.SetTransObject(SQLCa)
idwc_predio.Retrieve(dw_1.Object.prod_codigo[il_fila])

dw_1.GetChild("prcc_codigo", idwc_cuartel)
idwc_cuartel.SetTransObject(SQLCa)
idwc_cuartel.Retrieve(dw_1.Object.prod_codigo[il_fila], dw_1.Object.prbr_codpre[il_fila])

dw_1.GetChild("pefr_codigo", idwc_periodofrio)
idwc_periodofrio.SetTransObject(SQLCa)
idwc_periodofrio.Retrieve(dw_1.Object.frio_tipofr[il_fila])

dw_1.GetChild("cama_codigo", idwc_camaras)
idwc_camaras.SetTransObject(SQLCa)
idwc_camaras.Retrieve(dw_1.Object.lote_pltcod[il_fila])

dw_1.GetChild("ccag_codigo", idwc_agronomo)
idwc_agronomo.SetTransObject(SQLCa)
idwc_agronomo.Retrieve()

dw_1.GetChild("ccev_codigo", idwc_color)
idwc_color.SetTransObject(SQLCa)
idwc_color.Retrieve(dw_1.Object.lote_espcod[il_fila])

istr_mantdes.Argumento[01]			=	String(dw_1.Object.lote_pltcod[il_fila])
istr_mantdes.Argumento[02]			=	String(dw_1.Object.lote_espcod[il_fila])
istr_mantdes.Argumento[03]			=	String(dw_1.Object.lote_codigo[il_fila])
istr_mantdes.Argumento[04]			=	String(dw_1.Object.zona_codigo[il_fila])
istr_mantdes.Argumento[05]			=	String(dw_1.Object.ccag_codigo[il_fila])
istr_mantdes.Argumento[06]			=	String(dw_1.Object.lode_observ[il_fila])
istr_mantdes.Argumento[07]			=	String(dw_1.Object.lode_obsdre[il_fila])
istr_mantdes.Argumento[08]			=	String(dw_1.Object.lode_hocuhu[il_fila])
istr_mantdes.Argumento[09]			=	String(dw_1.Object.lode_fecing[il_fila])
istr_mantdes.Argumento[10]			=	String(dw_1.Object.lode_horing[il_fila])
istr_mantdes.Argumento[11]			=	String(dw_1.Object.lode_hordes[il_fila])
istr_mantdes.Argumento[12]			=	String(dw_1.Object.lode_mohod1[il_fila])
istr_mantdes.Argumento[13]			=	String(dw_1.Object.lode_mousu1[il_fila]) 
istr_mantdes.Argumento[14]			=	String(dw_1.Object.lode_mohod2[il_fila])
istr_mantdes.Argumento[15]			=	String(dw_1.Object.lode_mousu2[il_fila])
istr_mantdes.Argumento[16]			=	String(dw_1.Object.lode_mohod3[il_fila])
istr_mantdes.Argumento[17]			=	String(dw_1.Object.lode_mousu3[il_fila])
istr_mantdes.Argumento[18]			=	String(dw_1.Object.lode_fesaes[il_fila])
istr_mantdes.Argumento[19]			=	String(dw_1.Object.lode_hosaes[il_fila])
istr_mantdes.Argumento[20]			=	String(dw_1.Object.lode_fesare[il_fila])
istr_mantdes.Argumento[21]			=	String(dw_1.Object.lode_hosare[il_fila])
istr_mantdes.Argumento[22]			=	String(dw_1.Object.lode_diades[il_fila])
istr_mantdes.Argumento[23]			=	String(dw_1.Object.lode_estado[il_fila])
istr_mantdes.Argumento[24]			=	String(dw_1.Object.ccev_codigo[il_fila])

dw_1.Object.bloqueo[il_fila]		=	Integer(istr_mant.Argumento[30])
dw_1.AcceptText()
habilitalote()
calculafechaestimada(0, '24')
ib_saltofila							=	False
end event

event closequery;call super::closequery;Integer	li_estado, li_cont
Date		lt_fecha
String	ls_fecha, ls_mensaje, ls_colu[]

IF ib_verde THEN
	IF Isnull(dw_1.Object.ccag_codigo[il_fila]) OR dw_1.Object.ccag_codigo[il_fila] = 0 THEN
		li_cont ++
		ls_mensaje 			= ls_mensaje + "~nCódigo Agrónomo"
		ls_colu[li_cont]	= "ccag_codigo"
	END IF
	
	IF Isnull(dw_1.Object.ccev_codigo[il_fila]) OR dw_1.Object.ccev_codigo[il_fila] = 0 THEN
		li_cont ++
		ls_mensaje 			= ls_mensaje + "~nCódigo Color"
		ls_colu[li_cont]	= "ccev_codigo"
	END IF
	
	IF Isnull(dw_1.Object.lode_hordes[il_fila]) OR dw_1.Object.lode_hordes[il_fila] = 0 THEN
		li_cont ++
		ls_mensaje 			= ls_mensaje + "~nHoras Desverdizado"
		ls_colu[li_cont]	= "lode_hordes"
	END IF
	
	IF li_cont > 0 THEN
		MessageBox("Error de Consistencia", "Falta el ingreso de:~n" + ls_mensaje + ".", StopSign!, Ok!)
		dw_1.SetColumn(ls_colu[1])
		dw_1.SetFocus()
		
		RETURN 1
	END IF
	
	li_estado								=	dw_1.Object.lode_estado[il_fila]
	dw_1.Object.bloqueo[il_fila]		=	0
	
	CHOOSE CASE li_estado
		CASE 0
			lt_fecha	=	dw_1.Object.lode_fecing[il_fila]
			ls_fecha	=	String(dw_1.Object.lode_fecing[il_fila])
			
			IF NOT IsNull(lt_fecha) AND IsDate(ls_fecha) THEN
				dw_1.Object.lode_estado[il_fila]	=	1
				dw_1.Object.lode_usuing[il_fila]	=	gstr_us.Nombre
				
			END IF
			
		CASE 1
			lt_fecha	=	dw_1.Object.lode_fesare[il_fila]
			ls_fecha	=	String(dw_1.Object.lode_fesare[il_fila])
			
			IF NOT IsNull(lt_fecha) AND IsDate(ls_fecha) THEN
				dw_1.Object.lode_estado[il_fila]	=	2
				
			END IF
			
	END CHOOSE
	
ELSE
	dw_1.Object.bloqueo[il_fila]		=	0
	
END IF
end event

event ue_deshace;call super::ue_deshace;dw_1.Object.lote_pltcod[il_fila]	=	Long(istr_mantdes.Argumento[01])
dw_1.Object.lote_espcod[il_fila]	=	Integer(istr_mantdes.Argumento[02])
dw_1.Object.lote_codigo[il_fila]	=	Long(istr_mantdes.Argumento[03])
dw_1.Object.zona_codigo[il_fila]	=	Integer(istr_mantdes.Argumento[04])
dw_1.Object.ccag_codigo[il_fila]	=	Integer(istr_mantdes.Argumento[05])
dw_1.Object.lode_observ[il_fila]	=	istr_mantdes.Argumento[06]
dw_1.Object.lode_obsdre[il_fila]	=	istr_mantdes.Argumento[07]
dw_1.Object.lode_hocuhu[il_fila]	=	Integer(istr_mantdes.Argumento[08])
dw_1.Object.lode_fecing[il_fila]	=	Date(istr_mantdes.Argumento[09])
dw_1.Object.lode_horing[il_fila] =	Time(istr_mantdes.Argumento[10])
dw_1.Object.lode_hordes[il_fila]	=	Integer(istr_mantdes.Argumento[11])
dw_1.Object.lode_mohod1[il_fila]	=	Integer(istr_mantdes.Argumento[12])
dw_1.Object.lode_mousu1[il_fila] =	istr_mantdes.Argumento[13]
dw_1.Object.lode_mohod2[il_fila]	=	Integer(istr_mantdes.Argumento[14])
dw_1.Object.lode_mousu2[il_fila]	=	istr_mantdes.Argumento[15]
dw_1.Object.lode_mohod3[il_fila]	=	Integer(istr_mantdes.Argumento[16])
dw_1.Object.lode_mousu3[il_fila]	=	istr_mantdes.Argumento[17]
dw_1.Object.lode_fesaes[il_fila]	=	Date(istr_mantdes.Argumento[18])
dw_1.Object.lode_hosaes[il_fila]	=	Time(istr_mantdes.Argumento[19])
dw_1.Object.lode_fesare[il_fila]	=	Date(istr_mantdes.Argumento[20])
dw_1.Object.lode_hosare[il_fila]	=	Time(istr_mantdes.Argumento[21])
dw_1.Object.lode_diades[il_fila]	=	Dec(istr_mantdes.Argumento[22])
dw_1.Object.lode_estado[il_fila]	=	Integer(istr_mantdes.Argumento[23])
dw_1.Object.ccev_codigo[il_fila]	=	Integer(istr_mantdes.Argumento[24])
end event

event open;Integer	li_fila
x	= 100
y	= 450

This.Icon	=	Gstr_apl.Icono

PostEvent("ue_recuperadatos")

istr_mant 		= Message.PowerObjectParm
li_fila			=	istr_mant.dw.GetRow()
ib_saltofila	=	True

dw_1.SetTransObject(sqlca)
istr_mant.dw.ShareData(dw_1)

il_fila	=	li_fila
dw_1.SetRedraw(True)
dw_1.SetRow(il_fila)
dw_1.ScrollToRow(il_fila)
dw_1.SetRedraw(True)
end event

type pb_ultimo from w_mant_detalle_csd`pb_ultimo within w_mant_lotesfrutagranel_atemper
boolean visible = false
end type

type pb_siguiente from w_mant_detalle_csd`pb_siguiente within w_mant_lotesfrutagranel_atemper
boolean visible = false
end type

type pb_anterior from w_mant_detalle_csd`pb_anterior within w_mant_lotesfrutagranel_atemper
boolean visible = false
end type

type pb_primero from w_mant_detalle_csd`pb_primero within w_mant_lotesfrutagranel_atemper
boolean visible = false
end type

type pb_cancela from w_mant_detalle_csd`pb_cancela within w_mant_lotesfrutagranel_atemper
integer x = 3127
integer y = 308
end type

event pb_cancela::clicked;istr_mant.respuesta = 2

ib_verde	=	False

CloseWithReturn(Parent, istr_mant)
end event

type pb_acepta from w_mant_detalle_csd`pb_acepta within w_mant_lotesfrutagranel_atemper
integer x = 3127
integer y = 128
end type

event pb_acepta::clicked;ib_verde	=	True

istr_mant.respuesta = 1

IF istr_mant.agrega THEN
	Parent.TriggerEvent("ue_nuevo")
ELSE
	CloseWithReturn(Parent, istr_mant)
END IF
end event

type pb_salir from w_mant_detalle_csd`pb_salir within w_mant_lotesfrutagranel_atemper
integer x = 3127
integer y = 488
end type

type dw_1 from w_mant_detalle_csd`dw_1 within w_mant_lotesfrutagranel_atemper
integer x = 96
integer y = 112
integer width = 2930
integer height = 1392
string dataobject = "dw_mant_spro_lotesfrutagranel_atemper"
end type

event dw_1::itemchanged;call super::itemchanged;String	ls_columna
Integer	li_null

SetNull(li_null)
ls_columna	=	dwo.Name

CHOOSE CASE ls_columna
	CASE "lode_horing"
		calculafechaestimada(4, data)
		
	CASE "lode_fecing"
		calculafechaestimada(5, data)
		
	CASE "lode_hordes"
		calculafechaestimada(0, data)
		
	CASE "lode_mohod1"
		This.Object.lode_mousu1[row]	=	gstr_us.Nombre
		calculafechaestimada(1, data)
		
	CASE "lode_mohod2"
		This.Object.lode_mousu2[row]	=	gstr_us.Nombre
		calculafechaestimada(2, data)
		
	CASE "lode_mohod3"
		This.Object.lode_mousu3[row]	=	gstr_us.Nombre
		calculafechaestimada(3, data)
		
	CASE "lode_fesare"
		IF NOT DiasDesverd(1, data) THEN
			This.Object.lode_fesare[Row]	=	Date(li_null)
			Return 1
		END IF
		
	CASE "lode_hosare"
		IF NOT DiasDesverd(2, data) THEN
			This.Object.lode_hosare[Row]	=	Time(li_null)
			Return 1
		END IF
		
END CHOOSE
end event

event dw_1::itemerror;call super::itemerror;Return 1
end event

event dw_1::rowfocuschanging;call super::rowfocuschanging;IF NOT ib_saltofila THEN Return 1
end event

