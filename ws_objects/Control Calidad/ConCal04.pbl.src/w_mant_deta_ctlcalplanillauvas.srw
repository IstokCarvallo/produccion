$PBExportHeader$w_mant_deta_ctlcalplanillauvas.srw
forward
global type w_mant_deta_ctlcalplanillauvas from w_mant_detalle
end type
type tab_1 from tab within w_mant_deta_ctlcalplanillauvas
end type
type tabpage_5 from userobject within tab_1
end type
type dw_id from uo_dw within tabpage_5
end type
type tabpage_5 from userobject within tab_1
dw_id dw_id
end type
type tabpage_1 from userobject within tab_1
end type
type dw_embalaje from uo_dw within tabpage_1
end type
type tabpage_1 from userobject within tab_1
dw_embalaje dw_embalaje
end type
type tabpage_2 from userobject within tab_1
end type
type dw_calidad from uo_dw within tabpage_2
end type
type tabpage_2 from userobject within tab_1
dw_calidad dw_calidad
end type
type tabpage_3 from userobject within tab_1
end type
type dw_condicion from uo_dw within tabpage_3
end type
type tabpage_3 from userobject within tab_1
dw_condicion dw_condicion
end type
type tabpage_4 from userobject within tab_1
end type
type dw_resolucion from uo_dw within tabpage_4
end type
type tabpage_4 from userobject within tab_1
dw_resolucion dw_resolucion
end type
type tab_1 from tab within w_mant_deta_ctlcalplanillauvas
tabpage_5 tabpage_5
tabpage_1 tabpage_1
tabpage_2 tabpage_2
tabpage_3 tabpage_3
tabpage_4 tabpage_4
end type
end forward

global type w_mant_deta_ctlcalplanillauvas from w_mant_detalle
integer width = 3593
integer height = 2068
boolean controlmenu = true
tab_1 tab_1
end type
global w_mant_deta_ctlcalplanillauvas w_mant_deta_ctlcalplanillauvas

type variables
Integer	ii_Punuba, ii_Punura, ii_Pununi
String	is_Columna
Integer ii_cliente, ii_plde,ii_cclo,ii_ccpe,ii_valor

DataWindowChild idwc_clientes,idwc_plantas, idwc_predio, idwc_cuartel
DataWindow	dw_2, dw_3, dw_4, dw_5,dw_6

uo_ctlcaldanoespecie		iuo_ctlcaldanoespecie
uo_prodpredio				iuo_Predio
uo_prodCuarteles				iuo_Cuartel
end variables

forward prototypes
public function boolean valiracimo (string columna, string valor)
public subroutine sumaformaracimo (string as_columna, string valor, integer numero)
public function boolean sumpesoracim (integer ai_tipo, string as_valor)
public function boolean sumaracimo (integer ai_tipo, string as_valor)
public function boolean cienxciento (string columna, string valor)
public subroutine datosembalaje (string columna, string valor)
public function boolean datosresolucion (string columna, string valor)
public function string datosresolucioncaja (string columna, string valor)
public function boolean duplicado (string campo, integer tipo)
public subroutine pesoproba (string columna, string valor)
public subroutine promediobayas (string columna, string valor)
public function boolean resulcaj (integer ai_resul)
public function boolean sumadeshi (integer ai_tipo, string as_valor)
public function boolean existedañoinsecto (string data)
public function boolean existenvaloresracimo (integer ai_punura, integer ai_punuba, integer ai_pununi)
public function boolean cienporciento (string columna, string valor)
public function integer validapeso (string columna, string valor)
public function boolean existe_causal (integer ai_causal)
public function boolean existe_matextraña (string data)
public function boolean valida_causal (string as_columna, integer ai_valor)
public subroutine wf_nuevo ()
public function boolean causales (integer ai_causa)
public function boolean causales2 (integer ai_causa)
protected function long existepallet (integer ai_cliente, integer ai_planta, long al_lote, long al_pallet)
protected subroutine wf_prediopallet (integer ai_cliente, integer ai_planta, long al_lote, long al_pallet)
end prototypes

public function boolean valiracimo (string columna, string valor);Integer	li_valor1, li_valor2, li_valor3, li_suma
String	ls_Null

SetNull(ls_Null)

CHOOSE CASE Columna
		
	CASE "ccpd_punuba"
		li_valor1	=	Integer(valor)
		li_Suma		=	li_valor1
		
	CASE "ccpd_pununi"
		
		li_valor2	=	Integer(valor)
		li_suma     =  li_valor1 + li_valor2 

//	CASE "ccpd_punura"
//		li_valor3 	=	Tab_1.TabPage_3.dw_condicion.Object.ccpd_punura[il_fila]		
//	
END CHOOSE
		IF li_suma > 0  THEN 
			Messagebox("Atención","Debe Ingresar obligatoriamente Nro de Racimo")
			dw_4.Setfocus()
			RETURN FALSE
		END IF 	


RETURN TRUE

end function

public subroutine sumaformaracimo (string as_columna, string valor, integer numero);Integer	li_Valor1, li_Valor2, li_Suma

IF Numero	=	1	THEN		
	
	CHOOSE CASE	as_Columna
		CASE	"ccpd_forade"
			li_Valor1	=	Integer(Valor)

			li_Valor2		=	Tab_1.TabPage_2.dw_Calidad.Object.ccpd_foraap[il_Fila]
			
		CASE	"ccpd_foraap"
			li_Valor1		=	Tab_1.TabPage_2.dw_Calidad.Object.ccpd_forade[il_Fila]
			li_Valor2	=	Integer(Valor)		
			
	END CHOOSE
	
ELSEIF Numero	=	2	THEN
	
	CHOOSE CASE as_Columna
			
		CASE	"ccpd_pesras"
			li_Valor1	=	Integer(Valor)
			li_Valor2	=	Tab_1.TabPage_2.dw_Calidad.Object.ccpd_pesrab[il_Fila]
		CASE	"ccpd_pesrab"
			li_Valor1	=	Tab_1.TabPage_2.dw_Calidad.Object.ccpd_pesras[il_Fila]
			li_Valor2	=	Integer(Valor)
			
	END CHOOSE	
	
END IF


li_Suma	=	li_Valor1	+	li_Valor2

IF li_Suma	>	100 THEN
	
	MessageBox("Atención","Sumatoria de Forma de Racimo~r" +&
				  " no debe exceder 100 ")						
END IF						
end subroutine

public function boolean sumpesoracim (integer ai_tipo, string as_valor);Decimal 	ldc_sobre, ldc_bajo, ldc_Suma

ldc_Sobre		=	Tab_1.TabPage_2.dw_Calidad.Object.ccpd_pesras[il_Fila]
ldc_Bajo			=	Tab_1.TabPage_2.dw_Calidad.Object.ccpd_pesrab[il_Fila]		


CHOOSE CASE ai_tipo
		
	CASE 1
		ldc_Sobre	=	Dec(as_Valor)
		
	CASE 2
		ldc_Bajo		=	Dec(as_valor)	
		
END CHOOSE

IF IsNull(ldc_Sobre)	THEN ldc_Sobre  = 0
IF IsNull(ldc_bajo)	THEN ldc_Bajo	= 0 

ldc_Suma	=	ldc_Sobre + ldc_Bajo

IF ldc_Suma > 100 THEN	
	MessageBox("Atención","La Sumatoria del Peso de Racimo~r" +&
							"no debe ser mayor a 100")
	RETURN False
END IF					

RETURN True
end function

public function boolean sumaracimo (integer ai_tipo, string as_valor);
Decimal	ldc_deforme, ldc_apretado,ldc_Suma

ldc_deforme	=	Tab_1.TabPage_2.dw_Calidad.Object.ccpd_forade[il_Fila]
ldc_apretado	=	Tab_1.TabPage_2.dw_Calidad.Object.ccpd_foraap[il_Fila]		


CHOOSE CASE ai_tipo
		
	CASE 1
		ldc_deforme	=	Integer(as_valor)
		
	CASE 2
		ldc_apretado	=	Integer(as_valor)	
		
END CHOOSE

IF IsNull(ldc_Deforme)  THEN ldc_Deforme  = 0
IF IsNull(ldc_Apretado) THEN ldc_Apretado = 0 

ldc_Suma	=	ldc_Deforme + ldc_Apretado 

IF ldc_Suma > 100 THEN	
	MessageBox("Atención","La Sumatoria de la Forma de Racimo  ~r" +&
							"no debe ser mayor a 100")
	RETURN False
END IF					

RETURN True
end function

public function boolean cienxciento (string columna, string valor);//Integer	li_valor1, li_valor2, li_valor3, li_valor4, li_valor5, li_valor6, li_suma
//String	ls_Null
//
//SetNull(ls_Null)
//
//CHOOSE CASE Columna
//
//	CASE "ccpd_tamayo"
//		li_valor1	=	Integer(valor)
//		li_Suma		=	li_Valor1
//		IF li_suma > 100 THEN			
//			MessageBox("Error de Consistencia", "El valor no debe exceder el 100%", StopSign!, Ok!)
//			dw_3.SetFocus()
//			RETURN FALSE
//		END IF
//		
//	CASE "ccpd_tamba1"
//		li_Valor1	=	Tab_1.TabPage_2.dw_Calidad.Object.ccpd_tamayo[il_Fila]
//		li_valor2	=	Integer(valor)
//		li_Suma		=	li_Valor1	+	li_Valor2
//		IF li_suma > 100 THEN			
//			MessageBox("Error de Consistencia", "El valor no debe exceder el 100%", StopSign!, Ok!)
//			dw_3.SetFocus()
//			RETURN FALSE
//		END IF
//		
//	CASE "ccpd_tamba2"
//		li_Valor1	=	Tab_1.TabPage_2.dw_Calidad.Object.ccpd_tamayo[il_Fila]		
//		li_Valor2	=	Tab_1.TabPage_2.dw_Calidad.Object.ccpd_tamba1[il_Fila]		
//		li_valor3	=	Integer(valor)
//		li_Suma		=	li_Valor1	+	li_Valor2	+	li_Valor3
//		IF li_suma > 100 THEN			
//			MessageBox("Error de Consistencia", "El valor no debe exceder el 100%", StopSign!, Ok!)
//			dw_3.SetFocus()
//			RETURN FALSE
//		END IF
//
//	CASE "ccpd_tamba3"
//		li_Valor1	=	Tab_1.TabPage_2.dw_Calidad.Object.ccpd_tamayo[il_Fila]		
//		li_Valor2	=	Tab_1.TabPage_2.dw_Calidad.Object.ccpd_tamba1[il_Fila]		
//		li_Valor3	=	Tab_1.TabPage_2.dw_Calidad.Object.ccpd_tamba2[il_Fila]		
//		li_valor4 	=	Integer(valor)
//		li_Suma		=	li_Valor1	+	li_Valor2	+	li_Valor3	+	li_Valor4
//		IF li_suma > 100 THEN			
//			MessageBox("Error de Consistencia", "El valor no debe exceder el 100%", StopSign!, Ok!)
//			dw_3.SetFocus()
//			RETURN FALSE
//		END IF
//
//	CASE "ccpd_tamba4"
//		li_Valor1	=	Tab_1.TabPage_2.dw_Calidad.Object.ccpd_tamayo[il_Fila]
//		li_Valor2	=	Tab_1.TabPage_2.dw_Calidad.Object.ccpd_tamba1[il_Fila]		
//		li_Valor3	=	Tab_1.TabPage_2.dw_Calidad.Object.ccpd_tamba2[il_Fila]		
//		li_Valor4	=	Tab_1.TabPage_2.dw_Calidad.Object.ccpd_tamba3[il_Fila]		
//		li_valor5 	=	Integer(valor)
//		li_Suma		=	li_Valor1 +	li_Valor2 +	li_Valor3 +	li_Valor4 +	li_Valor5
//		IF li_suma > 100 THEN	
//			MessageBox("Error de Consistencia", "La Suma de Tamaño Bayas debe dar siempre 100.", StopSign!, Ok!)
//			dw_3.SetFocus()
//			RETURN FALSE
//		END IF
//	
//	CASE "ccpd_tameno"
//		li_Valor1	=	Tab_1.TabPage_2.dw_Calidad.Object.ccpd_tamayo[il_Fila]
//		li_Valor2	=	Tab_1.TabPage_2.dw_Calidad.Object.ccpd_tamba1[il_Fila]		
//		li_Valor3	=	Tab_1.TabPage_2.dw_Calidad.Object.ccpd_tamba2[il_Fila]		
//		li_Valor4	=	Tab_1.TabPage_2.dw_Calidad.Object.ccpd_tamba3[il_Fila]
//		li_valor5	=	Tab_1.TabPage_2.dw_Calidad.Object.ccpd_tamba4[il_Fila]
//		li_valor6 	=	Integer(valor)
//		li_Suma		=	li_Valor1 +	li_Valor2 +	li_Valor3 +	li_Valor4 +	li_Valor5 + li_valor6
//		IF li_suma > 100 OR li_Suma <	100	THEN			
//			MessageBox("Error de Consistencia", "La Suma de Tamaño Bayas debe dar siempre 100.", StopSign!, Ok!)
//			dw_3.SetFocus()
//			RETURN FALSE
//		END IF
//
//END CHOOSE
//
RETURN TRUE

end function

public subroutine datosembalaje (string columna, string valor);Integer	li_v5, li_v7, li_v8, li_v9
String	ls_v1, ls_v2, ls_v3, ls_v4, ls_v6
Dec{2}	ld_v9, ld_v5

ls_v1				=	dw_2.Object.ccpd_paleti[il_fila]
ls_v2				=	dw_2.Object.ccpd_rotula[il_fila]
ls_v3				=	dw_2.Object.ccpd_materi[il_fila]
ls_v4				=	dw_2.Object.ccpd_empaq[il_fila]
li_v5				=	dw_2.Object.ccpd_pesone[il_fila]
ls_v6				=	dw_2.Object.ccpd_aparie[il_fila]
li_v7				=	dw_2.Object.ccpd_nropaq[il_fila]
li_v8				=	dw_2.Object.ccpd_noraci[il_fila]
li_v9				=	dw_2.Object.ccpd_grdobr[il_fila]

CHOOSE CASE Columna

	CASE "ccpd_paleti"
		ls_v1	=	valor
		IF ls_v1 = '' THEN
			MessageBox("Error de Consistencia", "Debe Ingresar Paletizaje.", StopSign!, Ok!)					
			dw_2.SetFocus()
		END IF

	CASE "ccpd_rotula"
		ls_v2	=	valor
		IF ls_v2 = '' THEN
			MessageBox("Error de Consistencia", "Debe Ingresar Rotulación.", StopSign!, Ok!)
			dw_2.SetFocus()
		END IF

	CASE "ccpd_materi"
		ls_v3	=	valor
		IF ls_v3 = '' THEN
			MessageBox("Error de Consistencia", "Debe Ingresar Materiales.", StopSign!, Ok!)
			dw_2.SetFocus()
		END IF

	CASE "ccpd_empaq"
		ls_v4 =	valor
		IF ls_v4 = '' THEN
			MessageBox("Error de Consistencia", "Debe Ingresar Empaque.", StopSign!, Ok!)
			dw_2.SetFocus()
		END IF
		
	CASE "ccpd_pesone"
		ld_v5 =	Dec(valor)
		IF ld_v5 = 0 THEN
			MessageBox("Error de Consistencia", "Debe Ingresar Peso Neto.", StopSign!, Ok!)
			dw_2.SetFocus()
		END IF
		
	CASE "ccpd_aparie"
		ls_v6 =	valor
		IF ls_v6 = '' THEN
			MessageBox("Error de Consistencia", "Debe Ingresar Apariencia.", StopSign!, Ok!)
			dw_2.SetFocus()
		END IF
		
	CASE "ccpd_nropaq"
		li_v7 =	Integer(valor)
		IF li_v7 = 0 THEN
			MessageBox("Error de Consistencia", "Debe Ingresar Número Paquetes.", StopSign!, Ok!)
			dw_2.SetFocus()
		END IF

	CASE "ccpd_noraci"
		li_v8 =	Integer(valor)
		IF li_v8 = 0 or li_v8 > 99 THEN
			MessageBox("Error de Consistencia", "Debe Ingresar Número Racimos adecuado.", StopSign!, Ok!)
			dw_2.Object.ccpd_noraci[il_fila]=0
			ii_valor	=	2
			dw_2.SetFocus()
		END IF

	CASE "ccpd_grdobr"
		ld_v9 =	Dec(valor)
		IF ld_v9 = 0 THEN
			MessageBox("Error de Consistencia", "Debe Ingresar Grados Brix.", StopSign!, Ok!)
			dw_2.SetFocus()
		END IF
		
END CHOOSE

end subroutine

public function boolean datosresolucion (string columna, string valor);String	ls_v1, ls_v2, ls_v3, ls_v4

ls_v1		=	Trim(dw_2.Object.ccpd_resemb[il_fila])
ls_v2		=	Trim(dw_3.Object.ccpd_rescal[il_fila])
ls_v3		=	Trim(dw_4.Object.ccpd_rescon[il_fila])

CHOOSE CASE Columna

	CASE "ccpd_resemb"
		ls_v1	=	Trim(valor)
		IF ls_v1 = '' OR (ls_v1 <> 'B' AND ls_v1 <> 'R' AND ls_v1 <> 'M') THEN
			MessageBox("Error de Consistencia", "Debe Ingresar Resolución Embalaje B, R, o M.", StopSign!, Ok!)
			dw_5.SetFocus()
			RETURN False
		END IF

	CASE "ccpd_rescal"
		ls_v2	=	Trim(valor)
		IF ls_v2 = '' OR (ls_v2 <>'B'  AND  ls_v2 <> 'R'  AND  ls_v2 <> 'M') THEN
			MessageBox("Error de Consistencia", "Debe Ingresar Resolución Calidad B, R, o M.", StopSign!, Ok!)
			dw_5.SetFocus()
			RETURN False
		END IF

	CASE "ccpd_rescon"
		ls_v3	=	Trim(valor)
		IF ls_v3 = '' OR (ls_v3 <> 'B'  AND  ls_v3 <> 'R'  AND  ls_v3 <> 'M')THEN
			MessageBox("Error de Consistencia", "Debe Ingresar Resolución Condición B, R, o M.", StopSign!, Ok!)
			dw_5.SetFocus()
			RETURN False			
		END IF		
END CHOOSE

RETURN True
end function

public function string datosresolucioncaja (string columna, string valor);String	ls_v1, ls_v2, ls_v3, ls_v4, ls_juntos

ls_v1		=	dw_5.Object.ccpd_resemb[il_fila]
ls_v2		=	dw_5.Object.ccpd_rescal[il_fila]
ls_v3		=	dw_5.Object.ccpd_rescon[il_fila]

CHOOSE CASE Columna

	CASE "ccpd_resemb"
		ls_v1	=	valor

	CASE "ccpd_rescal"
		ls_v2	=	valor

	CASE "ccpd_rescon"
		ls_v3	=	valor
		
END CHOOSE

ls_juntos	=	ls_v1+ls_v2+ls_v3

IF ls_juntos = 'BBB' THEN 
	ls_v4 = 'A'
ELSEIF ls_juntos ='BBR' OR ls_juntos ='RBB' OR ls_juntos ='BRB' THEN
	ls_v4 = 'A'
ELSEIF ls_v1 = 'M' OR ls_v2 = 'M' OR ls_v3 = 'M' THEN
	ls_v4 = 'O'
ELSE 
	ls_v4 = 'Z'
END IF

RETURN ls_v4
end function

public function boolean duplicado (string campo, integer tipo);Long        ll_fila
String      ls_numero

ls_numero	=	String(dw_1.GetItemNumber(il_fila,"rpcf_numero"))

CHOOSE CASE tipo
	case 1
		ls_numero	=	campo

END CHOOSE

ll_fila = dw_1.Find("rpcf_numero = " + ls_numero, 1,dw_1.RowCount())

IF ll_fila > 0 and ll_fila <> il_fila THEN
	MessageBox("Error","Número de Párrafo ya fue ingresado anteriormente",Information!, OK!)
   RETURN True
ELSE
	RETURN False
END IF

end function

public subroutine pesoproba (string columna, string valor);Dec{1}	ld_promba

//ld_promba	=	dw_3.Object.ccpd_promba[il_fila]

CHOOSE CASE Columna
		
	CASE "ccpd_promba"
		ld_promba 		=	Dec(valor)
		IF ld_promba	= 0 or ld_promba >20 THEN
			MessageBox("Error de Consistencia", "Debe Ingresar Peso Promedio correspondiente .", StopSign!, Ok!)
			ii_valor=6
			dw_3.SetFocus()
		END IF	

END CHOOSE

end subroutine

public subroutine promediobayas (string columna, string valor);Dec{2}	li_valor1
String	ls_Null

SetNull(ls_Null)

li_valor1		=	dw_3.Object.ccpd_promba[il_fila]

CHOOSE CASE Columna

	CASE "ccpd_promba"
		li_valor1	=	Dec(valor)

END CHOOSE


IF li_valor1 = 0 THEN	
	MessageBox("Error de Consistencia", "Debe Ingresar Peso Promedio de Bayas.", StopSign!, Ok!)
	dw_3.SetFocus()
END IF


end subroutine

public function boolean resulcaj (integer ai_resul);Integer	li_Resul, li_Existe 

li_Resul	=	ai_resul

SELECT Count(*)
INTO	 :li_Existe 
FROM	 dbo.ctlcaldanoespecie
WHERE	 espe_codigo = 11
AND    ccda_secuen = :li_Resul;

IF sqlca.sqlcode	=	-1	THEN
	F_ErrorBaseDatos(sqlca,"No se pudo leer la Tabla de Daños por Especie")
ELSEIF li_Existe	>	0	THEN
	RETURN TRUE
ELSE
	MessageBox("Atención","Causal digitado, no se encuentra Ingresado en tabla respectiva",StopSign!, Ok!)
END IF

RETURN FALSE


end function

public function boolean sumadeshi (integer ai_tipo, string as_valor);Integer 	li_ValLev, li_ValMod, li_ValAlt, li_Suma

li_ValLev	=	Tab_1.TabPage_3.dw_Condicion.Object.ccpd_deslev[il_Fila]
li_ValMod	=	Tab_1.TabPage_3.dw_Condicion.Object.ccpd_desmod[il_Fila]		
li_ValAlt	=	Tab_1.TabPage_3.dw_Condicion.Object.ccpd_desalt[il_Fila]

CHOOSE CASE ai_tipo
		
	CASE 1
		li_ValLev	=	Integer(as_valor)
		
	CASE 2
		li_ValMod	=	Integer(as_valor)
		
	CASE 3
		li_ValAlt	=	Integer(as_valor)
		
END CHOOSE

IF IsNull(li_ValLev) THEN li_ValLev = 0
IF IsNull(li_ValMod) THEN li_ValMod = 0 
IF	IsNull(li_ValAlt) THEN li_ValAlt = 0

li_Suma	=	li_ValLev + li_ValMod + li_ValAlt		

IF li_Suma > 100 THEN	
	MessageBox("Atención","La Sumatoria de la Deshidratación ~r" +&
							"no debe ser mayor a 100",StopSign!)
	RETURN False
END IF						

RETURN True
end function

public function boolean existedañoinsecto (string data);//Integer	li_Daño	, li_Existe
//
//li_Daño	=	Integer(Data)
//
//SELECT Count(*)
//INTO	 :li_Existe
//FROM	 dba.ctlcaldanoespecie
//WHERE	 espe_codigo = 11
//AND    ccfa_codigo = 30
//AND    ccsf_codigo = 18
//AND    ccda_secuen = :li_Daño	;
//
//IF sqlca.sqlcode	=	-1	THEN
//	F_ErrorBaseDatos(sqlca,"No se pudo leer la Tabla de Daños por Especie")
//ELSEIF	li_Existe	>	0	THEN
//	RETURN TRUE
//ELSE
//	MessageBox("atención","Daño digitado no corresponde a Insectos")
//END IF
//
RETURN FALSE
end function

public function boolean existenvaloresracimo (integer ai_punura, integer ai_punuba, integer ai_pununi);String	li_Nula

SetNull(li_Nula)

Tab_1.TabPage_3.dw_condicion.accepttext()

IF ai_punuba <> 0 OR ai_pununi <> 0 AND ai_punura	=	0 THEN	
	MessageBox("Atención","Debe Ingresar el Número de Racimos", StopSign!)	
	RETURN False
END IF 	

Return True




end function

public function boolean cienporciento (string columna, string valor);Integer	li_valor1, li_valor2, li_valor3, li_valor4, li_suma
String		ls_Null

SetNull(ls_Null)

CHOOSE CASE Columna
	CASE "ccpd_cobay1"
		li_valor1	=	Integer(valor)
		li_Suma		=	li_Valor1	
		IF li_suma > 100	THEN
				MessageBox("Error de Consistencia", "Valor no debe exceder el 100%", StopSign!, Ok!)
				dw_3.SetFocus()
				RETURN FALSE
		END IF		

	CASE "ccpd_cobay2"
		li_Valor1	=	Tab_1.TabPage_2.dw_Calidad.Object.ccpd_cobay1[il_Fila]
		li_valor2	=	Integer(valor)
		
		li_Suma		=	li_Valor1	+	li_Valor2
		IF  li_suma > 100	THEN
				MessageBox("Error de Consistencia", "Valor no debe exceder el 100%", StopSign!, Ok!)
				dw_3.SetFocus()
				RETURN FALSE
		END IF		
	
	CASE "ccpd_cobay3"
		li_Valor1	=	Tab_1.TabPage_2.dw_Calidad.Object.ccpd_cobay1[il_Fila]
		li_Valor2	=	Tab_1.TabPage_2.dw_Calidad.Object.ccpd_cobay2[il_Fila]
		li_valor3	=	Integer(valor)
		li_Suma		=	li_Valor1	+	li_Valor2	+	li_Valor3
		IF  li_suma > 100	THEN
				MessageBox("Error de Consistencia", "Valor no debe exceder el 100%", StopSign!, Ok!)
				dw_3.SetFocus()
				RETURN FALSE
		END IF		
		
	CASE "ccpd_cobay4"
		li_Valor1	=	Tab_1.TabPage_2.dw_Calidad.Object.ccpd_cobay1[il_Fila]
		li_Valor2	=	Tab_1.TabPage_2.dw_Calidad.Object.ccpd_cobay2[il_Fila]		
		li_Valor3	=	Tab_1.TabPage_2.dw_Calidad.Object.ccpd_cobay3[il_Fila]		
		li_valor4 	=	Integer(valor)
		li_Suma		=	li_Valor1	+	li_Valor2	+	li_Valor3	+	li_Valor4
		IF  li_suma > 100 THEN
				MessageBox("Error de Consistencia", "Valor no debe exceder el 100%", StopSign!, Ok!)
				dw_3.SetFocus()
				RETURN FALSE
		END IF
END CHOOSE

RETURN TRUE
end function

public function integer validapeso (string columna, string valor);Dec{2}	ld_valor1, ld_PesoNeto, ld_PesoMin,ld_PesoMax
String	ls_Null, ls_Embalaje

ls_Embalaje	=	istr_mant.argumento[17]

SetNull(ls_Null)

ld_valor1	=	Dec(valor)
		
SELECT	en.enva_pesone
	INTO 	:ld_PesoNeto
 	FROM 	dbo.embalajesprod as em,dbo.envases as en
	WHERE em.emba_codigo = :ls_Embalaje
	AND	clie_codigo = :gi_CodExport 
	AND	em.enva_tipoen = en.enva_tipoen
	AND	em.enva_codigo = en.enva_codigo;
	
IF sqlca.sqlcode	=	-1	THEN
	F_ErrorBaseDatos(sqlca,"No se pudo leer la Tabla de Embalajes")
ELSEIF	ld_PesoNeto = 0	THEN	
	MessageBox("Atención","Código Embalaje No tiene Ingresado Peso Neto Existe")
ELSE
	ld_PesoMax	=	ld_PesoNeto + ld_PesoNeto *.5
	ld_PesoMin	=	ld_PesoNeto - ld_PesoNeto *.5
	 IF ld_valor1 > ld_PesoMax OR ld_valor1 < ld_PesoMin THEN
		MessageBox("Error de Consistencia", " Peso fuera de rango.", StopSign!, Ok!)
		dw_3.SetFocus()
	 	 ld_valor1=0
	 	 ii_valor =0
 	 END IF
END IF	

RETURN ld_valor1
end function

public function boolean existe_causal (integer ai_causal);Integer	li_Causal, li_Existe 

li_Causal	=	ai_causal

SELECT Count(*)
INTO	 :li_Existe 
FROM	 dbo.ctlcaldanoespecie
WHERE	 espe_codigo	=	11
AND    ccda_secuen	=	:li_Causal;

IF sqlca.sqlcode	=	-1	THEN
	F_ErrorBaseDatos(sqlca,"No se pudo leer la Tabla de Daños por Especie")
ELSEIF li_Existe	>	0	THEN
	RETURN TRUE
ELSE
	MessageBox("Atención","Causal digitado, no se encuentra Ingresado en tabla respectiva",StopSign!, Ok!)
END IF
RETURN FALSE


end function

public function boolean existe_matextraña (string data);Integer li_matext, li_existe

li_matext	=	integer(data)

SELECT Count(*)
INTO	 :li_Existe
FROM	 dbo.ctlcaldanoespecie
WHERE	 espe_codigo = 11
AND    ccfa_codigo = 30
AND    ccda_secuen = :li_matext	;


IF sqlca.sqlcode	=	-1	THEN
	F_ErrorBaseDatos(sqlca,"No se pudo leer la Tabla de Daños por Especie")
ELSEIF li_Existe	>	0	THEN
	RETURN TRUE
ELSE
	MessageBox("Atención","Materia Extraña digitada, no se encuentra Ingresado en tabla respectiva",StopSign!, Ok!)
END IF
RETURN FALSE





end function

public function boolean valida_causal (string as_columna, integer ai_valor);Integer li_causa1, li_causa2, li_causa3
String ls_nombre

CHOOSE CASE as_columna
	CASE 'ccpd_cobjem'
		li_causa1 = 100
		li_causa2 = 199
		ls_nombre = 'Embalaje'
	CASE 'ccpd_cobjca'
		li_causa1 = 300
		li_causa2 = 399
		ls_nombre = 'Calidad'
	CASE 'ccpd_cobjco'
		li_causa1 = 400
		li_causa2 = 499
		ls_nombre = 'Condición'
END CHOOSE
li_causa3 = 550

IF ai_valor < li_causa1 OR ai_valor > li_causa2 AND ai_valor <> li_causa3 THEN 
	MessageBox("ERROR","Causal de Objeción no corresponde a~ " + ls_nombre,StopSign!)
	RETURN FALSE
ELSE
	RETURN TRUE
END IF

end function

public subroutine wf_nuevo ();il_fila = dw_1.InsertRow(0)

dw_1.SetRedraw(False)
dw_1.SetRow(il_fila)
dw_1.ScrollToRow(il_fila)
istr_mant.dw.SetRow(il_fila)
istr_mant.dw.ScrolltoRow(il_fila)
istr_mant.dw.SelectRow(0,False)
istr_mant.dw.SelectRow(il_fila,True)
dw_1.SetRedraw(True)
end subroutine

public function boolean causales (integer ai_causa);Integer	li_Causal, li_Existe 
Boolean	lb_Retorno	
li_Causal	=	ai_causa

SELECT Count(*)
INTO	 :li_Existe 
FROM	 dbo.ctlcaldanoespecie
WHERE	 espe_codigo	=	11
AND    ccda_secuen	=	:li_Causal;

IF sqlca.sqlcode	=	-1	THEN
	F_ErrorBaseDatos(sqlca,"No se pudo leer la Tabla de Daños por Especie")
ELSEIF li_Existe	>	0	THEN
	lb_Retorno	=	True
ELSE
	MessageBox("Atención","Causal digitada, no se ha creado ",StopSign!, Ok!)
END IF
RETURN lb_Retorno


end function

public function boolean causales2 (integer ai_causa);Integer	li_Causal, li_Existe 
Boolean	lb_Retorno	
String ls_nombre
li_Causal	=	ai_causa

SELECT Count(*)
INTO	 :li_Existe 
FROM	 dbo.ctlcaldanoespecie
WHERE	 espe_codigo	=	11
AND    ccda_secuen	=	:li_Causal;

IF sqlca.sqlcode	=	-1	THEN
	F_ErrorBaseDatos(sqlca,"No se pudo leer la Tabla de Daños por Especie")
	Return True
ELSEIF li_Existe	>	0	THEN
	IF li_Causal >= 100  AND li_Causal <= 199 THEN
		IF dw_2.Object.ccpd_resemb[il_fila] = 'B' THEN
			MessageBox("Atención","No es posible ingresar causal por estar con calificación embalaje buena.",StopSign!, Ok!)
			Return True
		END IF	
		Return False
	ELSEIF li_Causal >= 300  AND li_Causal <= 399 THEN
		IF dw_2.Object.ccpd_rescal[il_fila] = 'B' THEN
			MessageBox("Atención","No es posible ingresar causal por estar con calificación calidad buena.",StopSign!, Ok!)
			Return True
		END IF	
		Return False
	ELSEIF li_Causal >= 400  AND li_Causal <= 499 THEN	
		IF dw_2.Object.ccpd_rescon[il_fila] = 'B' THEN
			MessageBox("Atención","No es posible ingresar causal por estar con calificación condición buena.",StopSign!, Ok!)
			Return True
		END IF	
		Return False
	END IF
ELSE
	MessageBox("Atención","Causal digitada, no se ha creado ",StopSign!, Ok!)
	Return True
END IF
Return False


end function

protected function long existepallet (integer ai_cliente, integer ai_planta, long al_lote, long al_pallet);Integer	li_existepallet
Long		ll_ccajas
string  ls_planilla

SetNull(ll_ccajas)

/*LRBB 13.Mar.2014 VerIFica que el pallet ingresado no este en otra planilla*/

SELECT list(distinct ccpe_numero)
   INTO :ls_planilla
  FROM dbo.ctlcalplacuaninspuvadet
WHERE ccpd_npalle = :al_pallet
 	And cclo_numero = :al_Lote;

IF sqlca.sqlcode	=	-1	THEN
		F_ErrorBaseDatos(sqlca,"No se pudo leer la Tabla dbo.ctlcalplacuaninspuvadet")
ELSEIF ls_planilla <> "" then
	MessageBox("Atención","Número de pallet ya existe en planilla(s) "+string(ls_planilla))
ELSE
	SELECT count(*)
		INTO :li_existepallet
	  FROM dbo.palletfrutahisto
	 WHERE clie_codigo		=	:ai_cliente
		AND plde_codigo		=	:ai_planta
		AND paen_numero	=	:al_pallet
		AND pafr_tipdoc		=	1;
		
	IF sqlca.sqlcode	=	-1	THEN
		F_ErrorBaseDatos(sqlca,"No se pudo leer la Tabla de Historica de Pallet")
	ELSEIF	li_existepallet = 0	THEN	
		MessageBox("Atención","Número de pallet no existe")
	ELSE
		SELECT sum(isnull(pafh_ccajas,0))
			INTO :ll_ccajas
		  FROM dbo.palletfrutahisto
		 WHERE clie_codigo		=	:ai_cliente
			AND plde_codigo	=	:ai_planta
			AND paen_numero	=	:al_pallet
			AND pafr_tipdoc		=	1
			AND pafh_nrlote	=	:al_lote;
			
		IF sqlca.sqlcode	=	-1	THEN
			F_ErrorBaseDatos(sqlca,"No se pudo leer la Tabla de Historica de Pallet")
		ELSEIF	isnull(ll_ccajas) OR ll_ccajas = 0	THEN	
			MessageBox("Atención","Número de pallet no tiene relación con número de lote")
		END IF
	END IF	
END IF
RETURN ll_ccajas
end function

protected subroutine wf_prediopallet (integer ai_cliente, integer ai_planta, long al_lote, long al_pallet);Long	ll_Existe, ll_Productor, ll_Predio, ll_Cuartel

SELECT Count(*)
	INTO :ll_Existe
  FROM dbo.palletfrutahisto
 WHERE clie_codigo		=	:ai_cliente
	AND plde_codigo		=	:ai_planta
	AND paen_numero		=	:al_pallet
	AND pafr_tipdoc		=	1;
	
If Sqlca.sqlcode	=	-1	Then
		F_ErrorBaseDatos(sqlca,"No se pudo leer la Tabla de Historica de Pallet")
ElseIf	ll_Existe = 0	Then	
	MessageBox("Atención","Número de pallet no existe")
Else
	SELECT DISTINCT prod_codigo, pafr_huert1, pafr_cuart1
		INTO :ll_Productor, :ll_Predio, :ll_Cuartel
	  FROM dbo.palletfrutahisto
	 WHERE clie_codigo	=	:ai_cliente
		AND plde_codigo	=	:ai_planta
		AND paen_numero	=	:al_pallet
		AND pafr_tipdoc	=	1
		AND pafh_nrlote	=	:al_lote
		USING SqlCa;

	If Sqlca.SqlCode	=	-1	Then
		If Sqlca.SqlErrText = "Select returned more than one row" Then
			MessageBox('Atención',  'Pallet posee mas de un predio debe seleccionar manual.', Information!, OK!)
			
			dw_6.GetChild("prpr_codigo", idwc_Predio)
			idwc_Predio.SetTransObject(sqlca)
			idwc_Predio.Retrieve(Long(istr_Mant.Argumento[8]))
				
			dw_6.GetChild("prcc_codigo", idwc_Cuartel)
			idwc_Cuartel.SetTransObject(sqlca)
			idwc_Cuartel.Retrieve(Long(istr_Mant.Argumento[8]), -1)
			
			dw_6.Object.prod_codigo[il_fila] = Long(istr_Mant.Argumento[8])
			
			Return
		Else
			F_ErrorBaseDatos(sqlca,"No se pudo leer la Tabla de Historica de Pallet")
		End If
	Else
		dw_6.Object.prod_codigo[il_fila] = ll_Productor
		dw_6.Object.prpr_codigo[il_fila] = ll_Predio
		dw_6.Object.prcc_codigo[il_fila] = ll_Cuartel
		
		dw_6.GetChild("prpr_codigo", idwc_Predio)
		idwc_Predio.SetTransObject(sqlca)
		idwc_Predio.Retrieve(ll_Productor)
			
		dw_6.GetChild("prcc_codigo", idwc_Cuartel)
		idwc_Cuartel.SetTransObject(sqlca)
		idwc_Cuartel.Retrieve(ll_Productor, ll_Predio)
	End If	
End If
end subroutine

on w_mant_deta_ctlcalplanillauvas.create
int iCurrent
call super::create
this.tab_1=create tab_1
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.tab_1
end on

on w_mant_deta_ctlcalplanillauvas.destroy
call super::destroy
destroy(this.tab_1)
end on

event ue_recuperadatos;w_main.SetMicroHelp("Recuperando Datos...")

SetPointer(HourGlass!)
PostEvent("ue_listo")

IF istr_mant.Agrega THEN
	pb_Cancela.Enabled	=	False
	pb_Primero.Enabled	=	False
	pb_Anterior.Enabled	=	False
	pb_Siguiente.Enabled	=	False
	pb_Ultimo.Enabled		=	False
	
	wf_nuevo()
	This.Title			= "INGRESO DE REGISTRO"
ELSE
	IF dw_1.RowCount() > 1 THEN
		pb_Primero.Enabled	=	True
		pb_Anterior.Enabled	=	True
		pb_Siguiente.Enabled	=	True
		pb_Ultimo.Enabled		=	True
	END IF
	
	il_fila	=	istr_mant.dw.GetRow()
	
	dw_1.SetRedraw(False)
	dw_1.SetRow(il_fila)
	dw_1.ScrollToRow(il_fila)
	dw_1.SetRedraw(True)

	IF istr_mant.Borra THEN
		dw_1.Enabled		=	False
		pb_Salir.Enabled	=	False
		This.Title			=	"ELIMINACION DE REGISTRO"
		pb_Acepta.PictureName	=	"\Desarrollo 12\Imagenes\Botones\EliminarEnab.png"
		pb_Acepta.DisabledName=	"\Desarrollo 12\Imagenes\Botones\EliminarDisab.png"
	ELSEIF istr_mant.Solo_Consulta THEN
		dw_1.Enabled			=	False
		pb_Acepta.Enabled		=	False
		pb_Cancela.Enabled	=	False
		This.Title				=	"CONSULTA DE REGISTRO"
	ELSE
		pb_Salir.Enabled	=	False
		This.Title			=	"MANTENCION DE REGISTRO"
	END IF
END IF

String	ls_Usuario
Integer	li_Grupo
ls_Usuario	=	Upper(Gstr_Us.Nombre)

ias_campo[54]	=	dw_1.Object.ccpd_rescaj[il_fila]	
IF ias_campo[54]	<> '' OR Not IsNull(ias_campo[54]) THEN
	istr_mant.argumento[5]	=	istr_mant.argumento[22]

//IF istr_mant.Argumento[22] ='0' OR dw_1.GetItemStatus(il_Fila, 0, Primary!) = NewModified!  THEN 		
IF not istr_mant.Agrega and not  istr_mant.Borra THEN 		
		ias_campo[1]	=	String(dw_1.Object.ccpe_numero[il_fila])
		ii_ccpe=Integer(ias_campo[1])
		ias_campo[2]	=	String(dw_1.Object.plde_codigo[il_fila])
		ii_plde=Integer(ias_campo[2])
		ias_campo[3]	=	String(dw_1.Object.clie_codigo[il_fila])
		ii_cliente=Integer(ias_campo[3])
		ias_campo[4]	=	String(dw_1.Object.cclo_numero[il_fila])
		ii_cclo=Integer(ias_campo[4])
		ias_campo[5]	=	String(dw_1.Object.ccpd_secuen[il_fila])
		ias_campo[6]	=	String(dw_1.Object.ccpe_noguia[il_fila])
		ias_campo[7]	=	dw_1.Object.ccpd_paleti[il_fila]
		ias_campo[8]	=	dw_1.Object.ccpd_rotula[il_fila]
		ias_campo[9]	=	dw_1.Object.ccpd_materi[il_fila]
		ias_campo[10]	=	dw_1.Object.ccpd_empaq[il_fila]
		ias_campo[11]	=	String(dw_1.Object.ccpd_pesone[il_fila])
		ias_campo[12]	=	dw_1.Object.ccpd_aparie[il_fila]
		ias_campo[13]	=	String(dw_1.Object.ccpd_nropaq[il_fila])
		ias_campo[14]	=	String(dw_1.Object.ccpd_noraci[il_fila])
		ias_campo[15]	=	String(dw_1.Object.ccpd_grdobr[il_fila])
		ias_campo[16]	=	String(dw_1.Object.ccpd_temper[il_fila])
		ias_campo[17]	=	String(dw_1.Object.ccpd_cobay1[il_fila])
		ias_campo[18]	=	String(dw_1.Object.ccpd_cobay2[il_fila])
		ias_campo[19]	=	String(dw_1.Object.ccpd_cobay3[il_fila])
		ias_campo[20]	=	String(dw_1.Object.ccpd_cobay4[il_fila])
		ias_campo[21]	=	String(dw_1.Object.ccpd_tamayo[il_fila])
		ias_campo[22]	=	String(dw_1.Object.ccpd_tamba1[il_fila])
		ias_campo[23]	=	String(dw_1.Object.ccpd_tamba2[il_fila])
		ias_campo[24]	=	String(dw_1.Object.ccpd_tamba3[il_fila])
		ias_campo[25]	=	String(dw_1.Object.ccpd_tamba4[il_fila])
		ias_campo[26]	=	String(dw_1.Object.ccpd_promba[il_fila])
		ias_campo[27]	=	String(dw_1.Object.ccpd_forade[il_fila])
		ias_campo[28]	=	String(dw_1.Object.ccpd_foraap[il_fila])
		ias_campo[29]	=	String(dw_1.Object.ccpd_pesras[il_fila])
		ias_campo[30]	=	String(dw_1.Object.ccpd_pesrab[il_fila])
		ias_campo[31]	=	String(dw_1.Object.ccpd_residu[il_fila])
		ias_campo[32]	=	String(dw_1.Object.ccpd_mancha[il_fila])
		ias_campo[33]	=	String(dw_1.Object.ccpd_insect[il_fila])
		ias_campo[34]	=	String(dw_1.Object.ccpd_deslev[il_fila])
		ias_campo[35]	=	String(dw_1.Object.ccpd_desmod[il_fila])
		ias_campo[36]	=	String(dw_1.Object.ccpd_desalt[il_fila])
		ias_campo[37]	=	String(dw_1.Object.ccpd_punuba[il_fila])
		ias_campo[38]	=	String(dw_1.Object.ccpd_pununi[il_fila])
		ias_campo[39]	=	String(dw_1.Object.ccpd_punura[il_fila])
		ias_campo[40]	=	String(dw_1.Object.ccpd_pacida[il_fila])
		ias_campo[41]	=	String(dw_1.Object.ccpd_opnbay[il_fila])
		ias_campo[42]	=	String(dw_1.Object.ccpd_coblan[il_fila])
		ias_campo[43]	=	String(dw_1.Object.ccpd_cocris[il_fila])
		ias_campo[44]	=	String(dw_1.Object.ccpd_codesp[il_fila])
		ias_campo[45]	=	String(dw_1.Object.ccpd_conso2[il_fila])
		ias_campo[46]	=	String(dw_1.Object.ccpd_cobapa[il_fila])
		ias_campo[47]	=	String(dw_1.Object.ccpd_codesg[il_fila])
		ias_campo[48]	=	String(dw_1.Object.ccpd_baacuo[il_fila])
		ias_campo[49]	=	String(dw_1.Object.ccpd_coprem[il_fila])
		ias_campo[50]	=	String(dw_1.Object.ccpd_otcond[il_fila])
		ias_campo[51]	=	dw_1.Object.ccpd_resemb[il_fila]
		ias_campo[52]	=	dw_1.Object.ccpd_rescal[il_fila]
		ias_campo[53]	=	dw_1.Object.ccpd_rescon[il_fila]
		ias_campo[54]	=	dw_1.Object.ccpd_rescaj[il_fila]
		ias_campo[55]	=	String(dw_1.Object.espe_codigo[il_fila])
		ias_campo[56]	=	String(dw_1.Object.ccda_secuen[il_fila])
		ias_campo[57]	=	String(dw_1.Object.ccpd_selecc[il_fila])
		ias_campo[58]	=	String(dw_1.Object.ccpd_embala[il_fila])
		ias_campo[59]  =  String(dw_1.Object.ccpd_caotro[il_fila])
		ias_campo[60]  =  String(dw_1.Object.ccpd_coidio[il_fila])
		ias_campo[61]  =  String(dw_1.Object.ccpd_cootro[il_fila])
		ias_campo[62]	=	dw_1.Object.ccpd_rescom[il_fila]
		ias_campo[63]	=	dw_1.Object.ccpd_segpal[il_fila]
		ias_campo[64]	=	dw_1.Object.ccpd_respal[il_fila]
		ias_campo[65]  =  String(dw_1.Object.ccpd_causal1[il_fila])
		ias_campo[66]  =  String(dw_1.Object.ccpd_causal2[il_fila])
		ias_campo[67]  =  String(dw_1.Object.ccpd_causal3[il_fila])
		
		if dw_5.Object.ccpd_rescaj[1] = "A" and dw_5.Object.ccpd_rescom[1] =  "A" then
			dw_5.Object.ccpd_rescom.protect = 1
			dw_5.Object.ccpd_respal.protect = 1
		end if
		
		if dw_5.Object.ccpd_rescaj[1] = "O" and dw_5.Object.ccpd_rescom[1] =  "O" then
			dw_5.Object.ccpd_respal.protect = 0
		end if
		
		if dw_5.Object.ccpd_rescaj[1] = "O" and dw_5.Object.ccpd_rescom[1] =  "A" then
			dw_5.Object.ccpd_respal.protect = 1
		end if

		ias_campo[70]  =  String(dw_1.Object.ccpd_npalle[il_fila])
	
//		IF ias_campo[54] = 'A' THEN
//			dw_5.Object.ccpd_rescaj.protect = 0
//			dw_5.Object.ccda_secuen.protect = 1             				  
//			dw_5.Object.ccda_secuen.BackGround.Color	=	RGB(192,192,192)
//		ELSE
//			dw_5.Object.ccpd_rescaj.protect = 1
//			dw_5.Object.ccda_secuen.protect = 0             				  
//			dw_5.Object.ccda_secuen.BackGround.Color	=	RGB(255,255,255)
//		END IF
		
		Pb_Acepta.Enabled	=	True 
		Pb_Cancela.Enabled	=	True 		
		Pb_Salir.Enabled		=	True 
	ELSE
		istr_mant.Solo_consulta							=	True 
		Tab_1.TabPage_1.dw_Embalaje.Enabled	=	False	
		Tab_1.TabPage_2.dw_Calidad.Enabled		=	False
		Tab_1.TabPage_3.dw_Condicion.Enabled	=	False			
		Tab_1.TabPage_4.dw_resolucion.Enabled	=	False		
		Tab_1.TabPage_5.dw_id.Enabled				=	False	
		Pb_Acepta.Enabled								=	False
		Pb_Cancela.Enabled								=	False
		pb_Salir.Enabled									=	True
	END IF 
END IF

IF istr_mant.Agrega THEN
	dw_1.SetItem(il_Fila,"clie_codigo",Integer(istr_mant.Argumento[7]))
	dw_1.SetItem(il_Fila,"plde_codigo",Integer(istr_mant.Argumento[4]))
	dw_1.SetItem(il_Fila,"ccpe_numero",Long(istr_mant.Argumento[2]))
	dw_1.SetItem(il_Fila,"espe_codigo",Integer(istr_mant.Argumento[9]))
	dw_1.SetItem(il_Fila,"ccpe_noguia",Long(istr_mant.Argumento[13]))
END IF

IF istr_mant.Borra THEN
	Pb_Acepta.Enabled	=	True
	Pb_Cancela.Enabled	=	True
	pb_Salir.Enabled		=	True
END IF	

dw_2.SetRow(il_fila)
dw_2.ScrolltoRow(il_fila)
dw_3.SetRow(il_fila)
dw_3.ScrolltoRow(il_fila)
dw_4.SetRow(il_fila)
dw_4.ScrolltoRow(il_fila)
dw_5.SetRow(il_fila)
dw_5.ScrolltoRow(il_fila)	
dw_6.SetRow(il_fila)
dw_6.ScrolltoRow(il_fila)

dw_6.Object.prod_codigo[il_fila] = Long(istr_Mant.Argumento[8])

IF dw_1.Rowcount() > 0 THEN	
	li_Grupo = BuscaGrupo(ls_Usuario)
		
	IF isnull(dw_5.Object.ccpd_rescaj[il_fila]) OR dw_5.Object.ccpd_rescaj[il_fila] = 'A' THEN
		dw_5.Object.ccpd_rescom.protect = 1
	ELSE		
		dw_5.Object.ccpd_rescom.protect = 0
	END IF		
  
   IF (istr_mant.Argumento[22] =	'0' OR dw_1.GetItemStatus(il_Fila, 0, Primary!) = NewModified!) THEN 
		//il_fila					= 1
 		pb_acepta.Enabled		= True
		pb_cancela.Enabled	= True
//	ELSE  
//		dw_2.Enabled			= False	
//		dw_3.Enabled			= False	
//		dw_4.Enabled			= False	
//		dw_5.Enabled			= False	
//		dw_6.Enabled	   	= False	
//		
//		pb_acepta.Enabled		= False	
//		pb_cancela.Enabled	= False
//		pb_salir.Enabled		= True 	
 
	END IF 	
END IF 	
//	 
//IF (li_Grupo	= 1 OR li_Grupo = 6) OR &  /* por cambio de requermiento original 2009-01-08*/
//	(dw_1.GetItemStatus(il_Fila, 0, Primary!) = NewModified!)THEN 
//	dw_2.Enabled			= True	
//	dw_3.Enabled			= True	
//	dw_4.Enabled			= True	
//	dw_5.Enabled			= True	
//	dw_6.Enabled	   	= True	
//	pb_acepta.Enabled		= True	
//	pb_cancela.Enabled	= True
//	pb_salir.Enabled		= True 	
//ELSE
//	 dw_2.Enabled			= False	
//	dw_3.Enabled			= False	
//	dw_4.Enabled			= False	
//	dw_5.Enabled			= False	
//	dw_6.Enabled	   	= False	
//	pb_acepta.Enabled		= False	
//	pb_cancela.Enabled	= False
//	pb_salir.Enabled		= True 	
//	 
//END IF	
IF istr_mant.agrega = True THEN
	pb_acepta.Enabled	= True	
	pb_cancela.Enabled	= True
	pb_salir.Enabled		= True 	
	dw_2.Enabled			= True	
	dw_3.Enabled			= True	
	dw_4.Enabled			= True	
	dw_5.Enabled			= True	
	dw_6.Enabled	 	  	= True	
END IF
end event

event ue_deshace;call super::ue_deshace;IF istr_mant.Argumento[22] ='0'OR dw_1.GetItemStatus(il_fila, 0, Primary!) = NewModified!  THEN
	IF UpperBound(ias_campo) > 0 THEN
		dw_1.SetItem(il_fila, "ccpe_numero", Long(ias_campo[1]))
		dw_1.SetItem(il_fila, "plde_codigo", Integer(ias_campo[2]))
		dw_1.SetItem(il_fila, "clie_codigo", Integer(ias_campo[3]))
		dw_1.SetItem(il_fila, "cclo_numero", Integer(ias_campo[4]))
		dw_1.SetItem(il_fila, "ccpd_secuen", Integer(ias_campo[5]))
		dw_1.SetItem(il_fila, "ccpe_noguia", Integer(ias_campo[6]))
		dw_1.SetItem(il_fila, "ccpd_paleti", ias_campo[7])
		dw_1.SetItem(il_fila, "ccpd_rotula", ias_campo[8])
		dw_1.SetItem(il_fila, "ccpd_materi", ias_campo[9])
		dw_1.SetItem(il_fila, "ccpd_empaq",  ias_campo[10])
		dw_1.SetItem(il_fila, "ccpd_pesone", Integer(ias_campo[11]))
		dw_1.SetItem(il_fila, "ccpd_aparie", ias_campo[12])
		dw_1.SetItem(il_fila, "ccpd_nropaq", Integer(ias_campo[13]))
		dw_1.SetItem(il_fila, "ccpd_noraci", Integer(ias_campo[14]))
		dw_1.SetItem(il_fila, "ccpd_grdobr", Integer(ias_campo[15]))
		dw_1.SetItem(il_fila, "ccpd_temper", Integer(ias_campo[16]))
		dw_1.SetItem(il_fila, "ccpd_cobay1", Integer(ias_campo[17]))
		dw_1.SetItem(il_fila, "ccpd_cobay2", Integer(ias_campo[18]))
		dw_1.SetItem(il_fila, "ccpd_cobay3", Integer(ias_campo[19]))
		dw_1.SetItem(il_fila, "ccpd_cobay4", Integer(ias_campo[20]))
		dw_1.SetItem(il_fila, "ccpd_tamayo", Integer(ias_campo[21]))
		dw_1.SetItem(il_fila, "ccpd_tamba1", Integer(ias_campo[22]))
		dw_1.SetItem(il_fila, "ccpd_tamba2", Integer(ias_campo[23]))
		dw_1.SetItem(il_fila, "ccpd_tamba3", Integer(ias_campo[24]))
		dw_1.SetItem(il_fila, "ccpd_tamba4", Integer(ias_campo[25]))
		dw_1.SetItem(il_fila, "ccpd_promba", Integer(ias_campo[26]))
		dw_1.SetItem(il_fila, "ccpd_forade", Integer(ias_campo[27]))
		dw_1.SetItem(il_fila, "ccpd_foraap", Integer(ias_campo[28]))
		dw_1.SetItem(il_fila, "ccpd_pesras", Integer(ias_campo[29]))
		dw_1.SetItem(il_fila, "ccpd_pesrab", Integer(ias_campo[30]))
		dw_1.SetItem(il_fila, "ccpd_residu", Integer(ias_campo[31]))
		dw_1.SetItem(il_fila, "ccpd_mancha", Integer(ias_campo[32]))
		dw_1.SetItem(il_fila, "ccpd_insect", Integer(ias_campo[33]))
		dw_1.SetItem(il_fila, "ccpd_deslev", Integer(ias_campo[34]))
		dw_1.SetItem(il_fila, "ccpd_desmod", Integer(ias_campo[35]))
		dw_1.SetItem(il_fila, "ccpd_desalt", Integer(ias_campo[36]))
		dw_1.SetItem(il_fila, "ccpd_punuba", Integer(ias_campo[37]))
		dw_1.SetItem(il_fila, "ccpd_pununi", Integer(ias_campo[38]))
		dw_1.SetItem(il_fila, "ccpd_punura", Integer(ias_campo[39]))
		dw_1.SetItem(il_fila, "ccpd_pacida", Integer(ias_campo[40]))
		dw_1.SetItem(il_fila, "ccpd_opnbay", Integer(ias_campo[41]))
		dw_1.SetItem(il_fila, "ccpd_coblan", Integer(ias_campo[42]))
		dw_1.SetItem(il_fila, "ccpd_cocris", Integer(ias_campo[43]))
		dw_1.SetItem(il_fila, "ccpd_codesp", Integer(ias_campo[44]))
		dw_1.SetItem(il_fila, "ccpd_conso2", Integer(ias_campo[45]))
		dw_1.SetItem(il_fila, "ccpd_cobapa", Integer(ias_campo[46]))
		dw_1.SetItem(il_fila, "ccpd_codesg", Integer(ias_campo[47]))
		dw_1.SetItem(il_fila, "ccpd_baacuo", Integer(ias_campo[48]))
		dw_1.SetItem(il_fila, "ccpd_coprem", Integer(ias_campo[49]))
		dw_1.SetItem(il_fila, "ccpd_otcond", Integer(ias_campo[50]))
		dw_1.SetItem(il_fila, "ccpd_resemb", ias_campo[51])
		dw_1.SetItem(il_fila, "ccpd_rescal", ias_campo[52])
		dw_1.SetItem(il_fila, "ccpd_rescon", ias_campo[53])
		dw_1.SetItem(il_fila, "ccpd_rescaj", ias_campo[54])
		dw_1.SetItem(il_fila, "espe_codigo", Integer(ias_campo[55]))
		dw_1.SetItem(il_fila, "ccda_secuen", Integer(ias_campo[56]))	
		dw_1.SetItem(il_fila, "ccpd_selecc", ias_campo[57])	
		dw_1.SetItem(il_fila, "ccpd_embala", ias_campo[58])
		dw_1.SetItem(il_fila, "ccpd_caotro", Integer(ias_campo[59]))
		dw_1.SetItem(il_fila, "ccpd_coidio", Integer(ias_campo[60]))
		dw_1.SetItem(il_fila, "ccpd_cootro", Integer(ias_campo[61]))
		dw_1.SetItem(il_fila, "ccpd_rescom", ias_campo[62])
		dw_1.SetItem(il_fila, "ccpd_segpal", ias_campo[63])	
		dw_1.SetItem(il_fila, "ccpd_respal", ias_campo[64])
		dw_1.SetItem(il_fila, "ccpd_causal1", Integer(ias_campo[65]))
		dw_1.SetItem(il_fila, "ccpd_causal2", Integer(ias_campo[66]))
		dw_1.SetItem(il_fila, "ccpd_causal3", Integer(ias_campo[67]))
		dw_1.SetItem(il_fila, "ccpd_npalle", Long(ias_campo[70]))
		
	END IF
END IF

end event

event ue_antesguardar;Integer	li_cont, li_suma, li_Nula, li_punuba, li_pununi, li_punura
String	ls_mensaje, ls_colu[]

SetNull(li_Nula)
li_suma  = 0 

li_punuba	=	dw_4.Object.ccpd_punuba[il_Fila]
li_pununi	=	dw_4.Object.ccpd_pununi[il_Fila]
li_punura	=	dw_4.Object.ccpd_punura[il_Fila]


IF IsNull(dw_3.object.ccpd_cobay1[il_fila]) = FALSE THEN
	li_suma = li_suma + (dw_3.object.ccpd_cobay1[il_fila])
END IF		

IF IsNull(dw_3.object.ccpd_cobay2[il_fila]) = FALSE THEN
	li_suma = li_suma + (dw_3.object.ccpd_cobay2[il_fila])
END IF	

IF IsNull(dw_3.object.ccpd_cobay3[il_fila]) = FALSE THEN
	li_suma = li_suma + (dw_3.object.ccpd_cobay3[il_fila])
END IF	

IF IsNull(dw_3.object.ccpd_cobay4[il_fila]) = FALSE THEN
	li_suma = li_suma + (dw_3.object.ccpd_cobay4[il_fila])
END IF	

IF li_suma > 100 THEN
	MessageBox("Error de Consistencia", "La Suma de Color Bayas debe dar siempre Menor 100.", StopSign!, Ok!)
	dw_3.SetFocus()
	Message.DoubleParm = -1	
END IF

li_suma  = 0 
/*
Cambio Solicitado Por V.Costa
*/
//IF IsNull(dw_3.object.ccpd_tamayo[il_fila]) = FALSE THEN
//	li_suma = li_suma + (dw_3.object.ccpd_tamayo[il_fila])	
//END IF	
//
//IF IsNull(dw_3.object.ccpd_tamba1[il_fila]) = FALSE THEN
//	li_suma = li_suma + (dw_3.object.ccpd_tamba1[il_fila])	
//END IF	
//
//IF IsNull(dw_3.object.ccpd_tamba2[il_fila]) = FALSE THEN
//	li_suma = li_suma + (dw_3.object.ccpd_tamba2[il_fila])	
//END IF
//
//IF IsNull(dw_3.object.ccpd_tamba3[il_fila]) = FALSE THEN
//	li_suma = li_suma + (dw_3.object.ccpd_tamba3[il_fila])	
//END IF
//
//IF IsNull(dw_3.object.ccpd_tamba4[il_fila]) = FALSE THEN
//	li_suma = li_suma + (dw_3.object.ccpd_tamba4[il_fila])	
//END IF
//
//IF IsNull(dw_3.object.ccpd_tameno[il_fila]) = FALSE THEN
//	li_suma = li_suma + (dw_3.object.ccpd_tameno[il_fila])	
//END IF
//
//IF li_suma <> 100 THEN
//	MessageBox("Error de Consistencia", "La Suma de Tamaño Bayas debe dar siempre 100.", StopSign!, Ok!)
//	dw_3.SetFocus()
//	Message.DoubleParm = -1
//END IF

li_suma  = 0 

IF IsNull(dw_3.object.ccpd_promba[il_fila]) = FALSE THEN
	li_suma = li_suma + (dw_3.object.ccpd_promba[il_fila])	
END IF	

IF li_suma = 0 THEN
	MessageBox("Error de Consistencia", "Debe Ingresar Peso Promedio de Bayas.", StopSign!, Ok!)
	dw_3.SetFocus()
	Message.DoubleParm = -1
END IF 

IF Tab_1.TabPage_2.dw_Calidad.Object.ccpd_forade[il_Fila]	>	100 THEN
	MessageBox("Atención","La Forma del Racimo debe Sumar hasta 100")	
	dw_3.SetFocus()
	Message.DoubleParm = -1
END IF

IF Tab_1.TabPage_2.dw_Calidad.Object.ccpd_foraap[il_Fila]	>	100 THEN	
	MessageBox("Atención","El Peso del Racimo debe Suma hasta 100")
	dw_3.SetFocus()
	Message.DoubleParm = -1	
END IF

IF li_punura = 0 AND (li_punuba > 0 OR li_pununi > 0) THEN
	li_cont ++
	ls_mensaje		 	= ls_mensaje + "~rNº de Racimos - (Pudrición - Botritis)"
	ls_colu[li_cont] 	= "ccpd_punura"
END IF

IF Isnull(dw_2.object.ccpd_paleti[il_fila]) or dw_2.object.ccpd_paleti[il_fila] = '' THEN
	li_cont ++
	ls_mensaje		 	= ls_mensaje + "~rPaletizaje"
	ls_colu[li_cont] 	= "ccpd_paleti"
END IF
IF Isnull(dw_2.object.ccpd_rotula[il_fila]) or dw_2.object.ccpd_rotula[il_fila] = '' THEN
	li_cont ++
	ls_mensaje		 	= ls_mensaje + "~rRotulación"
	ls_colu[li_cont] 	= "ccpd_rotula"
END IF
IF Isnull(dw_2.object.ccpd_materi[il_fila]) or dw_2.object.ccpd_materi[il_fila] = '' THEN
	li_cont ++
	ls_mensaje		 	= ls_mensaje + "~rMateriales"
	ls_colu[li_cont] 	= "ccpd_materi"
END IF
//IF Isnull(dw_2.object.ccpd_empaq[il_fila]) or dw_2.object.ccpd_empaq[il_fila] = '' THEN
//	li_cont ++
//	ls_mensaje		 	= ls_mensaje + "~rEmpaque"
//	ls_colu[li_cont] 	= "ccpd_empaq"
//END IF
IF Isnull(dw_2.object.ccpd_pesone[il_fila]) or dw_2.object.ccpd_pesone[il_fila] = 0  THEN
	li_cont ++
	ls_mensaje		 	= ls_mensaje + "~rPeso Neto"
	ls_colu[li_cont] 	= "ccpd_pesone"
END IF
//IF Isnull(dw_2.object.ccpd_aparie[il_fila]) or dw_2.object.ccpd_aparie[il_fila] = '' THEN
//	li_cont ++
//	ls_mensaje		 	= ls_mensaje + "~rApariencia"
//	ls_colu[li_cont] 	= "ccpd_aparie"
//END IF
//IF Isnull(dw_2.object.ccpd_nropaq[il_fila]) or dw_2.object.ccpd_nropaq[il_fila] = 0 THEN
//	li_cont ++
//	ls_mensaje		 	= ls_mensaje + "~rNúmero Paquetes"
//	ls_colu[li_cont] 	= "ccpd_nropaq"
//END IF
//IF Isnull(dw_2.object.ccpd_noraci[il_fila]) or dw_2.object.ccpd_noraci[il_fila] = 0 THEN
//	li_cont ++
//	ls_mensaje		 	= ls_mensaje + "~rNúmero de Racimo"
//	ls_colu[li_cont] 	= "ccpd_noraci"
//END IF
IF Isnull(dw_3.object.ccpd_grdobr[il_fila]) or dw_3.object.ccpd_grdobr[il_fila] = 0 THEN
	li_cont ++
	ls_mensaje		 	= ls_mensaje + "~rGrados Brix"
	ls_colu[li_cont] 	= "ccpd_grdobr"
END IF

IF Isnull(dw_5.object.ccpd_segpal[il_fila]) or dw_5.object.ccpd_segpal[il_fila] = '' THEN
	li_cont ++
	ls_mensaje		 	= ls_mensaje + "~rSegregación Pallet"
	ls_colu[li_cont] 	= "ccpd_segpal"
END IF

IF Isnull(dw_2.object.ccpd_resemb[il_fila]) or dw_2.object.ccpd_resemb[il_fila] = '' THEN
	li_cont ++
	ls_mensaje		 	= ls_mensaje + "~rResolución Embalaje"
	ls_colu[li_cont] 	= "ccpd_resemb"
END IF
IF Isnull(dw_3.object.ccpd_rescal[il_fila]) or dw_3.object.ccpd_rescal[il_fila] = '' THEN
	li_cont ++
	ls_mensaje		 	= ls_mensaje + "~rResolución Calidad"
	ls_colu[li_cont] 	= "ccpd_rescal"
END IF
IF Isnull(dw_4.object.ccpd_rescon[il_fila]) or dw_4.object.ccpd_rescon[il_fila] = '' THEN
	li_cont ++
	ls_mensaje		 	= ls_mensaje + "~rResolución Condición"
	ls_colu[li_cont] 	= "ccpd_rescon"
END IF


IF Isnull(dw_5.object.ccpd_rescaj[il_fila]) or dw_5.object.ccpd_rescaj[il_fila] = '' THEN
	li_cont ++
	ls_mensaje		 	= ls_mensaje + "~rResolución Caja"
	ls_colu[li_cont] 	= "ccpd_rescaj"
ELSE
	IF dw_5.object.ccpd_rescaj[il_fila] = 'O' THEN
		
			IF dw_5.object.ccpd_rescaj[il_fila] = 'O'  THEN
		               IF (Isnull(dw_5.object.ccpd_cobjem[il_fila]) or dw_5.object.ccpd_cobjem[il_fila] = 0) or & 
                            (Isnull(dw_5.object.ccpd_porob1[il_fila]) or dw_5.object.ccpd_porob1[il_fila] = 0)  THEN
							 li_cont ++
							 ls_mensaje		 	= ls_mensaje + "~rResolución Caja Debe Ingresar al Menos una Causal y su Respectivo %"
							 ls_colu[li_cont] 	= "ccpd_rescaj"
		                END IF	
			END IF
						
			IF dw_5.object.ccpd_cobjca[il_fila] > 0 THEN
				IF isnull(dw_5.Object.ccpd_porob2[il_fila]) OR dw_5.Object.ccpd_porob2[il_fila] = 0 THEN
					li_cont ++
					ls_mensaje		= ls_mensaje + "~rFalta el ingreso de Porcentaje de Causal 2"
				    ls_colu[li_cont] 	= "ccpd_rescaj"
				END IF	
			END IF
				
			IF dw_5.object.ccpd_cobjco[il_fila] > 0 THEN
				IF isnull(dw_5.Object.ccpd_porob3[il_fila]) OR dw_5.Object.ccpd_porob3[il_fila] = 0 THEN
					li_cont ++
					ls_mensaje		= ls_mensaje + "~rFalta el ingreso de Porcentaje de Causal 3"
				    ls_colu[li_cont] 	= "ccpd_rescaj"
				END IF	
			END IF
		
	END IF
END IF


IF Isnull(dw_2.object.ccpd_respal[il_fila]) or dw_2.object.ccpd_respal[il_fila] = '' THEN
	li_cont ++
	ls_mensaje		 	= ls_mensaje + "~rResolución Pallet"
	ls_colu[li_cont] 	= "ccpd_respal"
ELSE
	IF dw_5.object.ccpd_respal[il_fila] = 'R' or  dw_5.object.ccpd_respal[il_fila] = 'P' THEN
            IF (Isnull(dw_5.object.ccpd_causal1[il_fila]) or dw_5.object.ccpd_causal1[il_fila] = 0) or & 
                 (Isnull(dw_5.object.ccpd_pocau1[il_fila]) or dw_5.object.ccpd_pocau1[il_fila] = 0)  THEN			
			    li_cont ++
   			    ls_mensaje		 	= ls_mensaje + "~rResolución Pallet Debe Ingresar al Menos una Causal y su Respectivo %"
			    ls_colu[li_cont] 	= "ccpd_respal"
			END IF		 

		IF dw_5.object.ccpd_causal2[il_fila] > 0 THEN
			IF isnull(dw_5.Object.ccpd_pocau2[il_fila]) OR dw_5.Object.ccpd_pocau2[il_fila] = 0 THEN
				li_cont ++
				ls_mensaje		= ls_mensaje + "~rFalta el ingreso de Porcentaje de Causal 2"
			    ls_colu[li_cont] 	= "ccpd_respal"
			END IF	
		END IF
				
		IF dw_5.object.ccpd_causal3[il_fila] > 0 THEN
			IF isnull(dw_5.Object.ccpd_pocau3[il_fila]) OR dw_5.Object.ccpd_pocau3[il_fila] = 0 THEN
				li_cont ++
				ls_mensaje		= ls_mensaje + "~rFalta el ingreso de Porcentaje de Causal 3"
			    ls_colu[li_cont] 	= "ccpd_respal"
			END IF	
		END IF		

		
	END IF	
END IF





IF Isnull(dw_6.object.ccpd_npalle[il_fila]) or dw_6.object.ccpd_npalle[il_fila] = 0 THEN
	li_cont ++
	ls_mensaje		 	= ls_mensaje + "~rNúmero de Pallet"
	ls_colu[li_cont] 	= "ccpd_npalle"
END IF

//ccpd_npalle

//IF dw_5.object.ccpd_rescaj[il_fila] = 'O' THEN
//	IF Isnull(dw_5.object.ccda_secuen[il_fila]) or dw_5.object.ccda_secuen[il_fila] = 0 THEN
//		li_cont ++
//		ls_mensaje		 	= ls_mensaje + "~rCausal Objeción"
//		ls_colu[li_cont] 	= "ccda_secuen"
//	END IF
//END IF	


IF dw_2.object.ccpd_resemb[il_fila] = 'M' OR dw_3.object.ccpd_rescal[il_fila] = 'M'  OR dw_4.object.ccpd_rescon[il_fila] = 'M'THEN
	IF Isnull(dw_5.object.ccpd_cobjem[il_fila]) or dw_5.object.ccpd_cobjem[il_fila] = 0 AND &
	    Isnull(dw_5.object.ccpd_cobjca[il_fila]) or dw_5.object.ccpd_cobjca[il_fila] = 0 AND &
	    Isnull(dw_5.object.ccpd_cobjco[il_fila]) or dw_5.object.ccpd_cobjco[il_fila] = 0 THEN
		li_cont ++
		ls_mensaje		 	= ls_mensaje + "~rFalta Ingreso de Una Causal Objeción"
		ls_colu[li_cont] 	= "ccpd_cobjem"
	END IF
END IF
/*
IF dw_3.object.ccpd_rescal[il_fila] = 'M' THEN
	IF Isnull(dw_3.object.ccpd_cobjca[il_fila]) or dw_3.object.ccpd_cobjca[il_fila] = 0 THEN
		li_cont ++
		ls_mensaje		 	= ls_mensaje + "~rCausal Objeción Calidad"
		ls_colu[li_cont] 	= "ccpd_cobjca"
	END IF
END IF

IF dw_4.object.ccpd_rescon[il_fila] = 'M' THEN
	IF Isnull(dw_4.object.ccpd_cobjco[il_fila]) or dw_4.object.ccpd_cobjco[il_fila] = 0 THEN
		li_cont ++
		ls_mensaje		 	= ls_mensaje + "~rCausal Objeción Condición"
		ls_colu[li_cont] 	= "ccpd_cobjco"
	END IF
END IF
*/


IF li_cont > 0 THEN
	MessageBox("Error de Consistencia", "Falta el ingreso de :" + ls_mensaje + ".", StopSign!, Ok!)
	dw_4.SetColumn(ls_colu[1]) 
	dw_4.SetFocus()
	Message.DoubleParm = -1
END IF
end event

event ue_nuevo;String	ls_rut, ls_Nula

SetNull(ls_Nula)

ib_ok = True

This.TriggerEvent("ue_guardar")

IF Message.DoubleParm = -1 THEN ib_ok = False

IF ib_ok = False THEN RETURN

wf_nuevo()

tab_1.SelectTab(1)

dw_1.SetFocus()
dw_1.SetItem(il_fila,"clie_codigo",Integer(istr_mant.Argumento[7]))
dw_1.SetItem(il_fila,"plde_codigo",Integer(istr_mant.Argumento[4]))
dw_1.SetItem(il_fila,"espe_codigo",Integer(istr_mant.Argumento[9]))
dw_1.SetItem(il_fila,"ccpe_noguia",Long(istr_mant.Argumento[13]))

dw_2.SetRow(il_fila)
dw_3.SetRow(il_fila)
dw_4.SetRow(il_fila)
dw_5.SetRow(il_fila)
dw_6.SetRow(il_fila)

dw_2.ScrollToRow(il_fila)
dw_3.ScrollToRow(il_fila)
dw_4.ScrollToRow(il_fila)
dw_5.ScrollToRow(il_fila)
dw_6.ScrollToRow(il_fila)

dw_2.SetItem(il_fila,"ccpd_paleti",ls_Nula)
dw_2.SetItem(il_fila,"ccpd_rotula",ls_Nula)
dw_2.SetItem(il_fila,"ccpd_materi",ls_Nula)
dw_2.SetItem(il_fila,"ccpd_empaq",ls_Nula)

dw_5.SetItem(il_fila,"ccpd_resemb",ls_Nula)
dw_5.SetItem(il_fila,"ccpd_rescal",ls_Nula)
dw_5.SetItem(il_fila,"ccpd_rescon",ls_Nula)
dw_5.SetItem(il_fila,"ccpd_rescaj",ls_Nula)
end event

event open;Long		ll_trans, ll_planilla, ll_guia
Integer	li_planta, li_cliente, li_zona, li_especie

This.Icon	=	Gstr_apl.Icono

iuo_ctlcaldanoespecie	=	Create uo_ctlcaldanoespecie
iuo_Predio				=	Create uo_prodpredio	
iuo_Cuartel				=	Create uo_prodCuarteles

Postevent("ue_recuperadatos")

istr_mant = Message.PowerObjectParm

x	= 100
y	= 450

SetPointer(HourGlass!)

li_planta		=	Integer(istr_mant.argumento[4])
li_cliente		=	Integer(istr_mant.argumento[7])
li_zona		=	Integer(istr_mant.argumento[3])
li_especie	=	Integer(istr_mant.argumento[9])
ll_planilla		=	Long(istr_mant.argumento[2])
ll_guia		=	Long(istr_mant.argumento[13])

dw_1.GetChild("clie_codigo", idwc_clientes)
idwc_clientes.SetTransObject(sqlca)
idwc_clientes.Retrieve()
dw_1.SetItem(il_fila, "clie_codigo",li_cliente)

dw_1.GetChild("plde_codigo", idwc_plantas)
idwc_plantas.SetTransObject(sqlca)
idwc_plantas.Retrieve(1)
dw_1.SetItem(il_fila, "plde_codigo",li_planta)

Tab_1.TabPage_5.dw_id.GetChild("prpr_codigo", idwc_predio)
idwc_predio.SetTransObject(sqlca)
idwc_predio.Retrieve(Long(istr_mant.Argumento[8]))

Tab_1.TabPage_5.dw_id.GetChild("prcc_codigo", idwc_cuartel)
idwc_cuartel.SetTransObject(sqlca)
idwc_cuartel.Retrieve(Long(istr_mant.Argumento[8]), -1)

dw_1.SetItem(il_fila, "ccpe_numero",ll_planilla)
dw_1.SetItem(il_fila, "ccpe_noguia",ll_guia)
dw_1.SetItem(il_fila, "espe_codigo",li_especie)

dw_2	=	Tab_1.TabPage_1.dw_embalaje
dw_3	=	Tab_1.TabPage_2.dw_calidad
dw_4	=	Tab_1.TabPage_3.dw_condicion
dw_5	=	Tab_1.TabPage_4.dw_resolucion
dw_6	=	Tab_1.TabPage_5.dw_id

dw_1.SetTransObject(sqlca)
dw_2.SetTransObject(sqlca)
dw_3.SetTransObject(sqlca)
dw_4.SetTransObject(sqlca)
dw_5.SetTransObject(sqlca)
dw_6.SetTransObject(sqlca)

istr_mant.dw.ShareData(dw_1)

dw_1.ShareData(dw_2)
dw_1.ShareData(dw_3)
dw_1.ShareData(dw_4)
dw_1.ShareData(dw_5)
dw_1.ShareData(dw_6)

//dw_2.Object.ccpd_cobjem.protect = 1
//dw_3.Object.ccpd_cobjca.protect = 1
//dw_4.Object.ccpd_cobjco.protect = 1
// 
dw_5.Object.ccpd_rescom.protect = 1
end event

event resize;//
end event

event ue_guardar;SetPointer(HourGlass!)

Message.DoubleParm = 0


IF dw_1.AcceptText() = -1 THEN
	Message.DoubleParm = -1 
	RETURN
END IF

IF dw_2.AcceptText() = -1 THEN
	Message.DoubleParm = -1 
	RETURN
END IF

IF dw_3.AcceptText() = -1 THEN
	Message.DoubleParm = -1 
	RETURN
END IF

IF dw_4.AcceptText() = -1 THEN
	Message.DoubleParm = -1 
	RETURN
END IF

IF dw_5.AcceptText() = -1 THEN
	Message.DoubleParm = -1 
	RETURN
END IF

IF dw_6.AcceptText() = -1 THEN
	Message.DoubleParm = -1 
	RETURN
END IF


IF dw_1.AcceptText() = -1 THEN RETURN

SetPointer(HourGlass!)

w_main.SetMicroHelp("Grabando información...")

Message.DoubleParm = 0

TriggerEvent("ue_antesguardar")

IF Message.DoubleParm = -1 THEN RETURN 

end event

type pb_ultimo from w_mant_detalle`pb_ultimo within w_mant_deta_ctlcalplanillauvas
end type

type pb_siguiente from w_mant_detalle`pb_siguiente within w_mant_deta_ctlcalplanillauvas
end type

type pb_anterior from w_mant_detalle`pb_anterior within w_mant_deta_ctlcalplanillauvas
end type

type pb_primero from w_mant_detalle`pb_primero within w_mant_deta_ctlcalplanillauvas
end type

type pb_cancela from w_mant_detalle`pb_cancela within w_mant_deta_ctlcalplanillauvas
integer x = 2811
integer y = 344
integer taborder = 40
end type

type pb_acepta from w_mant_detalle`pb_acepta within w_mant_deta_ctlcalplanillauvas
integer x = 2816
integer y = 156
integer taborder = 30
boolean default = false
end type

event pb_acepta::clicked;String	ls_colu[],ls_mensaje
Integer	li_cont
istr_mant.respuesta = 1

IF istr_mant.agrega THEN
	Parent.TriggerEvent("ue_nuevo")
ELSE

	IF Isnull(dw_6.object.ccpd_npalle[il_fila]) or dw_6.object.ccpd_npalle[il_fila] = 0 THEN
		li_cont ++
		ls_mensaje		 	= ls_mensaje + "~rNúmero de Pallet"
		ls_colu[li_cont] 	= "ccpd_npalle"
	END IF

	IF Isnull(dw_5.object.ccpd_segpal[il_fila]) or dw_5.object.ccpd_segpal[il_fila] = '' THEN
		li_cont ++
		ls_mensaje		 	= ls_mensaje + "~rSegregación Pallet"
		ls_colu[li_cont] 	= "ccpd_segpal"
	END IF


/**/
    IF Isnull(dw_5.object.ccpd_rescaj[il_fila]) or dw_5.object.ccpd_rescaj[il_fila] = '' THEN
		li_cont ++
		ls_mensaje		 	= ls_mensaje + "~rResolución Caja"
		ls_colu[li_cont] 	= "ccpd_rescaj"
	ELSE	
			IF dw_5.object.ccpd_rescaj[il_fila] = 'O'  THEN
		               IF (Isnull(dw_5.object.ccpd_cobjem[il_fila]) or dw_5.object.ccpd_cobjem[il_fila] = 0) or & 
                            (Isnull(dw_5.object.ccpd_porob1[il_fila]) or dw_5.object.ccpd_porob1[il_fila] = 0)  THEN
							 li_cont ++
							 ls_mensaje		 	= ls_mensaje + "~rResolución Caja Debe Ingresar al Menos una Causal y su Respectivo %"
							 ls_colu[li_cont] 	= "ccpd_rescaj"
		                END IF	
			END IF
						
			IF dw_5.object.ccpd_cobjca[il_fila] > 0 THEN
				IF isnull(dw_5.Object.ccpd_porob2[il_fila]) OR dw_5.Object.ccpd_porob2[il_fila] = 0 THEN
					li_cont ++
					ls_mensaje		= ls_mensaje + "~rFalta el ingreso de Porcentaje de Causal 2"
				    ls_colu[li_cont] 	= "ccpd_rescaj"
				END IF	
			END IF
				
			IF dw_5.object.ccpd_cobjco[il_fila] > 0 THEN
				IF isnull(dw_5.Object.ccpd_porob3[il_fila]) OR dw_5.Object.ccpd_porob3[il_fila] = 0 THEN
					li_cont ++
					ls_mensaje		= ls_mensaje + "~rFalta el ingreso de Porcentaje de Causal 3"
				    ls_colu[li_cont] 	= "ccpd_rescaj"
				END IF	
			END IF
	END IF

	
	IF Isnull(dw_5.object.ccpd_respal[il_fila]) or dw_5.object.ccpd_respal[il_fila] = '' THEN
		li_cont ++
		ls_mensaje		 	= ls_mensaje + "~rResolución Pallet"
		ls_colu[li_cont] 	= "ccpd_respal"
	ELSE
		IF dw_5.object.ccpd_respal[il_fila] = 'R' or  dw_5.object.ccpd_respal[il_fila] = 'P'  THEN
             IF (Isnull(dw_5.object.ccpd_causal1[il_fila]) or dw_5.object.ccpd_causal1[il_fila] = 0) or & 
                 (Isnull(dw_5.object.ccpd_pocau1[il_fila]) or dw_5.object.ccpd_pocau1[il_fila] = 0)  THEN			
			    li_cont ++
   			    ls_mensaje		 	= ls_mensaje + "~rResolución Pallet Debe Ingresar al Menos una Causal y su Respectivo %"
			    ls_colu[li_cont] 	= "ccpd_respal"
			END IF		 
		END IF

		IF dw_5.object.ccpd_causal2[il_fila] > 0 THEN
			IF isnull(dw_5.Object.ccpd_pocau2[il_fila]) OR dw_5.Object.ccpd_pocau2[il_fila] = 0 THEN
				li_cont ++
				ls_mensaje		= ls_mensaje + "~rFalta el ingreso de Porcentaje de Causal 2"
			    ls_colu[li_cont] 	= "ccpd_respal"
			END IF	
		END IF
				
		IF dw_5.object.ccpd_causal3[il_fila] > 0 THEN
			IF isnull(dw_5.Object.ccpd_pocau3[il_fila]) OR dw_5.Object.ccpd_pocau3[il_fila] = 0 THEN
				li_cont ++
				ls_mensaje		= ls_mensaje + "~rFalta el ingreso de Porcentaje de Causal 3"
			    ls_colu[li_cont] 	= "ccpd_respal"
			END IF	
		END IF		
	END IF
	
	
	
/**/		

	
		

	
	IF li_cont > 0 THEN
		MessageBox("Error de Consistencia", "Falta el ingreso de :" + ls_mensaje + ".", StopSign!, Ok!)
		dw_5.SetColumn(ls_colu[1]) 
		dw_5.SetFocus()
		Return
	END IF

	CloseWithReturn(Parent, istr_mant)
END IF
end event

type pb_salir from w_mant_detalle`pb_salir within w_mant_deta_ctlcalplanillauvas
integer x = 2811
integer y = 564
integer taborder = 50
end type

type dw_1 from w_mant_detalle`dw_1 within w_mant_deta_ctlcalplanillauvas
integer x = 55
integer width = 2565
integer height = 636
string dataobject = "dw_mant_ctlcalplanillauvas"
end type

event dw_1::itemchanged;call super::itemchanged;//String	ls_columna
//
//ls_columna = dwo.name
//
//CHOOSE CASE ls_columna
//	CASE "rpcf_numero"
//		IF Duplicado(data,1) THEN
//			This.SetItem(il_fila, ls_columna, Integer(ias_campo[2]))
//			RETURN 1
//		END IF
//	
//END CHOOSE
end event

type tab_1 from tab within w_mant_deta_ctlcalplanillauvas
integer x = 82
integer y = 812
integer width = 3355
integer height = 1112
integer taborder = 20
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
boolean italic = true
long backcolor = 12639424
boolean focusonbuttondown = true
boolean boldselectedtext = true
integer selectedtab = 1
tabpage_5 tabpage_5
tabpage_1 tabpage_1
tabpage_2 tabpage_2
tabpage_3 tabpage_3
tabpage_4 tabpage_4
end type

on tab_1.create
this.tabpage_5=create tabpage_5
this.tabpage_1=create tabpage_1
this.tabpage_2=create tabpage_2
this.tabpage_3=create tabpage_3
this.tabpage_4=create tabpage_4
this.Control[]={this.tabpage_5,&
this.tabpage_1,&
this.tabpage_2,&
this.tabpage_3,&
this.tabpage_4}
end on

on tab_1.destroy
destroy(this.tabpage_5)
destroy(this.tabpage_1)
destroy(this.tabpage_2)
destroy(this.tabpage_3)
destroy(this.tabpage_4)
end on

type tabpage_5 from userobject within tab_1
integer x = 18
integer y = 112
integer width = 3319
integer height = 984
long backcolor = 16711680
string text = "Identificación"
long tabtextcolor = 33554432
long tabbackcolor = 12632256
long picturemaskcolor = 536870912
dw_id dw_id
end type

on tabpage_5.create
this.dw_id=create dw_id
this.Control[]={this.dw_id}
end on

on tabpage_5.destroy
destroy(this.dw_id)
end on

type dw_id from uo_dw within tabpage_5
integer x = 306
integer y = 56
integer width = 2542
integer height = 856
integer taborder = 21
string dataobject = "dw_mant_ctlcalplanilladetalle_id"
boolean vscrollbar = false
end type

event itemerror;RETURN 1
end event

event losefocus;call super::losefocus;Tab_1.TabPage_1.dw_Embalaje.accepttext()
Tab_1.TabPage_2.dw_Calidad.accepttext()
Tab_1.TabPage_3.dw_condicion.accepttext()
Tab_1.TabPage_4.dw_Resolucion.accepttext()



end event

event dwnkey;call super::dwnkey;IF KeyDown(KeyDownArrow!) OR KeyDown(KeyUpArrow!) THEN
		RETURN 1
ELSE
		RETURN 0
END IF


end event

event itemfocuschanged;call super::itemfocuschanged;ib_datos_ok = true
if rowcount() < 1 or getrow() = 0 or ib_borrar then 
	ib_datos_ok = false
else
	SetRow(il_fila)
	ScrolltoRow(il_fila)
end if
end event

event itemchanged;call super::itemchanged;String		ls_Columna, ls_Nula
Integer	li_ccajas

SetNull(ls_Nula)

ls_Columna	=	dwo.Name

Choose Case ls_Columna
	Case 'prpr_codigo'
		If Not iuo_Predio.Existe(Integer(Data), This.Object.prod_codigo[Row], True, Sqlca) Then
			This.SetItem(Row, ls_Columna, Integer(ls_Nula))
			Return 1
		Else
			dw_6.GetChild("prcc_codigo", idwc_cuartel)
			idwc_cuartel.SetTransObject(sqlca)
			idwc_cuartel.Retrieve(This.Object.prod_codigo[Row], iuo_Predio.Codigo)
		End If
		
	Case 'prcc_codigo'
		If Not iuo_Cuartel.Existe(This.Object.prod_codigo[Row], iuo_Predio.Codigo, Integer(Data), True, Sqlca) Then
			This.SetItem(Row, ls_Columna, Integer(ls_Nula))
			Return 1
		End If
		
	Case "ccpd_npalle"
		If Not IsNull(data) and data <> '' And gstr_parlote.codgen <> 1 Then
			This.Object.paen_ccajas.Protect = 1
			li_ccajas = ExistePallet(Integer(istr_mant.argumento[7]),Integer(istr_mant.argumento[4]),Long(istr_mant.argumento[1]),long(data))
			
			If IsNull(li_ccajas) Then
				This.SetItem(Row, ls_Columna, Integer(ls_Nula))
				dw_5.setcolumn("ccpd_npalle")
				dw_5.setfocus()
				Return 1
			Else
				wf_PredioPallet(Integer(istr_mant.argumento[7]),Integer(istr_mant.argumento[4]),Long(istr_mant.argumento[1]),long(data))
				
				dw_6.Object.paen_ccajas[il_fila] = li_ccajas
				If il_fila > 1 Then
					If dw_1.Object.ccpd_npalle[il_Fila - 1] = Long(data) Then
						Tab_1.TabPage_4.dw_resolucion.Object.ccpd_rescaj[il_fila]	=	dw_1.Object.ccpd_rescaj[il_fila - 1]
						Tab_1.TabPage_4.dw_resolucion.Object.ccpd_rescom[il_fila]	=	dw_1.Object.ccpd_rescom[il_fila - 1]
						Tab_1.TabPage_4.dw_resolucion.Object.ccpd_segpal[il_fila]	=	dw_1.Object.ccpd_segpal[il_fila - 1]
						Tab_1.TabPage_4.dw_resolucion.Object.ccpd_respal[il_fila]	=	dw_1.Object.ccpd_respal[il_fila - 1]
						Tab_1.TabPage_4.dw_resolucion.Object.ccpd_causal1[il_fila]	=	dw_1.Object.ccpd_causal1[il_fila - 1]
						Tab_1.TabPage_4.dw_resolucion.Object.ccpd_causal2[il_fila]	=	dw_1.Object.ccpd_causal2[il_fila - 1]
						Tab_1.TabPage_4.dw_resolucion.Object.ccpd_causal3[il_fila]	=	dw_1.Object.ccpd_causal3[il_fila - 1]
					End If
				End If
			End If	
		Else
			This.Object.paen_ccajas.Protect = 0			
		End If

End Choose

end event

type tabpage_1 from userobject within tab_1
integer x = 18
integer y = 112
integer width = 3319
integer height = 984
long backcolor = 16711680
string text = "Embalaje"
long tabtextcolor = 33554432
long tabbackcolor = 12632256
long picturemaskcolor = 536870912
dw_embalaje dw_embalaje
end type

on tabpage_1.create
this.dw_embalaje=create dw_embalaje
this.Control[]={this.dw_embalaje}
end on

on tabpage_1.destroy
destroy(this.dw_embalaje)
end on

type dw_embalaje from uo_dw within tabpage_1
integer x = 197
integer y = 80
integer width = 2912
integer height = 812
integer taborder = 11
string dataobject = "dw_mant_ctlcalplanillauvas_embalaje"
boolean vscrollbar = false
end type

event itemchanged;call super::itemchanged;String	ls_Columna, ls_Nula, ls_resolucioncaja
Integer	li_ValoRetorno
SetNull(ls_Nula)

ls_Columna	=	dwo.Name

CHOOSE CASE ls_Columna			
	CASE  "ccpd_paleti","ccpd_rotula","ccpd_materi","ccpd_empaq", &
			"ccpd_aparie","ccpd_nropaq","ccpd_noraci"
 		  DatosEmbalaje(ls_columna,data)
		  IF ii_valor=2 THEN
			  This.SetItem(Row, ls_Columna, Dec(ls_Nula))
			  Setnull(ii_valor)
			RETURN 1
   	  END IF	
			  
	CASE "ccpd_pesone"			
		
		IF Dec(Data) > 20 THEN
			messagebox("Error","Valor de Peso Neto (kg.) NO Pueder ser Mayor a 20", StopSign!, Ok!)
			This.SetItem(Row, ls_Columna, Dec(ls_Nula))
			RETURN 1
		END IF
		
//		li_ValoRetorno=ValidaPeso(ls_columna,data)
//		IF li_ValoRetorno	=	0 THEN
//			This.SetItem(Row, ls_Columna, Dec(ls_Nula))
//			RETURN 1
//		END IF	
		
	CASE "ccpd_temper"
		IF Dec(Data) > 99.99 OR Dec(Data) < 0 THEN
			This.SetItem(Row, ls_Columna, Dec(ls_Nula))
			RETURN 1
		END IF	
		
	CASE "ccpd_resemb"
		IF DatosResolucion(ls_columna,data) = False THEN
			  This.SetItem(il_fila, ls_Columna, Long(ls_nula))
			  RETURN 1
		ELSE
			IF data<>'M' THEN
				dw_2.Object.ccpd_cobjem.protect = 1
				dw_2.setcolumn("ccpd_paleti")
				This.SetItem(il_fila, "ccpd_cobjem", Long(ls_nula))
				dw_2.setfocus()
			ELSE
				dw_2.Object.ccpd_cobjem.protect = 0
				dw_2.setcolumn("ccpd_cobjem")
				dw_2.setfocus()
			END IF
			ls_resolucioncaja = DatosResolucionCaja(ls_columna,data)
			IF ls_resolucioncaja = 'A' THEN
				tab_1.Tabpage_4.dw_resolucion.Object.ccpd_rescaj.protect = 0
				tab_1.Tabpage_4.dw_resolucion.SetItem(il_fila, "ccpd_rescaj", 'A')	
				tab_1.Tabpage_4.dw_resolucion.SetItem(il_fila, "ccpd_rescom", 'A')
				tab_1.Tabpage_4.dw_resolucion.Object.ccda_secuen.protect = 1             				  
				tab_1.Tabpage_4.dw_resolucion.Object.ccda_secuen.BackGround.Color	=	RGB(192,192,192)
			
			ELSEIF ls_resolucioncaja = 'O' THEN
				tab_1.Tabpage_4.dw_resolucion.Object.ccpd_rescaj.protect = 1
				tab_1.Tabpage_4.dw_resolucion.SetItem(il_fila, "ccpd_rescaj", 'O')
				tab_1.Tabpage_4.dw_resolucion.SetItem(il_fila, "ccpd_rescom", 'O')
				tab_1.Tabpage_4.dw_resolucion.Object.ccpd_rescom.protect = 0
				tab_1.Tabpage_4.dw_resolucion.Object.ccda_secuen.protect = 0             
				tab_1.Tabpage_4.dw_resolucion.Object.ccda_secuen.BackGround.Color	=	RGB(255,255,255)
			ELSE 
				tab_1.Tabpage_4.dw_resolucion.Object.ccpd_rescaj.protect = 0  
				tab_1.Tabpage_4.dw_resolucion.SetItem(il_fila, "ccpd_rescaj", 'A')
				tab_1.Tabpage_4.dw_resolucion.Object.ccda_secuen.protect = 1             				  
				tab_1.Tabpage_4.dw_resolucion.Object.ccda_secuen.BackGround.Color	=	RGB(192,192,192)				
			END IF
			
		END IF
		
		CASE "ccpd_cobjem"
			IF NOT existe_causal(Integer(data)) OR valida_causal(ls_columna,Integer(Data)) = FALSE THEN
				dw_2.Setitem(row, "ccpd_cobjem",integer(ls_Nula))				
				RETURN 1
			END IF 
			
		CASE  "ccpd_muest1","ccpd_muest2","ccpd_muest3","ccpd_muest4","ccpd_muest5", &
				"ccpd_muest6","ccpd_muest7","ccpd_muest8","ccpd_muest9","ccpd_mues10",&
				"ccpd_mues11","ccpd_mues12","ccpd_mues13","ccpd_mues14","ccpd_mues15", &
				"ccpd_mues16","ccpd_mues17","ccpd_mues18","ccpd_mues19","ccpd_mues20"
				
				
				IF Dec(Data) > 20 THEN
					messagebox("Error","Valor de "+" "+ls_Columna+" "+"NO Pueder ser Mayor a 20", StopSign!, Ok!)
					This.SetItem(Row, ls_Columna, Dec(ls_Nula))
					RETURN 1
				END IF
				
//				IF Dec(Data) > 20.99 OR Dec(Data) < 0 THEN
//					This.SetItem(Row, ls_Columna, Dec(ls_Nula))
//					RETURN 1
//				END IF
			
END CHOOSE
end event

event losefocus;call super::losefocus;Tab_1.TabPage_1.dw_Embalaje.accepttext()
Tab_1.TabPage_2.dw_Calidad.accepttext()
Tab_1.TabPage_3.dw_condicion.accepttext()
Tab_1.TabPage_4.dw_Resolucion.accepttext()

end event

event itemerror;RETURN 1
end event

event dwnkey;call super::dwnkey;IF KeyDown(KeyDownArrow!) OR KeyDown(KeyUpArrow!) THEN
		RETURN 1
ELSE
		RETURN 0
END IF
end event

event itemfocuschanged;call super::itemfocuschanged;ib_datos_ok = true
if rowcount() < 1 or getrow() = 0 or ib_borrar then 
	ib_datos_ok = false
else
	SetRow(il_fila)
	ScrolltoRow(il_fila)
end if
end event

type tabpage_2 from userobject within tab_1
integer x = 18
integer y = 112
integer width = 3319
integer height = 984
long backcolor = 16711680
string text = "Calidad"
long tabtextcolor = 33554432
long tabbackcolor = 12632256
long picturemaskcolor = 536870912
dw_calidad dw_calidad
end type

on tabpage_2.create
this.dw_calidad=create dw_calidad
this.Control[]={this.dw_calidad}
end on

on tabpage_2.destroy
destroy(this.dw_calidad)
end on

type dw_calidad from uo_dw within tabpage_2
integer x = 370
integer y = 48
integer width = 2619
integer height = 872
integer taborder = 11
string dataobject = "dw_mant_ctlcalplanillauvas_calidad"
boolean vscrollbar = false
end type

event itemchanged;String	ls_Columna, ls_Nula, ls_resolucioncaja
Integer	li_Nula

SetNull(ls_Nula)
SetNull(li_Nula)

ls_Columna	=	dwo.Name

CHOOSE CASE ls_Columna			
	CASE "ccpd_cobay1","ccpd_cobay2","ccpd_cobay3","ccpd_cobay4"
 		IF Not CienporCiento(ls_columna,data)	THEN 
			dw_1.SetItem(il_Fila,ls_Columna,li_Nula)
			RETURN 1
		END IF
			
	CASE "ccpd_tamayo","ccpd_tamba1","ccpd_tamba2","ccpd_tamba3","ccpd_tamba4","ccpd_tameno"
 		IF Not  CienxCiento(ls_columna,data)	THEN
			dw_1.SetItem(il_Fila,ls_Columna,li_Nula)
			RETURN 1
		END IF

	CASE "ccpd_promba"
 		  Pesoproba(ls_columna,data)
		  IF Dec(Data) > 99.9 OR Dec(Data) < 0  OR ii_valor=6 THEN			
			This.SetItem(Row, ls_Columna, Dec(ls_Nula))	
			setnull(ii_valor)
			RETURN 1
  		  END IF
			
	CASE "ccpd_forade"
		
		IF Not Sumaracimo(1, data) THEN 
			Tab_1.TabPage_2.dw_calidad.Setitem(il_fila, "ccpd_forade",li_nula)			
			RETURN 1 
		END IF 
		
		IF Dec(Data) > 999.9 OR Dec(Data) < 0 THEN
			This.SetItem(Row, ls_Columna, Dec(ls_Nula))			
			RETURN 1	
		END IF
		
	CASE "ccpd_foraap"	
		IF Not Sumaracimo(2, data) THEN 
			Tab_1.TabPage_2.dw_calidad.Setitem(il_fila, "ccpd_foraap",li_nula)			
			RETURN 1 
		END IF 
		
		IF Dec(Data) > 999.9 OR Dec(Data) < 0 THEN
			This.SetItem(Row, ls_Columna, Dec(ls_Nula))			
			RETURN 1	
		END IF

	CASE "ccpd_pesras"
		IF Not Sumpesoracim(1, data) THEN 
			Tab_1.TabPage_2.dw_calidad.Setitem(il_fila, "ccpd_pesras",li_nula)			
			RETURN 1 
		END IF 
		
		IF Dec(Data) > 999.9 OR Dec(Data) < 0 THEN
			This.SetItem(Row, ls_Columna, Dec(ls_Nula))			
			RETURN 1	
		END IF
		
	CASE "ccpd_pesrab"	
		IF Not Sumpesoracim(2, data) THEN 
			Tab_1.TabPage_2.dw_calidad.Setitem(il_fila, "ccpd_pesrab",li_nula)			
			RETURN 1 
		END IF 					
		
		IF Dec(Data) > 999.9 OR Dec(Data) < 0 THEN
			This.SetItem(Row, ls_Columna, Dec(ls_Nula))			
			RETURN 1	
		END IF
		
	CASE	"ccpd_residu"
		
		IF Integer(Data)	>	100	THEN
			MessageBox("Atención","Residuos no puede ser mayor a 100")
			dw_Calidad.SetITem(il_Fila,"ccpd_residu",li_Nula)
			dw_Calidad.SetColumn("ccpd_residu")
			RETURN 1
		END IF
		
		IF Dec(Data) > 999.9 OR Dec(Data) < 0 THEN
			This.SetItem(Row, ls_Columna, Dec(ls_Nula))
			RETURN 1	
		END IF
		
	CASE "ccpd_mancha"
		
		IF	Integer(Data)	>	100	THEN
			MessageBox("Atención","Manchas en la Piel no puede ser Mayor a 100")
			dw_Calidad.SetItem(il_Fila,ls_Columna,li_Nula)
			dw_Calidad.SetColumn("ccpd_mancha")
			RETURN 1
		END IF
		
		IF Dec(Data) > 999.9 OR Dec(Data) < 0 THEN
			This.SetItem(Row, ls_Columna, Dec(ls_Nula))			
			RETURN 1	
		END IF
		
//	CASE "ccpd_insect"
		
//		IF	Not ExisteDañoInsecto(Data)	THEN
//			dw_Calidad.SetItem(il_Fila,ls_Columna,li_Nula)			
//			dw_Calidad.SetColumn("ccpd_insect")
//			RETURN 1
//		END IF
		
	CASE "ccpd_matext"
		
		IF	Not Existe_Matextraña(Data)	THEN
			dw_Calidad.SetItem(il_Fila,ls_Columna,li_Nula)			
			dw_Calidad.SetColumn("ccpd_matext")
			RETURN 1
		END IF		
		
	CASE "ccpd_rescal"
		IF DatosResolucion(ls_columna,data) = False THEN
			  This.SetItem(il_fila, ls_Columna, Long(ls_nula))
			  RETURN 1
		ELSE
			IF data<>'M' THEN
				dw_3.Object.ccpd_cobjca.protect = 1
				dw_3.setcolumn("ccpd_grdobr")
				This.SetItem(il_fila, "ccpd_cobjca", Long(ls_nula))
				dw_3.setfocus()
			ELSE
				dw_3.Object.ccpd_cobjca.protect = 0
				dw_3.setcolumn("ccpd_cobjca")
				dw_3.setfocus()
			END IF
			
			ls_resolucioncaja = DatosResolucionCaja(ls_columna,data)
			
			IF ls_resolucioncaja = 'A' THEN
				tab_1.Tabpage_4.dw_resolucion.Object.ccpd_rescaj.protect = 0
				tab_1.Tabpage_4.dw_resolucion.SetItem(il_fila, "ccpd_rescaj", 'A')	
				tab_1.Tabpage_4.dw_resolucion.SetItem(il_fila, "ccpd_rescom", 'A')
				tab_1.Tabpage_4.dw_resolucion.Object.ccda_secuen.protect = 1             				  
				tab_1.Tabpage_4.dw_resolucion.Object.ccda_secuen.BackGround.Color	=	RGB(192,192,192)
			
			ELSEIF ls_resolucioncaja = 'O' THEN
				tab_1.Tabpage_4.dw_resolucion.Object.ccpd_rescaj.protect = 1
				tab_1.Tabpage_4.dw_resolucion.SetItem(il_fila, "ccpd_rescaj", 'O')
				tab_1.Tabpage_4.dw_resolucion.SetItem(il_fila, "ccpd_rescom", 'O')
				tab_1.Tabpage_4.dw_resolucion.Object.ccpd_rescom.protect = 0
				tab_1.Tabpage_4.dw_resolucion.Object.ccda_secuen.protect = 0             
				tab_1.Tabpage_4.dw_resolucion.Object.ccda_secuen.BackGround.Color	=	RGB(255,255,255)
			ELSE 
				tab_1.Tabpage_4.dw_resolucion.Object.ccpd_rescaj.protect = 0  
				tab_1.Tabpage_4.dw_resolucion.SetItem(il_fila, "ccpd_rescaj", 'A')
				tab_1.Tabpage_4.dw_resolucion.Object.ccda_secuen.protect = 1             				  
				tab_1.Tabpage_4.dw_resolucion.Object.ccda_secuen.BackGround.Color	=	RGB(192,192,192)
			END IF
		END IF
		
	CASE "ccpd_grdobr"
		IF Dec(Data) > 99.99 OR Dec(Data) < 0 THEN
			This.SetItem(Row, ls_Columna, Dec(ls_Nula))
			RETURN 1
		END IF

	CASE "ccpd_bricri"
		IF Dec(Data) > 99.99 OR Dec(Data) < 0 THEN
			This.SetItem(Row, ls_Columna, Dec(ls_Nula))
			RETURN 1
		END IF

	CASE "ccpd_cobjca"
		IF NOT existe_causal(Integer(data)) OR valida_causal(ls_columna,Integer(Data)) = FALSE THEN
			dw_2.Setitem(row, "ccpd_cobjca",integer(ls_Nula))				
			RETURN 1 
		END IF 
		
	CASE "ccpd_caotro"
		
		IF	Dec(Data)	>	100	THEN
			MessageBox("Atención","Otros , no puede ser Mayor a 100")
			This.SetItem(il_Fila,ls_Columna,li_Nula)
			This.SetColumn("ccpd_caotro")
			RETURN 1
		END IF
END CHOOSE

end event

event itemerror;RETURN 1 
end event

event losefocus;call super::losefocus;Tab_1.TabPage_1.dw_Embalaje.accepttext()
Tab_1.TabPage_2.dw_Calidad.accepttext()
Tab_1.TabPage_3.dw_condicion.accepttext()
Tab_1.TabPage_4.dw_Resolucion.accepttext()

end event

event dwnkey;call super::dwnkey;IF KeyDown(KeyDownArrow!) OR KeyDown(KeyUpArrow!) THEN
		RETURN 1
ELSE
		RETURN 0
END IF
end event

event itemfocuschanged;call super::itemfocuschanged;ib_datos_ok = true
if rowcount() < 1 or getrow() = 0 or ib_borrar then 
	ib_datos_ok = false
else
	SetRow(il_fila)
	ScrolltoRow(il_fila)
end if
end event

type tabpage_3 from userobject within tab_1
integer x = 18
integer y = 112
integer width = 3319
integer height = 984
long backcolor = 16711680
string text = "Condición"
long tabtextcolor = 33554432
long tabbackcolor = 12632256
long picturemaskcolor = 536870912
dw_condicion dw_condicion
end type

on tabpage_3.create
this.dw_condicion=create dw_condicion
this.Control[]={this.dw_condicion}
end on

on tabpage_3.destroy
destroy(this.dw_condicion)
end on

type dw_condicion from uo_dw within tabpage_3
integer x = 430
integer y = 52
integer width = 2496
integer height = 880
integer taborder = 11
string dataobject = "dw_mant_ctlcalplanillauvas_condicion"
boolean vscrollbar = false
end type

event itemchanged;Integer	li_Nula, li_punuba, li_pununi, li_punura
String	ls_Nula, ls_Columna, ls_resolucioncaja

SetNull(li_Nula)
SetNull(ls_Nula)

ls_Columna	=	dwo.name

CHOOSE CASE ls_Columna
		
	CASE "ccpd_deslev"
		IF Not Sumadeshi(1, data) THEN 
			Tab_1.TabPage_3.dw_condicion.Setitem(il_fila, "ccpd_deslev",li_nula)			
			RETURN 1 
		END IF 
		
		IF Dec(Data) > 999.9 OR Dec(Data) < 0 THEN
			This.SetItem(Row, ls_Columna, Dec(ls_Nula))
			RETURN 1
		END IF
		
	CASE "ccpd_desmod"	
		IF Not Sumadeshi(2, data) THEN 
			Tab_1.TabPage_3.dw_condicion.Setitem(il_fila, "ccpd_desmod",li_nula)			
			RETURN 1 
		END IF 
		
		IF Dec(Data) > 999.9 OR Dec(Data) < 0 THEN
			This.SetItem(Row, ls_Columna, Dec(ls_Nula))
			RETURN 1
		END IF
		
	CASE "ccpd_desalt"
		IF Not Sumadeshi(3, data) THEN 
			Tab_1.TabPage_3.dw_condicion.Setitem(il_fila, "ccpd_desalt",li_nula)			
			RETURN 1 
		END IF
		
		IF Dec(Data) > 999.9 OR Dec(Data) < 0 THEN
			This.SetItem(Row, ls_Columna, Dec(ls_Nula))
			RETURN 1
		END IF
	
	CASE "ccpd_punuba"
		ii_punuba	=	Integer(Data)		
		

	CASE "ccpd_pununi"
		ii_pununi	=	Integer(Data)

	CASE "ccpd_punura"	
		ii_punura	=	Integer(Data)			
		
	CASE "ccpd_coblan"			
		IF Dec(Data) > 999.9 OR Dec(Data) < 0 THEN
			This.SetItem(Row, ls_Columna, Dec(ls_Nula))
			RETURN 1
		END IF
		
	CASE "ccpd_cocris"
		IF Dec(Data) > 999.9 OR Dec(Data) < 0 THEN
			This.SetItem(Row, ls_Columna, Dec(ls_Nula))			
			RETURN 1
		END IF
		
	CASE "ccpd_codesp"
		IF Dec(Data) > 999.9 OR Dec(Data) < 0 THEN
			This.SetItem(Row, ls_Columna, Dec(ls_Nula))			
			RETURN 1
		END IF
		
	CASE "ccpd_conso2"
		IF Dec(Data) > 999.9 OR Dec(Data) < 0 THEN
			This.SetItem(Row, ls_Columna, Dec(ls_Nula))			
			RETURN 1
		END IF
		
	CASE "ccpd_cobapa"
		IF Dec(Data) > 999.9 OR Dec(Data) < 0 THEN
			This.SetItem(Row, ls_Columna, Dec(ls_Nula))			
			RETURN 1
		END IF
		
	CASE "ccpd_codesg"
		IF Dec(Data) > 999.9 OR Dec(Data) < 0 THEN
			This.SetItem(Row, ls_Columna, Dec(ls_Nula))			
			RETURN 1
		END IF
		
	CASE 	"ccpd_baacuo"
		IF Dec(Data) > 999.9 OR Dec(Data) < 0 THEN
			This.SetItem(Row, ls_Columna, Dec(ls_Nula))			
			RETURN 1		
		END IF
		
	CASE "ccpd_rescon"
		IF DatosResolucion(ls_columna,data) = False THEN
			  This.SetItem(il_fila, ls_Columna, Long(ls_nula))
			  RETURN 1
		ELSE
			IF data<>'M' THEN
				dw_4.Object.ccpd_cobjco.protect = 1
				dw_4.setcolumn("ccpd_deslev")
				This.SetItem(il_fila, "ccpd_cobjco", Integer(ls_nula))
				dw_4.setfocus()
			ELSE
				dw_4.Object.ccpd_cobjco.protect = 0
				dw_4.setcolumn("ccpd_cobjco")
				dw_4.setfocus()
			END IF
			ls_resolucioncaja = DatosResolucionCaja(ls_columna,data)
			IF ls_resolucioncaja = 'A' THEN
				tab_1.Tabpage_4.dw_resolucion.Object.ccpd_rescaj.protect = 0
				tab_1.Tabpage_4.dw_resolucion.SetItem(il_fila, "ccpd_rescaj", 'A')	
				tab_1.Tabpage_4.dw_resolucion.SetItem(il_fila, "ccpd_rescom", 'A')
				tab_1.Tabpage_4.dw_resolucion.Object.ccda_secuen.protect = 1             				  
				tab_1.Tabpage_4.dw_resolucion.Object.ccda_secuen.BackGround.Color	=	RGB(192,192,192)
			
			ELSEIF ls_resolucioncaja = 'O' THEN
				tab_1.Tabpage_4.dw_resolucion.Object.ccpd_rescaj.protect = 1
				tab_1.Tabpage_4.dw_resolucion.SetItem(il_fila, "ccpd_rescaj", 'O')
				tab_1.Tabpage_4.dw_resolucion.SetItem(il_fila, "ccpd_rescom", 'O')
				tab_1.Tabpage_4.dw_resolucion.Object.ccpd_rescom.protect = 0
				tab_1.Tabpage_4.dw_resolucion.Object.ccda_secuen.protect = 0             
				tab_1.Tabpage_4.dw_resolucion.Object.ccda_secuen.BackGround.Color	=	RGB(255,255,255)
			ELSE 
				tab_1.Tabpage_4.dw_resolucion.Object.ccpd_rescaj.protect = 0  
				tab_1.Tabpage_4.dw_resolucion.SetItem(il_fila, "ccpd_rescaj", 'A')
				tab_1.Tabpage_4.dw_resolucion.Object.ccda_secuen.protect = 1             				  
				tab_1.Tabpage_4.dw_resolucion.Object.ccda_secuen.BackGround.Color	=	RGB(192,192,192)
			END IF
		END IF
		
	CASE "ccpd_cobjco"
		IF NOT existe_causal(Integer(data)) OR valida_causal(ls_columna,Integer(Data)) = FALSE THEN
			dw_2.Setitem(row, "ccpd_cobjco",integer(ls_Nula))				
			RETURN 1 
		END IF 
		
END CHOOSE




end event

event itemerror;RETURN 1
end event

event itemfocuschanged;String	ls_Nula
Decimal	ldc_Valor	

IF IsValid(w_main) THEN	
	w_main.SetMicroHelp(This.Tag)
END IF

SetNull(ls_Nula)

CHOOSE CASE is_Columna
	CASE "ccpd_punura"
		IF dwo.Name <> "ccpd_punura" THEN 
			IF Not IsNull(dw_4.Object.ccpd_punuba[il_fila]) OR &
				Not IsNull(dw_4.Object.ccpd_pununi[il_fila])	THEN
				IF  IsNull(dw_4.Object.ccpd_punura[il_fila]) THEN 
						Messagebox("Atención","Debe ingresar Número de Racimo",StopSign!)							
						SetColumn("ccpd_punura")						
						RETURN 1
						is_Columna	=	"" 
				ELSE
					is_Columna = dwo.name
				END IF 
			END IF
		END IF 
	CASE ELSE
		is_Columna	=	dwo.Name


END CHOOSE

ib_datos_ok = true
if rowcount() < 1 or getrow() = 0 or ib_borrar then 
	ib_datos_ok = false
else
	SetRow(il_fila)
	ScrolltoRow(il_fila)
end if
end event

event losefocus;Tab_1.TabPage_1.dw_Embalaje.accepttext()
Tab_1.TabPage_2.dw_Calidad.accepttext()
Tab_1.TabPage_3.dw_condicion.accepttext()
Tab_1.TabPage_4.dw_Resolucion.accepttext()



end event

event rowfocuschanged;call super::rowfocuschanged;//ib_datos_ok = true
//if rowcount() < 1 or getrow() = 0 or ib_borrar then 
//	ib_datos_ok = false
//else
//	SetRow(il_fila)
//	ScrolltoRow(il_fila)
//end if
end event

event dwnkey;call super::dwnkey;IF KeyDown(KeyDownArrow!) OR KeyDown(KeyUpArrow!) THEN
		RETURN 1
ELSE
		RETURN 0
END IF
end event

type tabpage_4 from userobject within tab_1
integer x = 18
integer y = 112
integer width = 3319
integer height = 984
long backcolor = 16711680
string text = "Resolución"
long tabtextcolor = 33554432
long tabbackcolor = 12632256
long picturemaskcolor = 536870912
dw_resolucion dw_resolucion
end type

on tabpage_4.create
this.dw_resolucion=create dw_resolucion
this.Control[]={this.dw_resolucion}
end on

on tabpage_4.destroy
destroy(this.dw_resolucion)
end on

type dw_resolucion from uo_dw within tabpage_4
integer x = 398
integer y = 116
integer width = 2501
integer height = 760
integer taborder = 11
string dataobject = "dw_mant_ctlcalplanillauvas_resolucion"
boolean vscrollbar = false
end type

event itemchanged;String	ls_Columna, ls_Nula, ls_resolucioncaja
Integer	li_Nula

SetNull(ls_Nula)
SetNull(li_Nula)

Tab_1.TabPage_4.dw_resolucion.AcceptText()

ls_Columna	=	dwo.Name


CHOOSE CASE ls_Columna			
//			
////	CASE "ccpd_resemb","ccpd_rescal","ccpd_rescon"
////		This.SetItem(il_fila, "ccda_secuen", Integer(ls_Nula))
////		IF DatosResolucion(ls_columna,data) = False THEN
////			  This.SetItem(il_fila, ls_Columna, Long(ls_nula))
////			  RETURN 1
////		ELSE
////			  ls_resolucioncaja = DatosResolucionCaja(ls_columna,data)
////			  IF ls_resolucioncaja = 'A' THEN
////				  This.Object.ccpd_rescaj.protect = 0
////				  This.SetItem(il_fila, "ccpd_rescaj", 'A')				  
////				  This.Object.ccda_secuen.protect = 1             				  
////			     This.Object.ccda_secuen.BackGround.Color	=	RGB(192,192,192)
////
////			  ELSEIF ls_resolucioncaja = 'O' THEN
////				         This.Object.ccpd_rescaj.protect = 1
////				  			This.SetItem(il_fila, "ccpd_rescaj", 'O')
////				  			This.Object.ccda_secuen.protect = 0             
////				  			This.Object.ccda_secuen.BackGround.Color	=	RGB(255,255,255)
////					ELSE 
////						  This.Object.ccpd_rescaj.protect = 0  
////						  This.SetItem(il_fila, "ccpd_rescaj", 'A')
////						  This.Object.ccda_secuen.protect = 1             				  
////						  This.Object.ccda_secuen.BackGround.Color	=	RGB(192,192,192)
////				  END IF
////			END IF
//		


  CASE "ccpd_rescaj"
		IF data = 'A' THEN	
			This.SetItem(il_fila, "ccpd_rescom", 'A')
			This.SetItem(il_fila, "ccpd_respal", 'A')		
			This.Object.ccpd_rescom.protect = 1
			This.Object.ccpd_respal.protect = 1
			This.SetItem(il_fila, "ccpd_cobjem", li_Nula)
			This.SetItem(il_fila, "ccpd_cobjca", li_Nula)
			This.SetItem(il_fila, "ccpd_cobjco", li_Nula)	
			This.SetItem(il_fila, "ccpd_porob1", li_Nula)
			This.SetItem(il_fila, "ccpd_porob2", li_Nula)
			This.SetItem(il_fila, "ccpd_porob3", li_Nula)	
			This.SetItem(il_fila, "ccpd_causal1", li_Nula)
			This.SetItem(il_fila, "ccpd_causal2", li_Nula)
			This.SetItem(il_fila, "ccpd_causal3", li_Nula)	
			This.SetItem(il_fila, "ccpd_pocau1", li_Nula)
			This.SetItem(il_fila, "ccpd_pocau2", li_Nula)
			This.SetItem(il_fila, "ccpd_pocau3", li_Nula)	

		ELSEIF data = 'O' THEN	
			This.SetItem(il_fila, "ccpd_rescom", 'O')
			This.SetItem(il_fila, "ccpd_respal", 'R')
			This.SetItem(il_fila, "ccpd_causal1", li_Nula)
			This.SetItem(il_fila, "ccpd_causal2", li_Nula)
			This.SetItem(il_fila, "ccpd_causal3", li_Nula)		
			This.Object.ccpd_rescom.protect = 0
			This.Object.ccpd_respal.protect = 0
//			This.SetItem(il_fila, "ccpd_rescom", 'A')	
		END IF		

  CASE "ccpd_rescom"
		IF data = 'A' THEN	
			This.SetItem(il_fila, "ccpd_respal", 'A')
			This.Object.ccpd_respal.protect = 1
			This.SetItem(il_fila, "ccpd_causal1", li_Nula)
			This.SetItem(il_fila, "ccpd_causal2", li_Nula)
			This.SetItem(il_fila, "ccpd_causal3", li_Nula)	
			This.SetItem(il_fila, "ccpd_pocau1", li_Nula)
			This.SetItem(il_fila, "ccpd_pocau2", li_Nula)
			This.SetItem(il_fila, "ccpd_pocau3", li_Nula)	
			
		ELSEIF data = 'O' THEN	
			This.SetItem(il_fila, "ccpd_respal", 'R')
			This.SetItem(il_fila, "ccpd_causal1", li_Nula)
			This.SetItem(il_fila, "ccpd_causal2", li_Nula)
			This.SetItem(il_fila, "ccpd_causal3", li_Nula)	
			This.Object.ccpd_respal.protect = 0
		END IF		

//  CASE "ccpd_cobjem"
//		IF NOT existe_causal(Integer(data)) OR valida_causal(ls_columna,Integer(Data)) = FALSE THEN
//			dw_2.Setitem(row, "ccpd_cobjem",integer(ls_Nula))				
//			RETURN 1
//		END IF 
			
  CASE "ccpd_respal"
		IF data = 'R' OR data = 'P' THEN	
			This.SetItem(il_fila, "ccpd_causal1", li_Nula)
			This.SetItem(il_fila, "ccpd_causal2", li_Nula)
			This.SetItem(il_fila, "ccpd_causal3", li_Nula)
		END IF		
		
	Case	"ccpd_causal1"	
		If NOT Causales(Integer(data)) Then
			This.SetItem(il_fila, "ccpd_causal1", li_Nula)
			RETURN 1
		End If		
		
	Case	"ccpd_causal2"	
		If NOT Causales(Integer(data)) Then
			This.SetItem(il_fila, "ccpd_causal2", li_Nula)
			RETURN 1
		End If		
		
	Case	"ccpd_causal3"	
		If NOT Causales(Integer(data)) Then
			This.SetItem(il_fila, "ccpd_causal3", li_Nula)
			RETURN 1			
		End If				

	Case	"ccpd_pocau1"
			If Integer(data) > 100.00 Then
				MessageBox('Atencion', 'Valor no puede ser mayor a 100') 
				This.SetItem(il_fila, "ccpd_pocau1" , li_Nula)
				RETURN 1 
			End If		
		
	Case	"ccpd_pocau2"		
			If Integer(data) > 100.00 Then
				MessageBox('Atencion', 'Valor no puede ser mayor a 100') 
				This.SetItem(il_fila, "ccpd_pocau2", li_Nula)
				RETURN 1 
			End If
	
	Case	"ccpd_pocau3"
		If Integer(data) > 100.00 Then
			MessageBox('Atencion', 'Valor no puede ser mayor a 100') 
			This.SetItem(il_fila, "ccpd_pocau3", li_Nula)
			RETURN 1 
		End If	
		
	Case	"ccpd_cobjca"	
		IF Causales2(Integer(data)) Then
			This.SetItem(il_fila, "ccpd_cobjca", li_Nula)
			RETURN 1
		END IF
		
	Case	"ccpd_cobjco"	
		IF Causales2(Integer(data)) Then
			This.SetItem(il_fila, "ccpd_cobjco", li_Nula)
			RETURN 1
		END IF
		
	Case	"ccpd_cobjem"	
		IF Causales2(Integer(data)) Then
			This.SetItem(il_fila, "ccpd_cobjem", li_Nula)
			RETURN 1			
		END IF					
END CHOOSE


end event

event losefocus;call super::losefocus;Tab_1.TabPage_1.dw_Embalaje.accepttext()
Tab_1.TabPage_2.dw_Calidad.accepttext()
Tab_1.TabPage_3.dw_condicion.accepttext()
Tab_1.TabPage_4.dw_Resolucion.accepttext()



end event

event rowfocuschanged;call super::rowfocuschanged;//ib_datos_ok = true
//if rowcount() < 1 or getrow() = 0 or ib_borrar then 
//	ib_datos_ok = false
//else
//	SetRow(il_fila)
//	ScrolltoRow(il_fila)
//end if
end event

event itemerror;RETURN 1
end event

event dwnkey;call super::dwnkey;IF KeyDown(KeyDownArrow!) OR KeyDown(KeyUpArrow!) THEN
		RETURN 1
ELSE
		RETURN 0
END IF
end event

event itemfocuschanged;call super::itemfocuschanged;ib_datos_ok = true
if rowcount() < 1 or getrow() = 0 or ib_borrar then 
	ib_datos_ok = false
else
	SetRow(il_fila)
	ScrolltoRow(il_fila)
end if
end event

