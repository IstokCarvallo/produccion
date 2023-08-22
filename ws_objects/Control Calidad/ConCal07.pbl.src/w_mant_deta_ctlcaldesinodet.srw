$PBExportHeader$w_mant_deta_ctlcaldesinodet.srw
forward
global type w_mant_deta_ctlcaldesinodet from w_mant_detalle_csd
end type
type tab_1 from tab within w_mant_deta_ctlcaldesinodet
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
type tab_1 from tab within w_mant_deta_ctlcaldesinodet
tabpage_1 tabpage_1
tabpage_2 tabpage_2
tabpage_3 tabpage_3
tabpage_4 tabpage_4
end type
end forward

global type w_mant_deta_ctlcaldesinodet from w_mant_detalle_csd
integer width = 3168
integer height = 2044
tab_1 tab_1
end type
global w_mant_deta_ctlcaldesinodet w_mant_deta_ctlcaldesinodet

type variables
Integer	ii_Punuba, ii_Punura, ii_Pununi
String	is_Columna


DataWindowChild idwc_clientes,idwc_plantas, idwc_productor, idwc_especie, idwc_variedad, idwc_etiqueta
DataWindow	dw_2, dw_3, dw_4, dw_5

uo_ctlcaldanoespecie		iuo_ctlcaldanoespecie
end variables

forward prototypes
public function string datosresolucioncaja (string columna, string valor)
public subroutine datosembalaje (string columna, string valor)
public function boolean datosresolucion (string columna, string valor)
public function boolean duplicado (string campo, integer tipo)
public function boolean existedañoinsecto (string data)
public function boolean existenvaloresracimo (integer ai_punura, integer ai_punuba, integer ai_pununi)
public subroutine pesoproba (string columna, string valor)
public subroutine promediobayas (string columna, string valor)
public function boolean sumadeshi (integer ai_tipo, string as_valor)
public subroutine sumaformaracimo (string as_columna, string valor, integer numero)
public function boolean sumaracimo (integer ai_tipo, string as_valor)
public function boolean sumpesoracim (integer ai_tipo, string as_valor)
public function boolean valiracimo (string columna, string valor)
public function boolean cienxciento (string columna, string valor)
public function boolean cienporciento (string columna, string valor)
public function boolean causalcajalote (integer ai_causal)
end prototypes

public function string datosresolucioncaja (string columna, string valor);String	ls_v1, ls_v2, ls_v3, ls_v4, ls_juntos

ls_v1		=	dw_5.Object.ccdd_resemb[il_fila]
ls_v2		=	dw_5.Object.ccdd_rescal[il_fila]
ls_v3		=	dw_5.Object.ccdd_rescon[il_fila]

CHOOSE CASE Columna

	CASE "ccdd_resemb"
		ls_v1	=	valor

	CASE "ccdd_rescal"
		ls_v2	=	valor

	CASE "ccdd_rescon"
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

public subroutine datosembalaje (string columna, string valor);Integer	li_v3
String	ls_v1, ls_v2

ls_v1				=	dw_2.Object.ccdd_paleti[il_fila]
ls_v2				=	dw_2.Object.ccdd_aparie[il_fila]
li_v3				=	dw_2.Object.ccdd_nropaq[il_fila] 
//li_v4				=	dw_3.Object.ccdd_nrorac[il_fila]

CHOOSE CASE Columna

	CASE "ccdd_paleti"
		ls_v1	=	valor
		IF ls_v1 = '' THEN
			MessageBox("Error de Consistencia", "Debe Ingresar Paletizaje.", StopSign!, Ok!)					
			dw_2.SetFocus()
		END IF	

	CASE "ccdd_aparie"
		ls_v2 =	valor
		IF ls_v2 = '' THEN
			MessageBox("Error de Consistencia", "Debe Ingresar Apariencia.", StopSign!, Ok!)
			dw_2.SetFocus()
		END IF
		
	CASE "ccdd_nropaq"
		li_v3 =	Integer(valor)
		IF li_v3 = 0 THEN
			MessageBox("Error de Consistencia", "Debe Ingresar Número Paquetes.", StopSign!, Ok!)
			dw_2.SetFocus()
		END IF
		
	CASE "ccpd_nrorac"
		li_v3 =	Integer(valor)
		IF li_v3 = 0 THEN
			MessageBox("Error de Consistencia", "Debe Ingresar Nro de Racimos", StopSign!, Ok!)
			dw_2.SetFocus()
		END IF	
		
END CHOOSE

end subroutine

public function boolean datosresolucion (string columna, string valor);String	ls_v1, ls_v2, ls_v3, ls_v4

ls_v1		=	dw_5.Object.ccdd_resemb[il_fila]
ls_v2		=	dw_5.Object.ccdd_rescal[il_fila]
ls_v3		=	dw_5.Object.ccdd_rescon[il_fila]

CHOOSE CASE Columna

	CASE "ccdd_resemb"
		ls_v1	=	valor
		IF ls_v1 = '' THEN
			MessageBox("Error de Consistencia", "Debe Ingresar Resolución Embalaje.", StopSign!, Ok!)
			dw_5.SetFocus()
			RETURN False
		END IF

	CASE "ccdd_rescal"
		ls_v2	=	valor
		IF ls_v2 = '' THEN
			MessageBox("Error de Consistencia", "Debe Ingresar Resolución Calidad.", StopSign!, Ok!)
			dw_5.SetFocus()
			RETURN False
		END IF

	CASE "ccdd_rescon"
		ls_v3	=	valor
		IF ls_v3 = '' THEN
			MessageBox("Error de Consistencia", "Debe Ingresar Resolución Condición.", StopSign!, Ok!)
			dw_5.SetFocus()
			RETURN False			
		END IF
		
END CHOOSE

RETURN True
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

public function boolean existedañoinsecto (string data);Integer	li_Daño	, li_Existe

li_Daño	=	Integer(Data)

SELECT Count(*)
INTO	 :li_Existe
FROM	 dba.ctlcaldanoespecie
WHERE	 espe_codigo = 11
AND    ccfa_codigo = 40
AND    ccsf_codigo = 15
AND    ccgr_codigo = 12
AND    ccda_secuen = :li_Daño	;

IF sqlca.sqlcode	=	-1	THEN
	F_ErrorBaseDatos(sqlca,"No se pudo leer la Tabla de Daños por Especie")
ELSEIF	li_Existe	>	0	THEN
	RETURN TRUE
ELSE
	MessageBox("atención","Daño digitado no corresponde a Insectos")
END IF

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

public subroutine pesoproba (string columna, string valor);Dec{1}	ld_promba

//ld_promba	=	dw_3.Object.ccpd_promba[il_fila]

CHOOSE CASE Columna
		
	CASE "ccpd_promba"
		ld_promba 		=	Dec(valor)
		IF ld_promba	= 0 THEN
			MessageBox("Error de Consistencia", "Debe Ingresar Peso Promedio.", StopSign!, Ok!)
			dw_3.SetFocus()
		END IF	

END CHOOSE

end subroutine

public subroutine promediobayas (string columna, string valor);//Dec{2}	li_valor1
//String	ls_Null
//
//SetNull(ls_Null)
//
//li_valor1		=	dw_3.Object.ccpd_promba[il_fila]
//
//CHOOSE CASE Columna
//
//	CASE "ccpd_promba"
//		li_valor1	=	Dec(valor)
//
//END CHOOSE
//
//
//IF li_valor1 = 0 THEN	
//	MessageBox("Error de Consistencia", "Debe Ingresar Peso Promedio de Bayas.", StopSign!, Ok!)
//	dw_3.SetFocus()
//END IF
//
//
end subroutine

public function boolean sumadeshi (integer ai_tipo, string as_valor);Integer 	li_ValLev, li_ValMod, li_ValAlt, li_Suma

li_ValLev	=	Tab_1.TabPage_3.dw_Condicion.Object.ccdd_deslev[il_fila]
li_ValMod	=	Tab_1.TabPage_3.dw_Condicion.Object.ccdd_desmod[il_fila]
li_ValAlt	=	Tab_1.TabPage_3.dw_Condicion.Object.ccdd_desalt[il_fila]

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

public subroutine sumaformaracimo (string as_columna, string valor, integer numero);Integer	li_Valor1, li_Valor2, li_Suma

IF Numero	=	1	THEN		
	
	CHOOSE CASE	as_Columna
			
		CASE "ccdd_forade"
			li_Valor1	=	Integer(Valor)
			li_Valor2		=	Tab_1.TabPage_2.dw_Calidad.Object.ccdd_foraap[il_Fila]
			
		CASE "ccdd_foraap"
			li_Valor1		=	Tab_1.TabPage_2.dw_Calidad.Object.ccdd_forade[il_Fila]
			li_Valor2	=	Integer(Valor)		
			
	END CHOOSE
	
ELSEIF Numero	=	2	THEN
	
	CHOOSE CASE as_Columna
			
		CASE "ccdd_pesras"
			li_Valor1	=	Integer(Valor)
			li_Valor2	=	Tab_1.TabPage_2.dw_Calidad.Object.ccdd_pesrab[il_Fila]
			
		CASE "ccdd_pesrab"
			li_Valor1	=	Tab_1.TabPage_2.dw_Calidad.Object.ccdd_pesras[il_Fila]
			li_Valor2	=	Integer(Valor)
			
	END CHOOSE	
	
END IF

li_Suma	=	li_Valor1	+	li_Valor2

IF li_Suma	>	100 THEN
	
	MessageBox("Atención","Sumatoria de Forma de Racimo~r" +&
				  " no debe exceder 100 ")						
END IF						
end subroutine

public function boolean sumaracimo (integer ai_tipo, string as_valor);
Integer 	li_deforme, li_apretado,li_Suma

li_deforme	=	Tab_1.TabPage_2.dw_Calidad.Object.ccdd_forade[il_fila]
li_apretado	=	Tab_1.TabPage_2.dw_Calidad.Object.ccdd_foraap[il_fila]


CHOOSE CASE ai_tipo
		
	CASE 1
		li_deforme	=	Integer(as_valor)
		
	CASE 2
		li_apretado	=	Integer(as_valor)	
		
END CHOOSE

IF IsNull(li_Deforme)  THEN li_Deforme  = 0
IF IsNull(li_Apretado) THEN li_Apretado = 0 

li_Suma	=	li_Deforme + li_Apretado 

IF li_Suma > 100 THEN	
	MessageBox("Atención","La Sumatoria de la Forma de Racimo  ~r" +&
							"no debe ser mayor a 100")
	RETURN False
END IF					

RETURN True
end function

public function boolean sumpesoracim (integer ai_tipo, string as_valor);Integer 	li_sobre, li_bajo, li_Suma

li_Sobre		=	Tab_1.TabPage_2.dw_Calidad.Object.ccdd_pesras[il_fila]
li_Bajo		=	Tab_1.TabPage_2.dw_Calidad.Object.ccdd_pesrab[il_fila]

CHOOSE CASE ai_tipo
		
	CASE 1
		li_Sobre	=	Integer(as_valor)
		
	CASE 2
		li_Bajo	=	Integer(as_valor)	
		
END CHOOSE

IF IsNull(li_Sobre)	THEN li_Sobre  = 0
IF IsNull(li_bajo)	THEN li_Bajo	= 0 

li_Suma	=	li_Sobre + li_Bajo

IF li_Suma > 100 THEN	
	MessageBox("Atención","La Sumatoria del Peso de Racimo~r" +&
							"no debe ser mayor a 100")
	RETURN False
END IF					

RETURN True
end function

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

public function boolean cienxciento (string columna, string valor);Integer	li_valor1, li_valor2, li_valor3, li_valor4, li_valor5, li_suma
String	ls_Null

SetNull(ls_Null)

CHOOSE CASE Columna

	CASE "ccdd_tamba1"
		li_valor1	=	Integer(valor)
		li_Suma		=	li_Valor1
		IF li_suma > 100 THEN			
			MessageBox("Error de Consistencia", "El valor no debe exceder el 100%", StopSign!, Ok!)
			dw_3.SetFocus()
			RETURN FALSE
		END IF

		
	CASE "ccdd_tamba2"
		li_Valor1	=	Tab_1.TabPage_2.dw_Calidad.Object.ccdd_tamba1[il_fila]
		li_valor2	=	Integer(valor)
		li_Suma		=	li_Valor1	+	li_Valor2
		IF li_suma > 100 THEN			
			MessageBox("Error de Consistencia", "El valor no debe exceder el 100%", StopSign!, Ok!)
			dw_3.SetFocus()
			RETURN FALSE
		END IF


	CASE "ccdd_tamba3"
		li_Valor1	=	Tab_1.TabPage_2.dw_Calidad.Object.ccdd_tamba1[il_fila]
		li_Valor2	=	Tab_1.TabPage_2.dw_Calidad.Object.ccdd_tamba2[il_fila]
		li_valor3	=	Integer(valor)
		li_Suma		=	li_Valor1	+	li_Valor2	+	li_Valor3
		IF li_suma > 100 THEN			
			MessageBox("Error de Consistencia", "El valor no debe exceder el 100%", StopSign!, Ok!)
			dw_3.SetFocus()
			RETURN FALSE
		END IF

	CASE "ccdd_tamba4"
		li_Valor1	=	Tab_1.TabPage_2.dw_Calidad.Object.ccdd_tamba1[il_fila]
		li_Valor2	=	Tab_1.TabPage_2.dw_Calidad.Object.ccdd_tamba2[il_fila]
		li_Valor3	=	Tab_1.TabPage_2.dw_calidad.Object.ccdd_tamba3[il_fila]
		li_valor4 	=	Integer(valor)
		li_Suma		=	li_Valor1	+	li_Valor2	+	li_Valor3	+	li_Valor4
		IF li_suma > 100 THEN			
			MessageBox("Error de Consistencia", "El valor no debe exceder el 100%", StopSign!, Ok!)
			dw_3.SetFocus()
			RETURN FALSE
		END IF

	CASE 	"ccdd_tamba5"
		li_Valor1	=	Tab_1.TabPage_2.dw_Calidad.Object.ccdd_tamba1[il_fila]
		li_Valor2	=	Tab_1.TabPage_2.dw_Calidad.Object.ccdd_tamba2[il_fila]
		li_Valor3	=	Tab_1.TabPage_2.dw_Calidad.Object.ccdd_tamba3[il_fila]
		li_Valor4	=	Tab_1.TabPage_2.dw_Calidad.Object.ccdd_tamba4[il_fila]
		li_valor5 	=	Integer(valor)
		li_Suma		=	li_Valor1 +	li_Valor2 +	li_Valor3 +	li_Valor4 +	li_Valor5
		IF li_suma > 100 OR li_Suma <	100	THEN			
			MessageBox("Error de Consistencia", "La Suma de Tamaño Bayas debe dar siempre 100.", StopSign!, Ok!)
			dw_3.SetFocus()
			RETURN FALSE
		END IF

END CHOOSE

RETURN TRUE

end function

public function boolean cienporciento (string columna, string valor);Integer	li_valor1, li_valor2, li_valor3, li_valor4, li_suma
String	ls_Null
//Dec{2}

SetNull(ls_Null)


CHOOSE CASE Columna

	CASE "ccdd_cobay1"
		li_valor1	=	Integer(valor)
		li_Suma		=	li_Valor1	
		IF li_suma > 100	THEN
				MessageBox("Error de Consistencia", "Valor no debe exceder el 100%", StopSign!, Ok!)
				dw_3.SetFocus()
				RETURN FALSE
		END IF
		

	CASE "ccdd_cobay2"
		li_Valor1	=	Tab_1.TabPage_2.dw_Calidad.Object.ccdd_cobay1[il_Fila]
		li_valor2	=	Integer(valor)
		
		li_Suma		=	li_Valor1	+	li_Valor2
		IF  li_suma > 100	THEN
				MessageBox("Error de Consistencia", "Valor no debe exceder el 100%", StopSign!, Ok!)
				dw_3.SetFocus()
				RETURN FALSE
		END IF
		
	
	CASE "ccdd_cobay3"
		li_Valor1	=	Tab_1.TabPage_2.dw_calidad.Object.ccdd_cobay1[il_fila]
		li_Valor2	=	Tab_1.TabPAge_2.dw_calidad.Object.ccdd_cobay2[il_fila]
		li_valor3	=	Integer(valor)
		li_Suma		=	li_Valor1	+	li_Valor2	+	li_Valor3
		IF  li_suma > 100	THEN
				MessageBox("Error de Consistencia", "Valor no debe exceder el 100%", StopSign!, Ok!)
				dw_3.SetFocus()
				RETURN FALSE
		END IF
		
		
	CASE "ccdd_cobay4"
		li_Valor1	=	Tab_1.TabPage_2.dw_calidad.Object.ccdd_cobay1[il_fila]
		li_Valor2	=	Tab_1.TabPage_2.dw_calidad.Object.ccdd_cobay2[il_fila]
		li_Valor3	=	Tab_1.TabPage_2.dw_calidad.Object.ccdd_cobay3[il_fila]
		li_valor4 	=	Integer(valor)
		li_Suma		=	li_Valor1	+	li_Valor2	+	li_Valor3	+	li_Valor4
		IF  li_suma > 100 OR	li_Suma	<	100	THEN
				MessageBox("Error de Consistencia", "La Suma de Color Bayas debe dar siempre 100.", StopSign!, Ok!)
				dw_3.SetFocus()
				RETURN FALSE
		END IF
		
END CHOOSE

RETURN TRUE



end function

public function boolean causalcajalote (integer ai_causal);Integer	li_Causal, li_Existe 

li_Causal	=	ai_causal

SELECT Count(*)
INTO	 :li_Existe 
FROM	 dba.ctlcaldanoespecie
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

on w_mant_deta_ctlcaldesinodet.create
int iCurrent
call super::create
this.tab_1=create tab_1
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.tab_1
end on

on w_mant_deta_ctlcaldesinodet.destroy
call super::destroy
destroy(this.tab_1)
end on

event ue_recuperadatos;call super::ue_recuperadatos;String	ls_Usuario
Integer	li_Grupo, li_cliente
ls_Usuario	=	Upper(Gstr_Us.Nombre)

ias_campo[54]	=	dw_1.Object.ccdd_rescaj[il_fila]	
IF ias_campo[54]	<>	'' Or Not IsNull(ias_campo[54]) THEN
	li_Grupo = BuscaGrupo(ls_Usuario)
	IF ((li_Grupo =	6) OR (li_grupo = 1)) OR istr_mant.Argumento[22] =	'0' OR dw_1.GetItemStatus(il_Fila, 0, Primary!) = NewModified!  THEN 									
		ias_campo[7]	=	dw_1.Object.ccdd_paleti[il_fila]
		ias_campo[12]	=	dw_1.Object.ccdd_aparie[il_fila]
		ias_campo[13]	=	String(dw_1.Object.ccdd_nropaq[il_fila])
		ias_campo[14]	=	String(dw_1.Object.ccdd_nrorac[il_fila])		
		ias_campo[17]	=	String(dw_1.Object.ccdd_cobay1[il_fila])
		ias_campo[18]	=	String(dw_1.Object.ccdd_cobay2[il_fila])
		ias_campo[19]	=	String(dw_1.Object.ccdd_cobay3[il_fila])		
		ias_campo[20]	=	String(dw_1.Object.ccdd_cobay4[il_fila])		
		ias_campo[21]	=	String(dw_1.Object.ccdd_tamba1[il_fila])
		ias_campo[22]	=	String(dw_1.Object.ccdd_tamba2[il_fila])
		ias_campo[23]	=	String(dw_1.Object.ccdd_tamba3[il_fila])
		ias_campo[24]	=	String(dw_1.Object.ccdd_tamba4[il_fila])
		ias_campo[25]	=	String(dw_1.Object.ccdd_tamba5[il_fila])
		ias_campo[27]	=	String(dw_1.Object.ccdd_forade[il_fila])
		ias_campo[28]	=	String(dw_1.Object.ccdd_foraap[il_fila])
		ias_campo[29]	=	String(dw_1.Object.ccdd_pesras[il_fila])
		ias_campo[30]	=	String(dw_1.Object.ccdd_pesrab[il_fila])		
		ias_campo[31]	=	String(dw_1.Object.ccdd_recidu[il_fila])
		ias_campo[32]	=	String(dw_1.Object.ccdd_mancha[il_fila])
		ias_campo[34]	=	String(dw_1.Object.ccdd_deslev[il_fila])
		ias_campo[35]	=	String(dw_1.Object.ccdd_desmod[il_fila])
		ias_campo[36]	=	String(dw_1.Object.ccdd_desalt[il_fila])
		ias_campo[37]	=	String(dw_1.Object.ccdd_punuba[il_fila])
		ias_campo[38]	=	String(dw_1.Object.ccdd_pununi[il_fila])
		ias_campo[39]	=	String(dw_1.Object.ccdd_punura[il_fila])
		ias_campo[40]	=	String(dw_1.Object.ccdd_pacida[il_fila])
		ias_campo[41]	=	String(dw_1.Object.ccdd_opnbay[il_fila])
		ias_campo[42]	=	String(dw_1.Object.ccdd_coblan[il_fila])
		ias_campo[43]	=	String(dw_1.Object.ccdd_cocris[il_fila])
		ias_campo[44]	=	String(dw_1.Object.ccdd_codesp[il_fila])
		ias_campo[45]	=	String(dw_1.Object.ccdd_conso2[il_fila])
		ias_campo[46]	=	String(dw_1.Object.ccdd_cobapa[il_fila])
		ias_campo[47]	=	String(dw_1.Object.ccdd_codesg[il_fila])
		ias_campo[48]	=	String(dw_1.Object.ccdd_baacuo[il_fila])
		ias_campo[49]	=	String(dw_1.Object.ccdd_coprem[il_fila])
		ias_campo[51]	=	dw_1.Object.ccdd_resemb[il_fila]
		ias_campo[52]	=	dw_1.Object.ccdd_rescal[il_fila]
		ias_campo[53]	=	dw_1.Object.ccdd_rescon[il_fila]
		ias_campo[54]	=	dw_1.Object.ccdd_rescaj[il_fila]
		ias_campo[55]	=	String(dw_1.Object.espe_codigo[il_fila])
		ias_campo[56]	=	String(dw_1.Object.ccdd_secuen[il_fila])			
		ias_campo[59]	=	String(dw_1.Object.ccdd_caucajl[il_fila])				
		Pb_Acepta.Enabled		=	True 
		Pb_Cancela.Enabled	=	True 		
		Pb_Salir.Enabled		=	True 
	ELSE
		istr_mant.Solo_consulta						=	True 
		Tab_1.TabPage_1.dw_Embalaje.Enabled		=	False	
		Tab_1.TabPage_2.dw_Calidad.Enabled		=	False
		Tab_1.TabPage_3.dw_Condicion.Enabled	=	False			
		Tab_1.TabPage_4.dw_resolucion.Enabled	=	False		
		Pb_Acepta.Enabled								=	False
		Pb_Cancela.Enabled							=	False
		pb_Salir.Enabled								=	True
	END IF 
END IF

IF istr_mant.Agrega THEN
	dw_1.SetItem(il_fila, "clie_codigo",gi_codexport)
	dw_1.SetItem(il_fila, "prod_codigo",Long(istr_mant.argumento[14]))
	dw_1.SetItem(il_fila, "espe_codigo",gi_codespecie)
	dw_1.GetChild("vari_codigo", idwc_variedad)	
	idwc_variedad.SetTransObject(sqlca)
	
	IF idwc_variedad.Retrieve(gi_codespecie) = 0 THEN 
		idwc_variedad.InsertRow(0)
	END IF 	
	
	dw_1.SetItem(il_fila, "vari_codigo",Integer(istr_mant.argumento[11]))
	dw_1.SetItem(il_fila, "etiq_codigo",Integer(istr_mant.argumento[16]))	
	dw_1.SetItem(il_fila, "nave_tipotr",Integer(istr_mant.argumento[15]))	
END IF 

dw_2.SetRow(il_fila)
dw_2.ScrolltoRow(il_fila)
dw_3.SetRow(il_fila)
dw_3.ScrolltoRow(il_fila)
dw_4.SetRow(il_fila)
dw_4.ScrolltoRow(il_fila)
dw_5.SetRow(il_fila)
dw_5.ScrolltoRow(il_fila)		
end event

event ue_deshace();call super::ue_deshace;//IF UpperBound(ias_campo) > 0 THEN
//	dw_1.SetItem(il_fila, "clie_codigo", Long(ias_campo[1]))
//	dw_1.SetItem(il_fila, "plde_codigo", Integer(ias_campo[2]))
//	dw_1.SetItem(il_fila, "clie_codigo", Integer(ias_campo[3]))
//	dw_1.SetItem(il_fila, "cclo_numero", Integer(ias_campo[4]))
//	dw_1.SetItem(il_fila, "ccpd_secuen", Integer(ias_campo[5]))
//	dw_1.SetItem(il_fila, "ccpe_noguia", Integer(ias_campo[6]))
//	dw_1.SetItem(il_fila, "ccpd_paleti", ias_campo[7])
//	dw_1.SetItem(il_fila, "ccpd_rotula", ias_campo[8])
//	dw_1.SetItem(il_fila, "ccpd_materi", ias_campo[9])
//	dw_1.SetItem(il_fila, "ccpd_empaq",  ias_campo[10])
//	dw_1.SetItem(il_fila, "ccpd_pesone", Integer(ias_campo[11]))
//	dw_1.SetItem(il_fila, "ccpd_aparie", ias_campo[12])
//	dw_1.SetItem(il_fila, "ccpd_nropaq", Integer(ias_campo[13]))
//	dw_1.SetItem(il_fila, "ccpd_noraci", Integer(ias_campo[14]))
//	dw_1.SetItem(il_fila, "ccpd_grdobr", Integer(ias_campo[15]))
//	dw_1.SetItem(il_fila, "ccpd_temper", Integer(ias_campo[16]))
//	dw_1.SetItem(il_fila, "ccpd_cobay1", Integer(ias_campo[17]))
//	dw_1.SetItem(il_fila, "ccpd_cobay2", Integer(ias_campo[18]))
//	dw_1.SetItem(il_fila, "ccpd_cobay3", Integer(ias_campo[19]))
//	dw_1.SetItem(il_fila, "ccpd_cobay4", Integer(ias_campo[20]))
//	dw_1.SetItem(il_fila, "ccpd_tamayo", Integer(ias_campo[21]))
//	dw_1.SetItem(il_fila, "ccpd_tamba1", Integer(ias_campo[22]))
//	dw_1.SetItem(il_fila, "ccpd_tamba2", Integer(ias_campo[23]))
//	dw_1.SetItem(il_fila, "ccpd_tamba3", Integer(ias_campo[24]))
//	dw_1.SetItem(il_fila, "ccpd_tamba4", Integer(ias_campo[25]))
//	dw_1.SetItem(il_fila, "ccpd_promba", Integer(ias_campo[26]))
//	dw_1.SetItem(il_fila, "ccpd_forade", Integer(ias_campo[27]))
//	dw_1.SetItem(il_fila, "ccpd_foraap", Integer(ias_campo[28]))
//	dw_1.SetItem(il_fila, "ccpd_pesras", Integer(ias_campo[29]))
//	dw_1.SetItem(il_fila, "ccpd_pesrab", Integer(ias_campo[30]))
//	dw_1.SetItem(il_fila, "ccpd_residu", Integer(ias_campo[31]))
//	dw_1.SetItem(il_fila, "ccpd_mancha", Integer(ias_campo[32]))
//	dw_1.SetItem(il_fila, "ccpd_insect", Integer(ias_campo[33]))
//	dw_1.SetItem(il_fila, "ccpd_deslev", Integer(ias_campo[34]))
//	dw_1.SetItem(il_fila, "ccpd_desmod", Integer(ias_campo[35]))
//	dw_1.SetItem(il_fila, "ccpd_desalt", Integer(ias_campo[36]))
//	dw_1.SetItem(il_fila, "ccpd_punuba", Integer(ias_campo[37]))
//	dw_1.SetItem(il_fila, "ccpd_pununi", Integer(ias_campo[38]))
//	dw_1.SetItem(il_fila, "ccpd_punura", Integer(ias_campo[39]))
//	dw_1.SetItem(il_fila, "ccpd_pacida", Integer(ias_campo[40]))
//	dw_1.SetItem(il_fila, "ccpd_opnbay", Integer(ias_campo[41]))
//	dw_1.SetItem(il_fila, "ccpd_coblan", Integer(ias_campo[42]))
//	dw_1.SetItem(il_fila, "ccpd_cocris", Integer(ias_campo[43]))
//	dw_1.SetItem(il_fila, "ccpd_codesp", Integer(ias_campo[44]))
//	dw_1.SetItem(il_fila, "ccpd_conso2", Integer(ias_campo[45]))
//	dw_1.SetItem(il_fila, "ccpd_cobapa", Integer(ias_campo[46]))
//	dw_1.SetItem(il_fila, "ccpd_codesg", Integer(ias_campo[47]))
//	dw_1.SetItem(il_fila, "ccpd_baacuo", Integer(ias_campo[48]))
//	dw_1.SetItem(il_fila, "ccpd_coprem", Integer(ias_campo[49]))
//	dw_1.SetItem(il_fila, "ccpd_otcond", Integer(ias_campo[50]))
//	dw_1.SetItem(il_fila, "ccpd_resemb", ias_campo[51])
//	dw_1.SetItem(il_fila, "ccpd_rescal", ias_campo[52])
//	dw_1.SetItem(il_fila, "ccpd_rescon", ias_campo[53])
//	dw_1.SetItem(il_fila, "ccpd_rescaj", ias_campo[54])
//	dw_1.SetItem(il_fila, "espe_codigo", Integer(ias_campo[55]))
//	dw_1.SetItem(il_fila, "ccda_secuen", Integer(ias_campo[56]))	
//	dw_1.SetItem(il_fila, "ccpd_selecc", ias_campo[57])	
//	dw_1.SetItem(il_fila, "ccpd_embala", ias_campo[58])
//END IF

//integer li_null
//setnull(li_null)
//
//	dw_1.SetItem(il_fila, "clie_codigo", li_null)
//	dw_1.SetItem(il_fila, "nave_tipotr", string (li_null))	
//	dw_1.SetItem(il_fila, "nave_codigo", li_null)	
//	dw_1.SetItem(il_fila, "espe_codigo", li_null)	
//	dw_1.SetItem(il_fila, "vari_codigo", li_null)	
//	dw_1.SetItem(il_fila, "etiq_codigo", li_null)	
//	dw_1.SetItem(il_fila, "prod_codigo", li_null)
//	dw_1.SetItem(il_fila, "vaca_clibr" , string(li_null))
//	dw_1.SetItem(il_fila, "emba_codigo", string(li_null))
//	dw_1.SetItem(il_fila, "ccdd_fecemb", li_null)	
//	dw_1.SetItem(il_fila, "ccdd_secuen", li_null)
//	dw_1.SetItem(il_fila, "ccdd_paleti", string(li_null))
//	dw_1.SetItem(il_fila, "ccdd_aprie" , string(li_null))
//	dw_1.SetItem(il_fila, "ccdd_nropaq", li_null)
//	dw_1.SetItem(il_fila, "ccdd_nrorac", li_null)	
//	dw_1.SetItem(il_fila, "ccdd_cobay1", li_null)
//	dw_1.SetItem(il_fila, "ccdd_cobay2", li_null)
//	dw_1.SetItem(il_fila, "ccdd_cobay3", li_null)
//	dw_1.SetItem(il_fila, "ccdd_cobay4", li_null)
//	dw_1.SetItem(il_fila, "ccdd_tamba1", li_null)
//	dw_1.SetItem(il_fila, "ccdd_tamba2", li_null)
//	dw_1.SetItem(il_fila, "ccdd_tamba3", li_null)
//	dw_1.SetItem(il_fila, "ccdd_tamba4", li_null)	
//	dw_1.SetItem(il_fila, "ccdd_tamba5", li_null)	
//	dw_1.SetItem(il_fila, "ccdd_forade", li_null)	
//	dw_1.SetItem(il_fila, "ccdd_foraap", li_null)	
//	dw_1.SetItem(il_fila, "ccdd_pesras", li_null)	
//	dw_1.SetItem(il_fila, "ccdd_recidu", li_null)	
//	dw_1.SetItem(il_fila, "ccdd_mancha", li_null)	
//	dw_1.SetItem(il_fila, "ccdd_desmod", li_null)	
//	dw_1.SetItem(il_fila, "ccdd_desalt", li_null)	
//	dw_1.SetItem(il_fila, "ccdd_deslev", li_null)	
//	dw_1.SetItem(il_fila, "ccdd_punuba", li_null)	
//	dw_1.SetItem(il_fila, "ccdd_pununi", li_null)	
//	dw_1.SetItem(il_fila, "ccdd_punura", li_null)	
//	dw_1.SetItem(il_fila, "ccdd_pacida", li_null)	
//	dw_1.SetItem(il_fila, "ccdd_opnbay", li_null)	
//	dw_1.SetItem(il_fila, "ccdd_coblan", li_null)
//	dw_1.SetItem(il_fila, "ccdd_cocris", li_null)
//	dw_1.SetItem(il_fila, "ccdd_codesp", li_null)
//	dw_1.SetItem(il_fila, "ccdd_conso2", li_null)
//	dw_1.SetItem(il_fila, "espe_cobapa", li_null)
//	dw_1.SetItem(il_fila, "ccdd_codesg", li_null)	
//	dw_1.setItem(il_fila, "ccdd_pardea", li_null)
//	dw_1.setItem(il_fila, "ccdd_dafrio", li_null)
//	dw_1.setItem(il_fila, "ccdd_baacuo", li_null)
//	dw_1.setItem(il_fila, "ccdd_coprem", li_null)
//	dw_1.setItem(il_fila, "ccdd_conden", li_null)
//	dw_1.setItem(il_fila, "ccdd_resemb", li_null)
//	dw_1.setItem(il_fila, "ccdd_rescal", li_null)
//	dw_1.setItem(il_fila, "ccdd_rescon", li_null)
//	dw_1.setItem(il_fila, "ccdd_rescaj", li_null)
//	dw_1.setItem(il_fila, "ccdd_caucajl", li_null)
//
//
end event

event ue_antesguardar;Integer	li_cont, li_suma, li_Nula, li_punuba, li_pununi, li_punura
String	ls_mensaje, ls_colu[]

SetNull(li_Nula)
li_suma  = 0 

li_punuba	=	dw_4.Object.ccdd_punuba[il_Fila]
li_pununi	=	dw_4.Object.ccdd_pununi[il_Fila]
li_punura	=	dw_4.Object.ccdd_punura[il_Fila]


IF IsNull(dw_3.object.ccdd_cobay1[il_fila]) = FALSE THEN
	li_suma = li_suma + (dw_3.object.ccdd_cobay1[il_fila])
END IF		

IF IsNull(dw_3.object.ccdd_cobay2[il_fila]) = FALSE THEN
	li_suma = li_suma + (dw_3.object.ccdd_cobay2[il_fila])
END IF	

IF IsNull(dw_3.object.ccdd_cobay3[il_fila]) = FALSE THEN
	li_suma = li_suma + (dw_3.object.ccdd_cobay3[il_fila])
END IF	

IF IsNull(dw_3.object.ccdd_cobay4[il_fila]) = FALSE THEN
	li_suma = li_suma + (dw_3.object.ccdd_cobay4[il_fila])
END IF	

IF li_suma <> 100 THEN
	MessageBox("Error de Consistencia", "La Suma de Color Bayas debe dar siempre 100.", StopSign!, Ok!)
	dw_3.SetFocus()
	Message.DoubleParm = -1	
END IF

li_suma  = 0 

IF IsNull(dw_3.object.ccdd_tamba1[il_fila]) = FALSE THEN
	li_suma = li_suma + (dw_3.object.ccdd_tamba1[il_fila])	
END IF	

IF IsNull(dw_3.object.ccdd_tamba2[il_fila]) = FALSE THEN
	li_suma = li_suma + (dw_3.object.ccdd_tamba2[il_fila])	
END IF	

IF IsNull(dw_3.object.ccdd_tamba3[il_fila]) = FALSE THEN
	li_suma = li_suma + (dw_3.object.ccdd_tamba3[il_fila])	
END IF

IF IsNull(dw_3.object.ccdd_tamba4[il_fila]) = FALSE THEN
	li_suma = li_suma + (dw_3.object.ccdd_tamba4[il_fila])	
END IF

IF IsNull(dw_3.object.ccdd_tamba5[il_fila]) = FALSE THEN
	li_suma = li_suma + (dw_3.object.ccdd_tamba5[il_fila])	
END IF

IF li_suma <> 100 THEN
	MessageBox("Error de Consistencia", "La Suma de Tamaño Bayas debe dar siempre 100.", StopSign!, Ok!)
	dw_3.SetFocus()
	Message.DoubleParm = -1
END IF

IF Tab_1.TabPage_2.dw_Calidad.Object.ccdd_forade[il_Fila]	>	100 THEN
	MessageBox("Atención","La Forma del Racimo debe Sumar hasta 100")	
	dw_3.SetFocus()
	Message.DoubleParm = -1
END IF

IF Tab_1.TabPage_2.dw_Calidad.Object.ccdd_foraap[il_Fila]	>	100 THEN	
	MessageBox("Atención","El Peso del Racimo debe Suma hasta 100")
	dw_3.SetFocus()
	Message.DoubleParm = -1	
END IF

IF li_punura = 0 AND (li_punuba > 0 OR li_pununi > 0) THEN
	li_cont ++
	ls_mensaje		 	= ls_mensaje + "~rNº de Racimos"
	ls_colu[li_cont] 	= "ccdd_punura"
END IF

IF Isnull(dw_2.object.ccdd_paleti[il_fila]) or dw_2.object.ccdd_paleti[il_fila] = '' THEN
	li_cont ++
	ls_mensaje		 	= ls_mensaje + "~rPaletizaje"
	ls_colu[li_cont] 	= "ccdd_paleti"
END IF

IF Isnull(dw_2.object.ccdd_aparie[il_fila]) or dw_2.object.ccdd_aparie[il_fila] = '' THEN
	li_cont ++
	ls_mensaje		 	= ls_mensaje + "~rApariencia"
	ls_colu[li_cont] 	= "ccdd_aparie"
END IF

IF Isnull(dw_2.object.ccdd_nropaq[il_fila]) or dw_2.object.ccdd_nropaq[il_fila] = 0 THEN
	li_cont ++
	ls_mensaje		 	= ls_mensaje + "~rNúmero Paquetes"
	ls_colu[li_cont] 	= "ccdd_nropaq"
END IF

IF Isnull(dw_2.object.ccdd_nrorac[il_fila]) or dw_2.object.ccdd_nrorac[il_fila] = 0 THEN
	li_cont ++
	ls_mensaje		 	= ls_mensaje + "~rNúmero de Racimo"
	ls_colu[li_cont] 	= "ccdd_noraci"
END IF

IF Isnull(dw_5.object.ccdd_resemb[il_fila]) or dw_5.object.ccdd_resemb[il_fila] = '' THEN
	li_cont ++
	ls_mensaje		 	= ls_mensaje + "~rResolución Embalaje"
	ls_colu[li_cont] 	= "ccdd_resemb"
END IF

IF Isnull(dw_5.object.ccdd_rescal[il_fila]) or dw_5.object.ccdd_rescal[il_fila] = '' THEN
	li_cont ++
	ls_mensaje		 	= ls_mensaje + "~rResolución Calidad"
	ls_colu[li_cont] 	= "ccdd_rescal"
END IF

IF Isnull(dw_5.object.ccdd_rescon[il_fila]) or dw_5.object.ccdd_rescon[il_fila] = '' THEN
	li_cont ++
	ls_mensaje		 	= ls_mensaje + "~rResolución Condición"
	ls_colu[li_cont] 	= "ccdd_rescon"
END IF

IF Isnull(dw_5.object.ccdd_rescaj[il_fila]) or dw_5.object.ccdd_rescaj[il_fila] = '' THEN
	li_cont ++
	ls_mensaje		 	= ls_mensaje + "~rResolución Caja"
	ls_colu[li_cont] 	= "ccdd_rescaj"
END IF

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
dw_1.SetItem(il_Fila,"prod_codigo",Long(istr_mant.Argumento[14]))
dw_1.SetItem(il_Fila,"espe_codigo",Integer(istr_mant.Argumento[21]))
dw_1.SetItem(il_Fila,"vari_codigo",Integer(istr_mant.Argumento[11]))
dw_1.SetItem(il_Fila,"clie_codigo",gi_codexport)
dw_1.SetItem(il_Fila,"etiq_codigo",Integer(istr_mant.Argumento[16]))

dw_2.SetRow(il_fila)
dw_3.SetRow(il_fila)
dw_4.SetRow(il_fila)
dw_5.SetRow(il_fila)

dw_2.ScrollToRow(il_fila)
dw_3.ScrollToRow(il_fila)
dw_4.ScrollToRow(il_fila)
dw_5.ScrollToRow(il_fila)

dw_2.SetItem(il_Fila,"ccdd_paleti",ls_Nula)

dw_5.SetItem(il_Fila,"ccdd_resemb",ls_Nula)
dw_5.SetItem(il_Fila,"ccdd_rescal",ls_Nula)
dw_5.SetItem(il_Fila,"ccdd_rescon",ls_Nula)
dw_5.SetItem(il_Fila,"ccdd_rescaj",ls_Nula)
end event

event open;Long		ll_trans, ll_planilla, ll_guia, ll_productor
Integer	li_planta, li_cliente, li_zona, li_especie, li_variedad, li_etiqueta

This.Icon	=	Gstr_apl.Icono

Tab_1.TabPage_1.dw_Embalaje.InsertRow(0)
Tab_1.TabPage_2.dw_Calidad.InsertRow(0)
Tab_1.TabPage_3.dw_Condicion.InsertRow(0)
Tab_1.TabPage_4.dw_Resolucion.InsertRow(0)

Tab_1.TabPage_1.dw_Embalaje.Enabled		=	True 
Tab_1.TabPage_2.dw_Calidad.Enabled		=	True
Tab_1.TabPage_3.dw_Condicion.Enabled	=	True
Tab_1.TabPage_4.dw_Resolucion.Enabled	=	True

Postevent("ue_recuperadatos")

istr_mant = Message.PowerObjectParm

SetPointer(HourGlass!)

ll_productor =	Long(istr_mant.argumento[14])
li_especie	 =	Integer(istr_mant.argumento[21])
li_variedad	 =	Integer(istr_mant.argumento[11])
li_cliente	 =	gi_codexport
li_etiqueta  =	Long(istr_mant.argumento[16])

//cliente//
dw_1.GetChild("clie_codigo", idwc_clientes)
idwc_clientes.SetTransObject(sqlca)
idwc_clientes.Retrieve()

//productor//
dw_1.GetChild("prod_codigo", idwc_productor)
idwc_productor.SetTransObject(sqlca)
idwc_productor.Retrieve(1)

//especie//
dw_1.GetChild("espe_codigo", idwc_especie)
idwc_especie.SetTransObject(sqlca)
idwc_especie.Retrieve()

//Variedad//
dw_1.GetChild("vari_codigo", idwc_variedad)
idwc_variedad.SetTransObject(sqlca)
IF  idwc_variedad.Retrieve(gi_CodEspecie) =  0 THEN 
	 idwc_variedad.InsertRow(0)
END IF 	 
idwc_variedad.SetSort("vari_nombre A")
idwc_variedad.Sort()

//Etiqueta//
dw_1.GetChild("etiq_codigo", idwc_etiqueta)
idwc_etiqueta.SetTransObject(sqlca)
IF  idwc_etiqueta.Retrieve(gi_CodEspecie) =  0 THEN 
	 idwc_etiqueta.InsertRow(0)
END IF 	 
idwc_etiqueta.SetSort("etiq_nombre A")
idwc_etiqueta.Sort()

dw_2	=	tab_1.tabpage_1.dw_embalaje
dw_3	=	tab_1.tabpage_2.dw_calidad
dw_4	=	tab_1.tabpage_3.dw_condicion
dw_5	=	tab_1.tabpage_4.dw_resolucion

dw_1.SetTransObject(sqlca)
dw_2.SetTransObject(sqlca)
dw_3.SetTransObject(sqlca)
dw_4.SetTransObject(sqlca)
dw_5.SetTransObject(sqlca)

istr_mant.dw.ShareData(dw_1)

dw_1.ShareData(dw_2)
dw_1.ShareData(dw_3)
dw_1.ShareData(dw_4)
dw_1.ShareData(dw_5)

end event

event resize;//
end event

event ue_guardar();
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


SetPointer(HourGlass!)

Message.DoubleParm = 0

IF dw_1.AcceptText() = -1 THEN RETURN

SetPointer(HourGlass!)

w_main.SetMicroHelp("Grabando información...")

Message.DoubleParm = 0

TriggerEvent("ue_antesguardar")

IF Message.DoubleParm = -1 THEN RETURN 

end event

type pb_ultimo from w_mant_detalle_csd`pb_ultimo within w_mant_deta_ctlcaldesinodet
end type

type pb_siguiente from w_mant_detalle_csd`pb_siguiente within w_mant_deta_ctlcaldesinodet
end type

type pb_anterior from w_mant_detalle_csd`pb_anterior within w_mant_deta_ctlcaldesinodet
end type

type pb_primero from w_mant_detalle_csd`pb_primero within w_mant_deta_ctlcaldesinodet
end type

type pb_cancela from w_mant_detalle_csd`pb_cancela within w_mant_deta_ctlcaldesinodet
integer x = 2816
integer y = 344
integer taborder = 40
end type

type pb_acepta from w_mant_detalle_csd`pb_acepta within w_mant_deta_ctlcaldesinodet
integer x = 2811
integer y = 156
integer taborder = 30
end type

type pb_salir from w_mant_detalle_csd`pb_salir within w_mant_deta_ctlcaldesinodet
integer x = 2811
integer y = 564
integer taborder = 50
end type

type dw_1 from w_mant_detalle_csd`dw_1 within w_mant_deta_ctlcaldesinodet
integer x = 151
integer y = 92
integer width = 2542
integer height = 608
string dataobject = "dw_mant_ctlcaldestinoplanilla"
end type

type tab_1 from tab within w_mant_deta_ctlcaldesinodet
integer x = 151
integer y = 772
integer width = 2949
integer height = 1116
integer taborder = 20
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean italic = true
long backcolor = 12632256
boolean focusonbuttondown = true
boolean boldselectedtext = true
integer selectedtab = 1
tabpage_1 tabpage_1
tabpage_2 tabpage_2
tabpage_3 tabpage_3
tabpage_4 tabpage_4
end type

on tab_1.create
this.tabpage_1=create tabpage_1
this.tabpage_2=create tabpage_2
this.tabpage_3=create tabpage_3
this.tabpage_4=create tabpage_4
this.Control[]={this.tabpage_1,&
this.tabpage_2,&
this.tabpage_3,&
this.tabpage_4}
end on

on tab_1.destroy
destroy(this.tabpage_1)
destroy(this.tabpage_2)
destroy(this.tabpage_3)
destroy(this.tabpage_4)
end on

type tabpage_1 from userobject within tab_1
integer x = 18
integer y = 112
integer width = 2912
integer height = 988
long backcolor = 12632256
string text = "Embalaje"
long tabtextcolor = 33554432
long tabbackcolor = 79741120
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
integer x = 521
integer y = 144
integer width = 1765
integer height = 632
integer taborder = 11
boolean enabled = false
string dataobject = "dw_mant_ctlcalplanilldestino_embalaje"
boolean hscrollbar = true
boolean vscrollbar = false
borderstyle borderstyle = stylelowered!
end type

event itemchanged;String	ls_Columna, ls_Nula

SetNull(ls_Nula)




ls_Columna	=	dwo.Name

CHOOSE CASE ls_Columna			
		
	CASE "ccdd_paleti","ccdd_aparie","ccdd_nropaq","ccdd_nororac"
 		  DatosEmbalaje(ls_columna,data)
						
END CHOOSE

end event

event losefocus;call super::losefocus;Tab_1.TabPage_1.dw_Embalaje.accepttext()
Tab_1.TabPage_2.dw_Calidad.accepttext()
Tab_1.TabPage_3.dw_condicion.accepttext()
Tab_1.TabPage_4.dw_Resolucion.accepttext()

end event

type tabpage_2 from userobject within tab_1
integer x = 18
integer y = 112
integer width = 2912
integer height = 988
long backcolor = 12632256
string text = "Calidad"
long tabtextcolor = 33554432
long tabbackcolor = 79741120
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
integer x = 183
integer y = 40
integer width = 2450
integer height = 920
integer taborder = 11
string dataobject = "dw_mant_ctlcalplanilladestino_calidad"
boolean vscrollbar = false
borderstyle borderstyle = stylelowered!
end type

event itemchanged;String	ls_Columna, ls_Nula
Integer	li_Nula

SetNull(ls_Nula)
SetNull(li_Nula)

ls_Columna	=	dwo.Name

CHOOSE CASE ls_Columna			
	CASE "ccdd_cobay1","ccdd_cobay2","ccdd_cobay3","ccdd_cobay4"
 		IF Not CienporCiento(ls_columna,data)	THEN 
			dw_1.SetItem(il_Fila,ls_Columna,li_Nula)
			
			RETURN 1
		END IF
			
	CASE "ccdd_tamba1","ccdd_tamba2","ccdd_tamba3","ccdd_tamba4","ccdd_tamba5"
 		IF Not  CienxCiento(ls_columna,data)	THEN
			dw_1.SetItem(il_Fila,ls_Columna,li_Nula)
			RETURN 1
		END IF
		
	CASE "ccdd_forade"
		IF Not Sumaracimo(1, data) THEN 
			Tab_1.TabPage_2.dw_calidad.Setitem(il_fila, "ccdd_forade",li_nula)			
			RETURN 1 
		END IF 
		
	CASE "ccdd_foraap"	
		IF Not Sumaracimo(2, data) THEN 
			Tab_1.TabPage_2.dw_calidad.Setitem(il_fila, "ccdd_foraap",li_nula)			
			RETURN 1 
		END IF 

	CASE "ccpdd_pesras"
		IF Not Sumpesoracim(1, data) THEN 
			Tab_1.TabPage_2.dw_calidad.Setitem(il_fila, "ccdd_pesras",li_nula)			
			RETURN 1 
		END IF 
		
	CASE "ccdd_pesrab"	
		IF Not Sumpesoracim(2, data) THEN 
			Tab_1.TabPage_2.dw_calidad.Setitem(il_fila, "ccdd_pesrab",li_nula)			
			RETURN 1 
		END IF 					
		
	CASE	"ccdd_recidu"		
		IF Integer(Data)	>	100	THEN
			MessageBox("Atención","Residuos no puede ser mayor a 100")
			dw_Calidad.SetITem(il_Fila,"ccpd_recidu",li_Nula)
			dw_Calidad.SetColumn("ccdd_recidu")
			RETURN 1
		END IF
		
	CASE "ccdd_mancha"		
		IF	Integer(Data)	>	100	THEN
			MessageBox("Atención","Manchas en la Pien no puede ser Mayor a 100")
			dw_Calidad.SetItem(il_Fila,ls_Columna,li_Nula)
			dw_Calidad.SetColumn("ccdd_mancha")
			RETURN 1
		END IF
		
END CHOOSE

end event

event itemerror;call super::itemerror;RETURN 1 
end event

event losefocus;call super::losefocus;Tab_1.TabPage_1.dw_Embalaje.accepttext()
Tab_1.TabPage_2.dw_Calidad.accepttext()
Tab_1.TabPage_3.dw_condicion.accepttext()
Tab_1.TabPage_4.dw_Resolucion.accepttext()

end event

type tabpage_3 from userobject within tab_1
integer x = 18
integer y = 112
integer width = 2912
integer height = 988
long backcolor = 12632256
string text = "Condición"
long tabtextcolor = 33554432
long tabbackcolor = 79741120
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
integer x = 91
integer y = 48
integer width = 2789
integer height = 880
integer taborder = 11
string dataobject = "dw_mant_ctlcalplanilladestino_condicion"
boolean controlmenu = true
boolean vscrollbar = false
borderstyle borderstyle = stylelowered!
end type

event itemchanged;Integer	li_Nula, li_punuba, li_pununi, li_punura
String	ls_Nula, ls_Columna 

SetNull(li_Nula)
SetNull(ls_Nula)

ls_Columna	=	dwo.name

CHOOSE CASE ls_Columna
		
	CASE "ccdd_deslev"
		IF Not Sumadeshi(1, data) THEN 
			Tab_1.TabPage_3.dw_condicion.Setitem(il_fila, "ccdd_deslev",li_nula)			
			RETURN 1 
		END IF 
		
	CASE "ccdd_desmod"	
		IF Not Sumadeshi(2, data) THEN 
			Tab_1.TabPage_3.dw_condicion.Setitem(il_fila, "ccdd_desmod",li_nula)			
			RETURN 1 
		END IF 
	
	CASE "ccdd_desalt"
		IF Not Sumadeshi(3, data) THEN 
			Tab_1.TabPage_3.dw_condicion.Setitem(il_fila, "ccdd_desalt",li_nula)			
			RETURN 1 
		END IF 			
	
	CASE "ccdd_punuba"
		ii_punuba	=	Integer(Data)		

	CASE "ccdd_pununi"
		ii_pununi	=	Integer(Data)

	CASE "ccdd_punura"	
		ii_punura	=	Integer(Data)			
		
	/*CASE "ccdd_pacida"			
		IF Not ExistenValoresRacimo(ii_Punura,ii_punuba, ii_pununi) THEN
			Tab_1.TabPage_3.dw_condicion.Setitem(il_Fila,"ccdd_punura",li_nula)
			SetColumn("ccdd_punura")
			RETURN 1
		END IF	*/
		
END CHOOSE




end event

event itemerror;call super::itemerror;RETURN 1
end event

event itemfocuschanged;Integer li_Nula 

IF IsValid(w_main) THEN	
	w_main.SetMicroHelp(This.Tag)
END IF

SetNull(li_Nula)

CHOOSE CASE is_Columna
	CASE "ccdd_punura"
		IF dwo.Name <> "ccdd_punura"	THEN 
			IF Not IsNull(dw_4.Object.ccdd_punuba[il_fila]) OR &
				Not IsNull(dw_4.Object.ccdd_pununi[il_fila])	THEN
				IF  IsNull(dw_4.Object.ccdd_punura[il_fila]) THEN 
						Messagebox("Atención","Debe Ingresar Nro de Racimos",StopSign!)
						SetColumn("ccdd_punura")
						RETURN 1
						is_Columna	=	"" 
				ELSE
					is_Columna = dwo.name
				END IF 
			END IF
		END IF 
		
	CASE ELSE
		is_Columna = dwo.name
END CHOOSE
end event

event losefocus;Tab_1.TabPage_1.dw_Embalaje.accepttext()
Tab_1.TabPage_2.dw_Calidad.accepttext()
Tab_1.TabPage_3.dw_condicion.accepttext()
Tab_1.TabPage_4.dw_Resolucion.accepttext()



end event

event rowfocuschanged;call super::rowfocuschanged;ib_datos_ok = true
if rowcount() < 1 or getrow() = 0 or ib_borrar then 
	ib_datos_ok = false
else
	SetRow(il_fila)
	ScrolltoRow(il_fila)
end if
end event

type tabpage_4 from userobject within tab_1
integer x = 18
integer y = 112
integer width = 2912
integer height = 988
long backcolor = 12632256
string text = "Resolución"
long tabtextcolor = 33554432
long tabbackcolor = 79741120
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
integer x = 270
integer y = 124
integer width = 2295
integer height = 672
integer taborder = 11
boolean enabled = false
string dataobject = "dw_mant_ctlcalplanilladetalle_resolucion"
boolean vscrollbar = false
borderstyle borderstyle = stylelowered!
end type

event itemchanged;String	ls_Columna, ls_Nula, ls_resolucioncaja
Integer  li_Nula

SetNull(ls_Nula)

ls_Columna	=	dwo.Name


CHOOSE CASE ls_Columna			
			
	CASE "ccdd_resemb","ccdd_rescal","ccdd_rescon"
		This.SetItem(il_fila, "ccdd_caucajl", Integer(ls_Nula))
		IF DatosResolucion(ls_columna,data) = False THEN
			  This.SetItem(il_fila, ls_Columna, Long(ls_nula))
			  RETURN 1
		ELSE
			  ls_resolucioncaja = DatosResolucionCaja(ls_columna,data)
			  IF ls_resolucioncaja = 'A' THEN
				  This.Object.ccdd_rescaj.protect = 1
				  This.SetItem(il_fila, "ccdd_rescaj", 'A')				  
				  This.Object.ccdd_caucajl.protect = 1             				  
			     This.Object.ccdd_caucajl.BackGround.Color	=	RGB(192,192,192)
				  
			  ELSEIF ls_resolucioncaja = 'O' THEN
				         This.Object.ccdd_rescaj.protect = 1
				  			This.SetItem(il_fila, "ccdd_rescaj", 'O')
				  			This.Object.ccdd_caucajl.protect = 0             
				  			This.Object.ccdd_caucajl.BackGround.Color	=	RGB(255,255,255)
					ELSE 
						  This.Object.ccdd_rescaj.protect = 0  
						  This.SetItem(il_fila, "ccdd_rescaj", 'A')
						  This.Object.ccdd_caucajl.protect = 1             				  
						  This.Object.ccdd_caucajl.BackGround.Color	=	RGB(192,192,192)
				  END IF
			END IF
		
	CASE "ccdd_rescaj"
		IF data = 'O' THEN
			This.Object.ccdd_caucajl.protect = 0             
			This.Object.ccdd_caucajl.BackGround.Color	=	RGB(255,255,255)			
			SetColumn("ccdd_caucajl")
		ELSE	
			This.Object.ccdd_caucajl.protect = 1             
			This.Object.ccdd_caucajl.BackGround.Color	=	RGB(192,192,192)
			Tab_1.TabPage_4.dw_resolucion.Setitem(il_fila, "ccdd_caucajl",li_Nula)									
		END IF		
		
	CASE "ccdd_caucajl"
		IF NOT Causalcajalote(Integer(data)) THEN
			Tab_1.TabPage_4.dw_resolucion.Setitem(row, "ccdd_caucajl",li_Nula)				
			RETURN 1 
		END IF 
END CHOOSE

end event

event losefocus;call super::losefocus;Tab_1.TabPage_1.dw_Embalaje.accepttext()
Tab_1.TabPage_2.dw_Calidad.accepttext()
Tab_1.TabPage_3.dw_condicion.accepttext()
Tab_1.TabPage_4.dw_Resolucion.accepttext()

end event

