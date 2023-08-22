$PBExportHeader$w_mant_deta_ctlcaldanoespecie.srw
$PBExportComments$Mantenedor de Daños por Especie.
forward
global type w_mant_deta_ctlcaldanoespecie from w_mant_detalle_csd
end type
end forward

global type w_mant_deta_ctlcaldanoespecie from w_mant_detalle_csd
integer x = 123
integer y = 96
integer width = 2501
integer height = 1224
end type
global w_mant_deta_ctlcaldanoespecie w_mant_deta_ctlcaldanoespecie

type variables
DataWindowChild	 	idwc_especie, idwc_familia, idwc_subfamilia, idwc_grupo,&
                     idwc_subgrupo, idwc_subgrupodet, idwc_planta


end variables

forward prototypes
public function boolean noexistegrupo (integer ai_familia, integer ai_subfamilia, integer ai_grupo)
public function boolean noexistesubfamilia (integer ai_familia, integer ai_subfamilia)
public function boolean noexistesubgrupo (integer ai_familia, integer ai_subfamilia, integer ai_grupo, integer ai_subgrupo)
public function boolean noexistesubgrupodet (integer ai_familia, integer ai_subfamilia, integer ai_grupo, integer ai_subgrupo, integer ai_subgrupodet)
public function boolean duplicado (string as_valor, integer ai_tipo)
private function boolean noexistefamilia (integer ai_familia)
end prototypes

public function boolean noexistegrupo (integer ai_familia, integer ai_subfamilia, integer ai_grupo);String ls_nomfam

SELECT	ccgr_descrip
	INTO	:Istr_Mant.Argumento[9]
	FROM	dba.CTLCALGRUPO
	WHERE	ccfa_codigo	=	:ai_familia 	AND
			ccsf_codigo	=	:ai_subfamilia AND
			ccgr_codigo	=	:ai_grupo;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de tabla CTLCALGRUPO")
	
	RETURN True
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "Código de Grupo no ha sido Definido.~r~r" + &
					"Ingrese o seleccione otro código.")
	
	RETURN True
END IF	


RETURN False
end function

public function boolean noexistesubfamilia (integer ai_familia, integer ai_subfamilia);String ls_nomfam

SELECT	ccsf_descrip
	INTO	:istr_Mant.Argumento[10]
	FROM	dba.CTLCALSUBFAMILIA
	WHERE	ccfa_codigo	=	:ai_familia 	AND
			ccsf_codigo	=	:ai_subfamilia;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de tabla CTLCALSUBFAMILIA")
	
	RETURN True
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "Código de SubFamilia no ha sido Definido.~r~r" + &
					"Ingrese o seleccione otro código.")
	
	RETURN True
END IF	


RETURN False
end function

public function boolean noexistesubgrupo (integer ai_familia, integer ai_subfamilia, integer ai_grupo, integer ai_subgrupo);String ls_nomfam

SELECT	ccsg_descrip
	INTO	:Istr_Mant.Argumento[11]
	FROM	dba.CTLCALSUBGRUPO
	WHERE	ccfa_codigo	=	:ai_familia 	AND
			ccsf_codigo	=	:ai_subfamilia AND
			ccgr_codigo	=	:ai_grupo 		AND
			ccsg_codigo	=	:ai_subgrupo;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de tabla CTLCALSUBGRUPO")
	
	RETURN True
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "Código de SubGrupo no ha sido Definido.~r~r" + &
					"Ingrese o seleccione otro código.")
	
	RETURN True
END IF	


RETURN False
end function

public function boolean noexistesubgrupodet (integer ai_familia, integer ai_subfamilia, integer ai_grupo, integer ai_subgrupo, integer ai_subgrupodet);String ls_nomfam
SELECT	ccde_descrip
	INTO	:Istr_Mant.Argumento[12]
	FROM	dba.CTLCALSUBGRUPODET
	WHERE	ccfa_codigo	=	:ai_familia 	AND
			ccsf_codigo	=	:ai_subfamilia AND
			ccgr_codigo	=	:ai_grupo 		AND
			ccsg_codigo	=	:ai_subgrupo	AND
			ccde_Secuen	=	:ai_subgrupodet;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de tabla CTLCALSUBGRUPODET")
	
	RETURN True
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "Código de SubGrupoDetalle no ha sido Definido.~r~r" + &
					"Ingrese o seleccione otro código.")
	
	RETURN True
END IF	


RETURN False
end function

public function boolean duplicado (string as_valor, integer ai_tipo);Long     ll_fila

ll_fila = dw_1.Find("ccda_secuen = " + as_valor, 1, dw_1.RowCount())

IF ll_fila > 0 AND ll_fila <> il_fila THEN
	MessageBox("Error","Registro ya fue ingresado anteriormente",Information!, Ok!)
	RETURN True
ELSE
	RETURN False
END IF

end function

private function boolean noexistefamilia (integer ai_familia);String ls_nomfam

SELECT	ccfa_descrip
	INTO	:istr_Mant.Argumento[8]
	FROM	dba.CTLCALFAMILIAS
	WHERE	ccfa_codigo	=	:ai_familia;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de tabla CTLCALFAMILIAS")	
	RETURN True
	
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "Código de Familia no ha sido Definido.~r~r" + &
					"Ingrese o seleccione otro código.")	
	RETURN True
END IF	
RETURN False
end function

on w_mant_deta_ctlcaldanoespecie.create
call super::create
end on

on w_mant_deta_ctlcaldanoespecie.destroy
call super::destroy
end on

event ue_recuperadatos;call super::ue_recuperadatos;String	ls_Usuario 
Integer	li_Grupo
ls_Usuario	=	Upper(Gstr_Us.Nombre)

ias_campo[2]	= 	String(dw_1.Object.espe_codigo[il_Fila])
ias_campo[3]	= 	String(dw_1.Object.ccda_secuen[il_Fila])
ias_campo[4]	= 	String(dw_1.Object.ccfa_codigo[il_Fila])
ias_campo[5]	= 	String(dw_1.Object.ccsf_codigo[il_Fila])
ias_campo[6]	= 	String(dw_1.Object.ccgr_codigo[il_Fila])
ias_campo[7]	= 	String(dw_1.Object.ccsg_codigo[il_Fila])
ias_campo[8]	= 	String(dw_1.Object.ccde_secuen[il_Fila])
ias_campo[12]	= 	String(dw_1.Object.ccda_descri[il_Fila])

dw_1.SetItem(il_fila, "espe_codigo", Integer(istr_mant.argumento[2]))

dw_1.SetItemStatus(il_fila, "espe_codigo", Primary!, NotModified!)


IF istr_mant.agrega = False THEN
	idwc_subfamilia.SetTransObject(SQLCA)
	idwc_subfamilia.Retrieve(Integer(ias_campo[4]))
	idwc_grupo.SetTransObject(SQLCA)
	idwc_grupo.Retrieve(Integer(ias_campo[4]), Integer(ias_campo[5]))
	idwc_subgrupo.SetTransObject(SQLCA)
	idwc_subgrupo.Retrieve(Integer(ias_campo[4]), Integer(ias_campo[5]), Integer(ias_campo[6]))
	idwc_subgrupodet.SetTransObject(SQLCA)
	idwc_subgrupodet.Retrieve(Integer(ias_campo[4]), Integer(ias_campo[5]), Integer(ias_campo[6]), Integer(ias_campo[7]))
	dw_1.Object.espe_codigo.Protect	=	1
	dw_1.Object.espe_codigo.BackGround.Color	=	RGB(192,192,192)
	dw_1.Object.ccda_secuen.Protect	=	1
	dw_1.Object.ccda_secuen.BackGround.Color	=	RGB(192,192,192)
END IF

IF dw_1.Rowcount() > 0 THEN	
	li_Grupo = BuscaGrupo(ls_Usuario)					
	IF (li_Grupo = 2) OR (li_Grupo = 1) THEN 			
		dw_1.SetFocus()
		//il_fila					= 1
		pb_acepta.Enabled		= True
		pb_cancela.Enabled	= True
	ELSE
		dw_1.Enabled			= False	
		pb_acepta.Enabled		= False	
		pb_salir.Enabled		= True 	
	END IF 	
END IF 
end event

event ue_deshace;call super::ue_deshace;IF UpperBound(ias_campo) > 0 THEN
	dw_1.SetItem(il_fila, "espe_codigo", Integer(ias_campo[2]))
	dw_1.SetItem(il_fila, "ccda_secuen", Integer(ias_campo[3]))
	dw_1.SetItem(il_fila, "ccfa_codigo", Integer(ias_campo[4]))
	dw_1.SetItem(il_fila, "ccsf_codigo", Integer(ias_campo[5]))
	dw_1.SetItem(il_fila, "ccgr_codigo", Integer(ias_campo[6]))
	dw_1.SetItem(il_fila, "ccsg_codigo", Integer(ias_campo[7]))
	dw_1.SetItem(il_fila, "ccde_secuen", integer(ias_campo[8]))
	dw_1.SetItem(il_fila, "ccda_descri", ias_campo[12])	
END IF

end event

event ue_antesguardar;String	ls_mensaje, ls_colu[]
Integer	li_cont

IF Isnull(dw_1.Object.espe_codigo[il_fila]) OR dw_1.Object.espe_codigo[il_fila] = 0 THEN
	li_cont	++
	ls_mensaje 			= ls_mensaje + "~nEspecie"
	ls_colu[li_cont]	= "espe_codigo"
END IF


IF Isnull(dw_1.Object.ccda_secuen[il_fila]) OR dw_1.Object.ccda_secuen[il_fila] = 0 THEN
	li_cont	++
	ls_mensaje 			= ls_mensaje + "~nSecuencia"
	ls_colu[li_cont]	= "ccda_secuen"
END IF

IF Isnull(dw_1.Object.ccfa_codigo[il_fila]) OR dw_1.Object.ccfa_codigo[il_fila] = 0 THEN
	li_cont	++
	ls_mensaje 			= ls_mensaje + "~nFamilia"
	ls_colu[li_cont]	= "ccfa_codigo"
END IF

IF li_cont > 0 THEN
	MessageBox("Error de Consistencia", "Falta el Ingreso de :" + ls_mensaje + ".", StopSign!, Ok!)
	dw_1.SetColumn(ls_colu[1])
	dw_1.SetFocus()
	Message.DoubleParm = -1
END IF
end event

event ue_nuevo;call super::ue_nuevo;
dw_1.SetItem(il_fila, "espe_codigo", Integer(istr_mant.argumento[2]))
dw_1.SetItemStatus(il_fila, "espe_codigo", Primary!, NotModified!)
end event

event open;/*
	Argumentos :
						[2]	=	Código de especie
						[3]   =  Familia
						[4]	= 	Sub Familia
						[5]	=	Grupo
						[6]	=	Sub Grupo
						[7]	=	Sub Grupo Detalle
						[8]	=	Nombre Familia	
						[9]	= 	Nombre Sub Familia
						[10]	=  Nombre Grupo
						[11]	=  Nombre Sub Grupo
						[12]	=	
*/
Long ls_Null, li_cuenta

x	= 100
y	= 450

This.Icon	=	Gstr_apl.Icono

PostEvent("ue_recuperadatos")

istr_mant = Message.PowerObjectParm

dw_1.GetChild("espe_codigo", idwc_especie)
dw_1.GetChild("ccfa_codigo", idwc_familia)

idwc_especie.SetTransObject(SQLCA)
idwc_especie.Retrieve()

idwc_familia.SetTransObject(SQLCA)

IF idwc_familia.Retrieve()	=	0 THEN 
	idwc_familia.insertrow(0)
END IF

dw_1.GetChild("ccsf_codigo", idwc_subfamilia)
idwc_subfamilia.SetTransObject(SQLCA)

IF idwc_subfamilia.Retrieve(0)	=	0 THEN 
	idwc_subfamilia.insertrow(0)
END IF

dw_1.GetChild("ccgr_codigo", idwc_grupo)
idwc_grupo.SetTransObject(SQLCA)

IF idwc_grupo.Retrieve(0,0)	=	0 THEN 
	idwc_grupo.insertrow(0)
END IF	

dw_1.GetChild("ccsg_codigo", idwc_subgrupo)
idwc_subgrupo.SetTransObject(SQLCA)

IF idwc_subgrupo.Retrieve(0,0,0)	=	0 THEN 
	idwc_subgrupo.insertrow(0)
END IF

dw_1.Getchild("ccde_secuen", idwc_subgrupodet)
idwc_subgrupodet.SetTransObject(SQLCA)

IF idwc_subgrupodet.Retrieve(0,0,0,0)	=	0 THEN 
	idwc_subgrupodet.insertrow(0)
END IF

dw_1.SetTransObject(sqlca)
istr_mant.dw.ShareData(dw_1)
end event

type pb_ultimo from w_mant_detalle_csd`pb_ultimo within w_mant_deta_ctlcaldanoespecie
end type

type pb_siguiente from w_mant_detalle_csd`pb_siguiente within w_mant_deta_ctlcaldanoespecie
end type

type pb_anterior from w_mant_detalle_csd`pb_anterior within w_mant_deta_ctlcaldanoespecie
end type

type pb_primero from w_mant_detalle_csd`pb_primero within w_mant_deta_ctlcaldanoespecie
end type

type pb_cancela from w_mant_detalle_csd`pb_cancela within w_mant_deta_ctlcaldanoespecie
integer x = 2181
integer y = 384
end type

type pb_acepta from w_mant_detalle_csd`pb_acepta within w_mant_deta_ctlcaldanoespecie
integer x = 2176
integer y = 168
end type

type pb_salir from w_mant_detalle_csd`pb_salir within w_mant_deta_ctlcaldanoespecie
integer x = 2176
integer y = 600
end type

type dw_1 from w_mant_detalle_csd`dw_1 within w_mant_deta_ctlcaldanoespecie
integer x = 96
integer y = 96
integer width = 1883
integer height = 996
string dataobject = "dw_mant_ctlcaldanoespecie"
boolean maxbox = true
end type

event dw_1::itemchanged;Long		Familia, SubFamilia, Grupo, SubGrupo, SubGrupoDet
String	ls_columna, ls_Null
Integer	li_Null

SetNull(ls_Null)
SetNull(li_Null)

ls_columna	=	dwo.Name

CHOOSE CASE ls_columna
	
	CASE "ccfa_codigo"		
		IF NoexisteFamilia(Integer(Data)) THEN
			This.SetItem(Row, "ccfa_codigo", li_Null)
      	RETURN 1	
		ELSE	
			This.SetItem(il_fila,"ccfa_descrip",istr_Mant.Argumento[8])
			idwc_subfamilia.SetTransObject(SQLCA)
			IF idwc_subfamilia.Retrieve(Integer(Data)) = 0 THEN
				This.SetColumn("ccda_secuen")
			END IF	
		END IF	
		
	CASE "ccsf_codigo"
		IF NoExisteSubFamilia(dw_1.Object.ccfa_codigo[Row],Integer(Data))	THEN
 			This.SetItem(Row, "ccsf_codigo", li_Null)
      	RETURN 1	
		ELSE	
			This.SetItem(il_fila,"ccsf_descrip",istr_Mant.Argumento[10])
			idwc_grupo.SetTransObject(SQLCA)
   		IF idwc_grupo.Retrieve(dw_1.Object.ccfa_codigo[Row],Integer(data)) = 0 THEN
				This.SetColumn("ccda_secuen")
			END IF		
		END IF	
	
	CASE "ccgr_codigo"		
		IF NoExisteGrupo(dw_1.Object.ccfa_codigo[Row], + &
							  dw_1.Object.ccsf_codigo[Row], + &
							  Integer(Data)) THEN
			This.SetItem(Row, "ccgr_codigo", li_Null)
      	RETURN 1	
		ELSE	
			This.SetItem(il_fila,"ccgr_descrip",istr_Mant.Argumento[9])
			idwc_subgrupo.SetTransObject(SQLCA)
			IF idwc_subgrupo.Retrieve(dw_1.Object.ccfa_codigo[Row], + &
											  dw_1.Object.ccsf_codigo[Row],+ &
											  Integer(data)) = 0 THEN
				This.SetColumn("ccda_secuen")
			END IF	
		END IF	
	
	CASE "ccsg_codigo"
		IF NoexisteSubGrupo(dw_1.Object.ccfa_codigo[Row], + &
								  dw_1.Object.ccsf_codigo[Row], + &
								  dw_1.Object.ccgr_codigo[Row],Integer(Data)) THEN
			This.SetItem(Row, "ccsg_codigo", li_Null)
      	RETURN 1	
		ELSE	
			This.SetItem(il_fila,"ccsg_descrip",istr_Mant.Argumento[11])
			idwc_subgrupodet.SetTransObject(SQLCA)
			IF idwc_subgrupodet.Retrieve(dw_1.Object.ccfa_codigo[Row], + &
												  dw_1.Object.ccsf_codigo[Row], + &
												  dw_1.Object.ccgr_codigo[Row],Integer(Data)) = 0 THEN
				This.SetColumn("ccda_secuen")
			END IF	
		END IF	
	
	CASE "ccde_secuen"		
		IF NoExisteSubGrupoDet(dw_1.Object.ccfa_codigo[Row], + &
									  dw_1.Object.ccsf_codigo[Row], + &
									  dw_1.Object.ccgr_codigo[Row], + & 
									  dw_1.Object.ccsg_codigo[Row], + &
									  Integer(Data)) THEN
			This.SetItem(Row, "ccde_secuen", li_Null)
      	RETURN 1
		ELSE	
			Istr_Mant.Argumento[7]	=	data
			This.SetItem(il_fila,"ccde_descrip",istr_Mant.Argumento[12])
		END IF	

	CASE "ccda_secuen"
		IF Duplicado(data, 1) THEN
			This.SetItem(Row, "ccda_secuen", li_Null)
			RETURN 1
		ELSE
			RETURN 0
		END IF		
		
END CHOOSE
end event

event dw_1::itemerror;call super::itemerror;RETURN 1
end event

