$PBExportHeader$w_mant_deta_cuarteles.srw
forward
global type w_mant_deta_cuarteles from w_mant_detalle_csd
end type
type tab_cuartel from tab within w_mant_deta_cuarteles
end type
type tabpage_1 from userobject within tab_cuartel
end type
type dw_cuartel from uo_dw within tabpage_1
end type
type tabpage_1 from userobject within tab_cuartel
dw_cuartel dw_cuartel
end type
type tabpage_2 from userobject within tab_cuartel
end type
type dw_plantas from uo_dw within tabpage_2
end type
type tabpage_2 from userobject within tab_cuartel
dw_plantas dw_plantas
end type
type tabpage_3 from userobject within tab_cuartel
end type
type dw_cajas from uo_dw within tabpage_3
end type
type tabpage_3 from userobject within tab_cuartel
dw_cajas dw_cajas
end type
type tabpage_4 from userobject within tab_cuartel
end type
type dw_observa from uo_dw within tabpage_4
end type
type tabpage_4 from userobject within tab_cuartel
dw_observa dw_observa
end type
type tabpage_5 from userobject within tab_cuartel
end type
type dw_variedad from uo_dw within tabpage_5
end type
type pb_elimina from picturebutton within tabpage_5
end type
type pb_inserta from picturebutton within tabpage_5
end type
type tabpage_5 from userobject within tab_cuartel
dw_variedad dw_variedad
pb_elimina pb_elimina
pb_inserta pb_inserta
end type
type tab_cuartel from tab within w_mant_deta_cuarteles
tabpage_1 tabpage_1
tabpage_2 tabpage_2
tabpage_3 tabpage_3
tabpage_4 tabpage_4
tabpage_5 tabpage_5
end type
type dw_2 from uo_dw within w_mant_deta_cuarteles
end type
end forward

global type w_mant_deta_cuarteles from w_mant_detalle_csd
integer x = 123
integer y = 96
integer width = 3337
integer height = 1388
string title = "CUARTELES"
boolean controlmenu = true
tab_cuartel tab_cuartel
dw_2 dw_2
end type
global w_mant_deta_cuarteles w_mant_deta_cuarteles

type variables
String is_rut, is_codigo
Integer ii_zonas

uo_especie		iuo_especies
uo_variedades	iuo_variedades
uo_variedades	iuo_variedades2
uo_conduccion	iuo_conduccion	
uo_riegos		iuo_riegos	
uo_nrosemana	iuo_semana

DataWindowChild 	idwc_especie, idwc_variedad, idwc_riego, &
						idwc_conduccion, idwc_predio,idwc_cuarteles, idwc_Variedad2, idwc_Especie2
end variables

forward prototypes
public function boolean existeriego (integer ai_codigo)
public function boolean existeconduccion (integer ai_codigo)
public subroutine parampredios ()
public subroutine wf_calcula_superficie (string valor, string columna)
end prototypes

public function boolean existeriego (integer ai_codigo);Integer li_codigo
String   ls_Nombre, ls_abrevi
Boolean	lb_Retorno = True

SELECT	siri_codigo, siri_nombre, siri_abrevi
	INTO	:li_codigo, :ls_Nombre, :ls_abrevi
	FROM	dbo.prodsistriego
  WHERE	siri_codigo	=	:ai_codigo;
  
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla de Riegos")
	
	lb_Retorno	=	False
ELSEIF sqlca.SQLCode=100 THEN
	MessageBox("Atención","Codigo de Riego No ha sido creado. Ingrese Otro.")
	lb_Retorno	=	False
END IF

RETURN lb_Retorno
end function

public function boolean existeconduccion (integer ai_codigo);Integer li_codigo
String   ls_Nombre, ls_abrevi
Boolean	lb_Retorno = True

SELECT	sico_codigo, sico_nombre, sico_abrevi
	INTO	:li_codigo, :ls_Nombre, :ls_abrevi
	FROM	dbo.prodsistconduc
  WHERE	sico_codigo	=	:ai_codigo;
  
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla de Conducción")
	
	lb_Retorno	=	False
ELSEIF sqlca.SQLCode	=	100 THEN
	MessageBox("Atención","Codigo de Conducción No ha sido creado. Ingrese Otro.")
	lb_Retorno	=	False
END IF

RETURN lb_Retorno
end function

public subroutine parampredios ();String tempo1, tempo2, tempo3, tempo4, tempo5

  SELECT papc_tempo1,papc_tempo2, papc_tempo3,papc_tempo4,papc_tempo5  
    INTO :tempo1, :tempo2, :tempo3, :tempo4, :tempo5  
    FROM dbo.parampredioscuarteles;  
	
	dw_1.Modify("tempo1.text = '" + tempo1 + "'")
	dw_1.Modify("tempo2.text = '" + tempo2 + "'")
	dw_1.Modify("tempo3.text = '" + tempo3 + "'")
	dw_1.Modify("tempo4.text = '" + tempo4 + "'")
	dw_1.Modify("tempo5.text = '" + tempo5 + "'")


end subroutine

public subroutine wf_calcula_superficie (string valor, string columna);Decimal{2}	ld_de,ld_ds,ld_has
Long			ll_pb,ll_pr,ll_pm, ll_densidad, ll_total

ld_de	=	dw_1.Object.prcc_ptaent[il_fila]
ld_ds	=	dw_1.Object.prcc_ptasob[il_fila]
ll_pb	=	dw_1.Object.prcc_plabue[il_fila]
ll_pr	=	dw_1.Object.prcc_plareg[il_fila]
ll_pm	=	dw_1.Object.prcc_plamal[il_fila]

Choose Case columna
	Case "prcc_ptaent"
		ld_de	=	Dec(valor)
		
	Case "prcc_ptasob"
		ld_ds	=	Dec(valor)
		
	Case "prcc_plabue"
		ll_pb	=	Long(valor)
		
	Case "prcc_plareg"
		ll_pr	=	Long(valor)
		
	Case "prcc_plamal"
		ll_pm	=	Long(valor)
		
End Choose 

If ld_de > 0 AND ld_ds > 0 Then 
	//Calcula Densidad
	ll_densidad	=	Round(10000 / (ld_de * ld_ds),0)
	dw_1.SetItem(il_fila,"prcc_densid",ll_densidad)
	
	//Cálculo de Superficie
	ld_has	=	Round((ll_pb + ll_pr + ll_pm) * (ld_de * ld_ds) / 10000,2)
	dw_1.SetItem(il_fila,"prcc_superf",ld_has)
	
	//Calcula Total de Plantas
	ll_total = (ll_pb + ll_pr + ll_pm)
	dw_1.SetItem(il_fila,"prcc_nropta",ll_total)	
End If

Return
end subroutine

on w_mant_deta_cuarteles.create
int iCurrent
call super::create
this.tab_cuartel=create tab_cuartel
this.dw_2=create dw_2
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.tab_cuartel
this.Control[iCurrent+2]=this.dw_2
end on

on w_mant_deta_cuarteles.destroy
call super::destroy
destroy(this.tab_cuartel)
destroy(this.dw_2)
end on

event ue_recuperadatos;call super::ue_recuperadatos;Tab_Cuartel.TabPage_1.dw_cuartel.SetRedraw(False)
Tab_Cuartel.TabPage_1.dw_cuartel.SetRow(il_fila)
Tab_Cuartel.TabPage_1.dw_cuartel.ScrollToRow(il_fila)
Tab_Cuartel.TabPage_1.dw_cuartel.SetRedraw(True)

Tab_Cuartel.TabPage_2.dw_plantas.SetRedraw(False)
Tab_Cuartel.TabPage_2.dw_plantas.SetRow(il_fila)
Tab_Cuartel.TabPage_2.dw_plantas.ScrollToRow(il_fila)
Tab_Cuartel.TabPage_2.dw_plantas.SetRedraw(True)

Tab_Cuartel.TabPage_3.dw_cajas.SetRedraw(False)
Tab_Cuartel.TabPage_3.dw_cajas.SetRow(il_fila)
Tab_Cuartel.TabPage_3.dw_cajas.ScrollToRow(il_fila)
Tab_Cuartel.TabPage_3.dw_cajas.SetRedraw(True)

Tab_Cuartel.TabPage_4.dw_observa.SetRedraw(False)
Tab_Cuartel.TabPage_4.dw_observa.SetRow(il_fila)
Tab_Cuartel.TabPage_4.dw_observa.ScrollToRow(il_fila)
Tab_Cuartel.TabPage_4.dw_observa.SetRedraw(True)


ias_campo[1] 	= String(dw_1.GetItemNumber(il_fila, "prcc_codigo"))
ias_campo[2] 	= dw_1.GetItemString(il_fila, "prcc_nombre")
ias_campo[3] 	= String(dw_1.GetItemNumber(il_fila, "espe_codigo"))
ias_campo[4] 	= String(dw_1.GetItemNumber(il_fila, "vari_codigo"))
ias_campo[5]	= String(dw_1.GetItemNumber(il_fila, "siri_codigo"))
ias_campo[6] 	= String(dw_1.GetItemNumber(il_fila, "sico_codigo"))
ias_campo[7] 	= String(dw_1.GetItemNumber(il_fila, "prcc_superf"))
ias_campo[8] 	= dw_1.GetItemString(il_fila, "prcc_porinj")
ias_campo[9] 	= String(dw_1.GetItemNumber(il_fila, "prcc_anoinj"))
ias_campo[10] 	= String(dw_1.GetItemNumber(il_fila, "prcc_ptasob"))
ias_campo[11] 	= String(dw_1.GetItemNumber(il_fila, "prcc_ptaent"))
ias_campo[12] 	= String(dw_1.GetItemNumber(il_fila, "prcc_nropta"))
ias_campo[13] 	= String(dw_1.GetItemNumber(il_fila, "prcc_anopla"))
ias_campo[14] 	= String(dw_1.GetItemNumber(il_fila, "prcc_disgot"))
ias_campo[15] 	= String(dw_1.GetItemNumber(il_fila, "prcc_caugot"))
ias_campo[16] 	= String(dw_1.GetItemNumber(il_fila, "prcc_gothec"))
ias_campo[17] 	= String(dw_1.GetItemNumber(il_fila, "prcc_gashec"))
ias_campo[18] 	= String(dw_1.GetItemNumber(il_fila, "prcc_cjtem1"))
ias_campo[19] 	= String(dw_1.GetItemNumber(il_fila, "prcc_cjtem2"))
ias_campo[20]	= String(dw_1.GetItemNumber(il_fila, "prcc_cjtem3"))
ias_campo[21] 	= String(dw_1.GetItemNumber(il_fila, "prcc_cjtem4"))
ias_campo[22] 	= String(dw_1.GetItemNumber(il_fila, "prcc_cjtem5"))
ias_campo[23] 	= String(dw_1.GetItemNumber(il_fila, "prod_codigo"))
ias_campo[24] 	= String(dw_1.GetItemNumber(il_fila, "prcc_cuapro"))
ias_campo[25] 	= dw_1.GetItemString(il_fila, "prcc_nompro")
ias_campo[26] 	= String(dw_1.GetItemNumber(il_fila, "prcc_densid"))
ias_campo[27] 	= String(dw_1.GetItemNumber(il_fila, "prcc_plabue"))
ias_campo[28] 	= String(dw_1.GetItemNumber(il_fila, "prcc_plareg"))
ias_campo[29] 	= String(dw_1.GetItemNumber(il_fila, "prcc_plamal"))
ias_campo[30] 	= String(dw_1.GetItemNumber(il_fila, "prcc_frubue"))
ias_campo[31] 	= String(dw_1.GetItemNumber(il_fila, "prcc_frureg"))
ias_campo[32] 	= String(dw_1.GetItemNumber(il_fila, "prcc_frumal"))
ias_campo[33] 	= dw_1.GetItemString(il_fila, "prcc_encarg")
ias_campo[34] 	= String(dw_1.GetItemNumber(il_fila, "patr_codigo"))
ias_campo[35] 	= String(dw_1.GetItemDate(il_fila, "prcc_fecact"), 'dd/mm/yyyy)')
ias_campo[36] 	= dw_1.GetItemString(il_fila, "prcc_observ")
ias_campo[37] 	= dw_1.GetItemString(il_fila, "prcc_obscli")
ias_campo[38] 	= dw_1.GetItemString(il_fila, "prcc_obsral")
ias_campo[39] 	= dw_1.GetItemString(il_fila, "prcc_obsman")
ias_campo[40] 	= String(dw_1.GetItemNumber(il_fila, "prcc_porpro"))
ias_campo[41] 	= String(dw_1.GetItemNumber(il_fila, "prcc_pprose"))
ias_campo[42] 	= String(dw_1.GetItemNumber(il_fila, "prcc_poremb"))
ias_campo[43] 	= String(dw_1.GetItemNumber(il_fila, "prcc_pembse"))
ias_campo[44] 	= String(dw_1.GetItemNumber(il_fila, "prcc_cmedse"))
ias_campo[46] 	= String(dw_1.GetItemNumber(il_fila, "prcc_calmed"))
ias_campo[47] 	= String(dw_1.GetItemNumber(il_fila, "prcc_estado"))
ias_campo[48] 	= String(dw_1.GetItemNumber(il_fila, "prcc_cajhec"))
ias_campo[49] 	= String(dw_1.GetItemNumber(il_fila, "prcc_cajcuar"))

ias_campo[50] 	= String(dw_1.Object.prcc_cajhec[il_fila])
ias_campo[51] 	= String(dw_1.Object.prcc_cajcuar[il_fila])

Parampredios()

If Not istr_mant.agrega And Not istr_mant.borra Then
	idwc_variedad.SetTransObject(sqlca)
	idwc_variedad.Retrieve(Integer(ias_campo[3]))
	idwc_variedad.SetSort("vari_nombre A")
	idwc_variedad.Sort()
	
	Tab_Cuartel.TabPage_5.dw_Variedad.GetChild("vari_codigo", idwc_variedad2)
	idwc_variedad2.SetTransObject(sqlca)
	idwc_variedad2.Retrieve(Integer(ias_campo[3]))
	idwc_variedad2.SetSort("vari_nombre A")
	idwc_variedad2.Sort()

	IF NOT Upper(gstr_us.Nombre) =Upper(gstr_parametros.admin1) and NOT Upper(gstr_us.Nombre) = Upper(gstr_parametros.admin2) Then		
		Tab_Cuartel.TabPage_1.dw_cuartel.Object.prcc_codigo.Protect 	= 1
		Tab_Cuartel.TabPage_1.dw_cuartel.Object.espe_codigo.Protect	= 1
		Tab_Cuartel.TabPage_1.dw_cuartel.Object.vari_codigo.Protect 	= 1
		Tab_Cuartel.TabPage_1.dw_cuartel.Object.patr_codigo.Protect 	= 1
		Tab_Cuartel.TabPage_2.dw_plantas.Object.prcc_superf.Protect 	= 1
		
		Tab_Cuartel.TabPage_2.dw_plantas.Object.prcc_superf.Color	= RGB(255,255,255)
		Tab_Cuartel.TabPage_1.dw_cuartel.Object.prcc_codigo.Color	= RGB(255,255,255)
		Tab_Cuartel.TabPage_1.dw_cuartel.Object.espe_codigo.Color	= RGB(255,255,255)
		Tab_Cuartel.TabPage_1.dw_cuartel.Object.vari_codigo.Color	= RGB(255,255,255)
		Tab_Cuartel.TabPage_1.dw_cuartel.Object.patr_codigo.Color	= RGB(255,255,255)
		
		Tab_Cuartel.TabPage_2.dw_plantas.Object.prcc_superf.BackGround.Color	= 553648127
		Tab_Cuartel.TabPage_1.dw_cuartel.Object.prcc_codigo.BackGround.Color	= 553648127
		Tab_Cuartel.TabPage_1.dw_cuartel.Object.espe_codigo.BackGround.Color	= 553648127
		Tab_Cuartel.TabPage_1.dw_cuartel.Object.vari_codigo.BackGround.Color	= 553648127
		Tab_Cuartel.TabPage_1.dw_cuartel.Object.patr_codigo.BackGround.Color	= 553648127
	Else
		Tab_Cuartel.TabPage_1.dw_cuartel.Object.prcc_codigo.Protect 	= 0
		Tab_Cuartel.TabPage_1.dw_cuartel.Object.espe_codigo.Protect	= 0
		Tab_Cuartel.TabPage_1.dw_cuartel.Object.vari_codigo.Protect 	= 0
		Tab_Cuartel.TabPage_1.dw_cuartel.Object.patr_codigo.Protect 	= 0
		Tab_Cuartel.TabPage_2.dw_plantas.Object.prcc_superf.Protect 	= 0

		Tab_Cuartel.TabPage_2.dw_plantas.Object.prcc_superf.Color	= 0
		Tab_Cuartel.TabPage_1.dw_cuartel.Object.prcc_codigo.Color	= 0
		Tab_Cuartel.TabPage_1.dw_cuartel.Object.espe_codigo.Color	= 0
		Tab_Cuartel.TabPage_1.dw_cuartel.Object.vari_codigo.Color	= 0
		Tab_Cuartel.TabPage_1.dw_cuartel.Object.patr_codigo.Color	= 0
		
		Tab_Cuartel.TabPage_2.dw_plantas.Object.prcc_superf.BackGround.Color	= RGB(255,255,255)
		Tab_Cuartel.TabPage_1.dw_cuartel.Object.prcc_codigo.BackGround.Color	= RGB(255,255,255)
		Tab_Cuartel.TabPage_1.dw_cuartel.Object.espe_codigo.BackGround.Color	= RGB(255,255,255)
		Tab_Cuartel.TabPage_1.dw_cuartel.Object.vari_codigo.BackGround.Color	= RGB(255,255,255)
		Tab_Cuartel.TabPage_1.dw_cuartel.Object.patr_codigo.BackGround.Color	= RGB(255,255,255)
	End If
Else
	dw_1.Object.prpr_codigo[il_fila] = Long(istr_mant.argumento[1])
	dw_1.Object.prod_codigo[il_fila] = Long(istr_mant.argumento[2])
End If
end event

event ue_deshace;call super::ue_deshace;IF UpperBound(ias_campo) > 0 THEN
	dw_1.SetItem(il_fila, "prod_codigo", long(ias_campo[23]))
	dw_1.SetItem(il_fila, "prcc_codigo", Integer(ias_campo[1]))
	dw_1.SetItem(il_fila, "prcc_nombre", ias_campo[2])
	dw_1.SetItem(il_fila, "espe_codigo", Integer(ias_campo[3]))
	dw_1.SetItem(il_fila, "vari_codigo", Integer(ias_campo[4]))
	dw_1.SetItem(il_fila, "siri_codigo", Integer(ias_campo[5]))
	dw_1.SetItem(il_fila, "sico_codigo", Integer(ias_campo[6]))
	dw_1.SetItem(il_fila, "prcc_superf", long(ias_campo[7]))
	dw_1.SetItem(il_fila, "prcc_porinj", ias_campo[8])
	dw_1.SetItem(il_fila, "prcc_anoinj", Integer(ias_campo[9]))
	dw_1.SetItem(il_fila, "prcc_ptasob", Integer(ias_campo[10]))
	dw_1.SetItem(il_fila, "prcc_ptaent", Integer(ias_campo[11]))
	dw_1.SetItem(il_fila, "prcc_nropta", long(ias_campo[12]))
	dw_1.SetItem(il_fila, "prcc_anopla", Integer(ias_campo[13]))
	dw_1.SetItem(il_fila, "prcc_disgot", long(ias_campo[14]))
	dw_1.SetItem(il_fila, "prcc_caugot", long(ias_campo[15]))
	dw_1.SetItem(il_fila, "prcc_gothec", long(ias_campo[16]))
	dw_1.SetItem(il_fila, "prcc_gashec", long(ias_campo[17]))
	dw_1.SetItem(il_fila, "prcc_cjtem1", long(ias_campo[18]))
	dw_1.SetItem(il_fila, "prcc_cjtem2", long(ias_campo[19]))
	dw_1.SetItem(il_fila, "prcc_cjtem3", long(ias_campo[20]))
	dw_1.SetItem(il_fila, "prcc_cjtem4", long(ias_campo[21]))
	dw_1.SetItem(il_fila, "prcc_cjtem5", long(ias_campo[22]))
	dw_1.SetItem(il_fila, "prcc_cuapro", Long(ias_campo[24]))
	dw_1.SetItem(il_fila, "prcc_nompro", ias_campo[25])		
	dw_1.SetItem(il_fila, "prcc_densid", long(ias_campo[26]))
	dw_1.SetItem(il_fila, "prcc_plabue", long(ias_campo[27]))
	dw_1.SetItem(il_fila, "prcc_plareg", long(ias_campo[28]))
	dw_1.SetItem(il_fila, "prcc_plamal", long(ias_campo[29]))
	dw_1.SetItem(il_fila, "prcc_frubue", Integer(ias_campo[30]))
	dw_1.SetItem(il_fila, "prcc_frureg", Integer(ias_campo[31]))
	dw_1.SetItem(il_fila, "prcc_frumal", Integer(ias_campo[32]))
	dw_1.SetItem(il_fila, "prcc_encarg", ias_campo[33])
	dw_1.SetItem(il_fila, "patr_codigo", Integer(ias_campo[34]))
	dw_1.SetItem(il_fila, "prcc_fecact", ias_campo[35])
	dw_1.SetItem(il_fila, "prcc_observ", ias_campo[36])
	dw_1.SetItem(il_fila, "prcc_obscli", ias_campo[37])
	dw_1.SetItem(il_fila, "prcc_obsral", ias_campo[38])
	dw_1.SetItem(il_fila, "prcc_obsman", ias_campo[39])
	dw_1.SetItem(il_fila, "prcc_porpro", long(ias_campo[40]))
	dw_1.SetItem(il_fila, "prcc_pprose", long(ias_campo[41]))
	dw_1.SetItem(il_fila, "prcc_poremb", long(ias_campo[42]))
	dw_1.SetItem(il_fila, "prcc_pembse", long(ias_campo[43]))
	dw_1.SetItem(il_fila, "prcc_cmedse", long(ias_campo[44]))
	dw_1.SetItem(il_fila, "prcc_calmed", long(ias_campo[45]))
	dw_1.SetItem(il_fila, "prcc_estado", Integer(ias_campo[46]))
	dw_1.SetItem(il_fila, "prcc_cajhec", long(ias_campo[47]))
	dw_1.SetItem(il_fila, "prcc_cajcuar", long(ias_campo[48]))
	dw_1.Object.prcc_cajhec[il_Fila] = Integer(ias_campo[49])
	dw_1.Object.prcc_cajcuar[il_Fila] = Integer(ias_campo[50])
END IF
end event

event ue_antesguardar;call super::ue_antesguardar;String	ls_mensaje, ls_colu[]
Long		li_cont

IF Isnull(dw_1.Object.prod_codigo[il_fila]) OR dw_1.Object.prod_codigo[il_fila] = 0 THEN
	li_cont	++
	ls_mensaje 			= ls_mensaje + "~nCódigo Productor"
	ls_colu[li_cont]	= "prod_codigo"
END IF

IF Isnull(dw_1.Object.prcc_codigo[il_fila]) OR dw_1.Object.prcc_codigo[il_fila] = 0 THEN
	li_cont	++
	ls_mensaje 			= ls_mensaje + "~nCódigo Cuartel"
	ls_colu[li_cont]	= "prcc_codigo"
END IF

IF Isnull(dw_1.Object.prcc_nombre[il_fila]) OR dw_1.Object.prcc_nombre[il_fila] = "" THEN
	li_cont	++
	ls_mensaje 			= ls_mensaje + "~nNombre Cuartel"
	ls_colu[li_cont]	= "prcc_nombre"
END IF

IF Isnull(dw_1.Object.espe_codigo[il_fila]) OR dw_1.Object.espe_codigo[il_fila] = 0 THEN
	li_cont	++
	ls_mensaje 			= ls_mensaje + "~nCódigo Especie"
	ls_colu[li_cont]	= "espe_codigo"
END IF

IF Isnull(dw_1.Object.vari_codigo[il_fila]) OR dw_1.Object.vari_codigo[il_fila] = 0 THEN
	li_cont	++
	ls_mensaje 			= ls_mensaje + "~nCódigo Variedad"
	ls_colu[li_cont]	= "vari_codigo"
END IF

IF Isnull(dw_1.Object.siri_codigo[il_fila]) OR dw_1.Object.siri_codigo[il_fila] = 0 THEN
	li_cont	++
	ls_mensaje 			= ls_mensaje + "~nCódigo Riego"
	ls_colu[li_cont]	= "siri_codigo"
END IF

IF Isnull(dw_1.Object.sico_codigo[il_fila]) OR dw_1.Object.sico_codigo[il_fila] = 0 THEN
	li_cont	++
	ls_mensaje 			= ls_mensaje + "~nCódigo Conducción"
	ls_colu[li_cont]	= "sico_codigo"
END IF

IF Isnull(dw_1.Object.prcc_superf[il_fila]) OR dw_1.Object.prcc_superf[il_fila] = 0 THEN
	li_cont	++
	ls_mensaje 			= ls_mensaje + "~nSuperficie"
	ls_colu[li_cont]	= "prcc_superf"
END IF

IF Isnull(dw_1.Object.prcc_nropta[il_fila]) OR dw_1.Object.prcc_nropta[il_fila] = 0 THEN
	li_cont	++
	ls_mensaje 			= ls_mensaje + "~nNúmero Planta"
	ls_colu[li_cont]	= "prcc_nropta"
END IF

IF Isnull(dw_1.Object.prcc_anopla[il_fila]) OR dw_1.Object.prcc_anopla[il_fila] = 0 THEN
	li_cont	++
	ls_mensaje 			= ls_mensaje + "~nAño Plantación"
	ls_colu[li_cont]	= "prcc_anopla"
END IF

IF li_cont > 0 THEN
	MessageBox("Error de Consistencia", "Falta el ingreso de " + ls_mensaje + ".", StopSign!, Ok!)
	dw_1.SetColumn(ls_colu[1])
	dw_1.SetFocus()
	Message.DoubleParm = -1
END IF
end event

event open;x	= 0
y	= 300

This.Icon	=	Gstr_apl.Icono

PostEvent("ue_recuperadatos")

istr_mant = Message.PowerObjectParm

dw_1.SetTransObject(sqlca)
dw_2.SetTransObject(sqlca)
istr_mant.dw.ShareData(dw_1)
istr_mant.dw2.ShareData(dw_2)

Tab_Cuartel.TabPage_1.dw_Cuartel.SetTransObject(SqlCa)
Tab_Cuartel.TabPage_2.dw_Plantas.SetTransObject(SqlCa)
Tab_Cuartel.TabPage_3.dw_Cajas.SetTransObject(SqlCa)
Tab_Cuartel.TabPage_4.dw_Observa.SetTransObject(SqlCa)
Tab_Cuartel.TabPage_5.dw_variedad.SetTransObject(SqlCa)

dw_1.ShareData(Tab_Cuartel.TabPage_1.dw_Cuartel)
dw_1.ShareData(Tab_Cuartel.TabPage_2.dw_Plantas)
dw_1.ShareData(Tab_Cuartel.TabPage_3.dw_Cajas)
dw_1.ShareData(Tab_Cuartel.TabPage_4.dw_Observa)
dw_2.ShareData(Tab_Cuartel.TabPage_5.dw_Variedad)

iuo_especies		=	Create uo_especie
iuo_variedades 	=  Create uo_variedades
iuo_variedades2	=	Create uo_variedades
iuo_conduccion		=	Create uo_conduccion
iuo_riegos			=	Create uo_riegos
iuo_semana			=	Create uo_nrosemana

Tab_Cuartel.TabPage_5.dw_Variedad.GetChild("espe_codigo", idwc_especie2)
idwc_especie2.SetTransObject(sqlca)
idwc_especie2.Retrieve()

Tab_Cuartel.TabPage_5.dw_Variedad.GetChild("vari_codigo", idwc_variedad2)
idwc_variedad2.SetTransObject(sqlca)
idwc_variedad2.Retrieve(-1)
idwc_variedad2.SetSort("vari_nombre A")
idwc_variedad2.Sort()

Tab_Cuartel.TabPage_1.dw_Cuartel.GetChild("espe_codigo", idwc_especie)
idwc_especie.SetTransObject(sqlca)
idwc_especie.Retrieve()

Tab_Cuartel.TabPage_1.dw_Cuartel.GetChild("vari_codigo", idwc_variedad)
idwc_variedad.SetTransObject(sqlca)
idwc_variedad.Retrieve(-1)
idwc_variedad.SetSort("vari_nombre A")
idwc_variedad.Sort()

Tab_Cuartel.TabPage_1.dw_Cuartel.GetChild("siri_codigo", idwc_riego)
idwc_riego.SetTransObject(sqlca)
idwc_riego.Retrieve()
idwc_riego.InsertRow(0)

Tab_Cuartel.TabPage_1.dw_Cuartel.GetChild("sico_codigo", idwc_conduccion)
idwc_conduccion.SetTransObject(sqlca)
idwc_conduccion.Retrieve()
idwc_riego.InsertRow(0)


end event

event ue_nuevo;call super::ue_nuevo;Parampredios()

Tab_Cuartel.TabPage_1.dw_Cuartel.SetRow(il_fila)
Tab_Cuartel.TabPage_1.dw_Cuartel.ScrolltoRow(il_fila)
Tab_Cuartel.TabPage_2.dw_Plantas.SetRow(il_fila)
Tab_Cuartel.TabPage_2.dw_Plantas.ScrolltoRow(il_fila)
Tab_Cuartel.TabPage_3.dw_Cajas.SetRow(il_fila)
Tab_Cuartel.TabPage_3.dw_Cajas.ScrolltoRow(il_fila)
Tab_Cuartel.TabPage_4.dw_Observa.SetRow(il_fila)
Tab_Cuartel.TabPage_4.dw_Observa.ScrolltoRow(il_fila)

dw_1.Object.prpr_codigo[il_fila] = Long(istr_mant.argumento[1])
dw_1.Object.prod_codigo[il_fila] = Long(istr_mant.argumento[2])
end event

type pb_ultimo from w_mant_detalle_csd`pb_ultimo within w_mant_deta_cuarteles
end type

type pb_siguiente from w_mant_detalle_csd`pb_siguiente within w_mant_deta_cuarteles
end type

type pb_anterior from w_mant_detalle_csd`pb_anterior within w_mant_deta_cuarteles
end type

type pb_primero from w_mant_detalle_csd`pb_primero within w_mant_deta_cuarteles
end type

type pb_cancela from w_mant_detalle_csd`pb_cancela within w_mant_deta_cuarteles
integer x = 2962
integer y = 388
end type

type pb_acepta from w_mant_detalle_csd`pb_acepta within w_mant_deta_cuarteles
integer x = 2958
integer y = 172
end type

event pb_acepta::clicked;Tab_Cuartel.TabPage_1.dw_Cuartel.AcceptText()
Tab_Cuartel.tabpage_2.dw_Plantas.AcceptText()
Tab_Cuartel.tabpage_3.dw_Cajas.AcceptText()
Tab_Cuartel.tabpage_4.dw_Observa.AcceptText()
dw_1.AcceptText()

istr_mant.respuesta = 1

IF istr_mant.agrega THEN
	Parent.TriggerEvent("ue_nuevo")
ELSE
	CloseWithReturn(Parent, istr_mant)
END IF
end event

type pb_salir from w_mant_detalle_csd`pb_salir within w_mant_deta_cuarteles
integer x = 2958
integer y = 604
end type

type dw_1 from w_mant_detalle_csd`dw_1 within w_mant_deta_cuarteles
boolean visible = false
integer x = 37
integer y = 84
integer width = 2894
integer height = 1340
string dataobject = "dw_mant_cuarteles"
boolean hscrollbar = true
boolean vscrollbar = true
boolean resizable = true
end type

event dw_1::itemchanged;Long		ll_fila
String	ls_campo, ls_busca,ls_Nula

ls_campo = dwo.name()
SetNull(ls_Nula)

CHOOSE CASE ls_campo
			
	CASE "prcc_codigo"
		ls_busca = Data
		ll_fila = This.Find("prcc_codigo = " + ls_busca, 1, This.RowCount())
		IF ll_fila > 0 and ll_fila <> il_fila THEN
			MessageBox("Error","Código de Cuartel ya fue ingresado anteriormente",Information!, Ok!)
			This.SetItem(il_fila, "prcc_codigo", Integer(ias_campo[1]))
			RETURN 1			
		END IF
		
	CASE "espe_codigo"
		  IF Not iuo_especies.existe(Integer(data),TRUE,sqlca) THEN
			  This.SetItem(row, "espe_codigo", Integer(ls_Nula))
			  RETURN 1
			ELSE
				dw_1.GetChild("vari_codigo", idwc_variedad)
				idwc_variedad.SetTransObject(sqlca)
				idwc_variedad.Retrieve(Integer(data))
				idwc_variedad.SetSort("vari_nombre A")
				idwc_variedad.Sort()
			  END IF	
		  
	CASE "vari_codigo"
		  IF Not iuo_variedades.existe(dw_1.Object.espe_codigo[Row],Integer(data),TRUE,sqlca) THEN
			  This.SetItem(row, "vari_codigo", Integer(ls_Nula))
			  RETURN 1
	     END IF			

	CASE "siri_codigo"
		IF NOT iuo_riegos.existe(Integer(data),True,Sqlca) THEN
			This.SetItem(il_fila,"siri_codigo", Integer(ls_Nula))
			This.SetFocus()
			RETURN 1
		END IF		
	
   CASE "sico_codigo"
		IF NOT iuo_conduccion.existe(Integer(data),True,Sqlca) THEN
			This.SetItem(il_fila,"sico_codigo", Integer(ls_Nula))
			This.SetFocus()
			RETURN 1
		END IF
		
   CASE  "prcc_cjtem1","prcc_cjtem2","prcc_cjtem3","prcc_cjtem4","prcc_cjtem5" 		  
		  	IF Dec(Data) > 999999 OR Dec(Data) < 0 THEN
				This.SetItem(Row, ls_Campo, Dec(ls_Nula))
				RETURN 1
			END IF
	END CHOOSE
end event

type tab_cuartel from tab within w_mant_deta_cuarteles
integer x = 37
integer y = 84
integer width = 2711
integer height = 1092
integer taborder = 30
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long backcolor = 16777215
boolean raggedright = true
boolean focusonbuttondown = true
boolean boldselectedtext = true
integer selectedtab = 1
tabpage_1 tabpage_1
tabpage_2 tabpage_2
tabpage_3 tabpage_3
tabpage_4 tabpage_4
tabpage_5 tabpage_5
end type

on tab_cuartel.create
this.tabpage_1=create tabpage_1
this.tabpage_2=create tabpage_2
this.tabpage_3=create tabpage_3
this.tabpage_4=create tabpage_4
this.tabpage_5=create tabpage_5
this.Control[]={this.tabpage_1,&
this.tabpage_2,&
this.tabpage_3,&
this.tabpage_4,&
this.tabpage_5}
end on

on tab_cuartel.destroy
destroy(this.tabpage_1)
destroy(this.tabpage_2)
destroy(this.tabpage_3)
destroy(this.tabpage_4)
destroy(this.tabpage_5)
end on

type tabpage_1 from userobject within tab_cuartel
integer x = 18
integer y = 176
integer width = 2674
integer height = 900
long backcolor = 16777215
string text = "Cuartel"
long tabtextcolor = 33554432
long tabbackcolor = 12639424
long picturemaskcolor = 536870912
dw_cuartel dw_cuartel
end type

on tabpage_1.create
this.dw_cuartel=create dw_cuartel
this.Control[]={this.dw_cuartel}
end on

on tabpage_1.destroy
destroy(this.dw_cuartel)
end on

type dw_cuartel from uo_dw within tabpage_1
integer x = 55
integer y = 48
integer width = 2555
integer height = 840
integer taborder = 11
string dataobject = "dw_mant_cuarteles_general"
boolean vscrollbar = false
boolean border = false
end type

event itemchanged;call super::itemchanged;Integer	li_Semana, li_Temporada
Long		ll_fila
String		ls_Columna, ls_busca,ls_Null

ls_Columna = dwo.Name
SetNull(ls_Null)

Choose Case ls_Columna			
	Case "prcc_codigo"
		ls_busca = Data
		ll_fila = This.Find("prcc_codigo = " + ls_busca, 1, This.RowCount())
		If ll_fila > 0 and ll_fila <> il_fila Then
			MessageBox("Error","Código de Cuartel ya fue ingresado anteriormente",Information!, Ok!)
			This.SetItem(il_fila, "prcc_codigo", Integer(ias_campo[1]))
			Return 1			
		End If

	Case "espe_codigo"
		  If Not iuo_especies.existe(Integer(data),TRUE,sqlca) Then
			  This.SetItem(row, ls_Columna, Integer(ls_Null))
			  Return 1
			Else
				dw_1.GetChild("vari_codigo", idwc_variedad)
				idwc_variedad.SetTransObject(sqlca)
				idwc_variedad.Retrieve(Integer(data))
				idwc_variedad.SetSort("vari_nombre A")
				idwc_variedad.Sort()
		  End If	
		  
	Case "vari_codigo"
		  If Not iuo_variedades.existe(dw_1.Object.espe_codigo[Row],Integer(data),TRUE,sqlca) Then
			  This.SetItem(row, ls_Columna, Integer(ls_Null))
			  Return 1
	     End If			

	Case "siri_codigo"
		If NOT iuo_riegos.existe(Integer(data),True,Sqlca) Then
			This.SetItem(il_fila,ls_Columna, Integer(ls_Null))
			This.SetFocus()
			Return 1
		End If		
	
   Case "sico_codigo"
		If NOT iuo_conduccion.existe(Integer(data),True,Sqlca) Then
			This.SetItem(il_fila,ls_Columna, Integer(ls_Null))
			This.SetFocus()
			Return 1
		End If
		
	Case 'prcc_semini'
		If Not IsNull(gstr_paramtempo.Temporada) And gstr_paramtempo.Temporada <> 0 Then li_Temporada = gstr_paramtempo.Temporada
		If Not IsNull(gstr_tempo.Temporada) And gstr_tempo.Temporada <> 0 Then li_Temporada = gstr_tempo.Temporada
		If iuo_Semana.Semana(Integer(Data), li_Temporada, This.Object.espe_codigo[Row]) Then				
			li_Semana	=	F_NroSemanaAno(iuo_Semana.Lunes)
			If Integer(data) = 53 Then
				If 	li_Semana = 52 Then
					This.SetItem(Row, ls_Columna, Integer(ls_Null))
					MessageBox('Alerta', 'Este año no posee semana 53.', Exclamation!, OK!)
					Return 1
				End If
			End If
			This.Object.prcc_fecini[Row] = iuo_Semana.Lunes
		End If		
		
End Choose
end event

event dwnkey;call super::dwnkey;If key = KeyTab! Then
	If 'prcc_encarg' = Trim(This.GetColumnName()) Then
		This.SetColumn('prcc_codigo') 
	End If
End IF


end event

type tabpage_2 from userobject within tab_cuartel
integer x = 18
integer y = 176
integer width = 2674
integer height = 900
long backcolor = 16777215
string text = "Plantas"
long tabtextcolor = 33554432
long tabbackcolor = 12639424
long picturemaskcolor = 536870912
dw_plantas dw_plantas
end type

on tabpage_2.create
this.dw_plantas=create dw_plantas
this.Control[]={this.dw_plantas}
end on

on tabpage_2.destroy
destroy(this.dw_plantas)
end on

type dw_plantas from uo_dw within tabpage_2
integer x = 55
integer y = 48
integer width = 2533
integer height = 892
integer taborder = 11
string dataobject = "dw_mant_cuarteles_plantas"
boolean vscrollbar = false
boolean border = false
end type

event itemchanged;call super::itemchanged;Long	ll_fila
String	ls_campo

ls_campo = dwo.Name

//wf_calcula_Superficie(Data, ls_Campo)
end event

event dwnkey;call super::dwnkey;If key = KeyTab! Then
	If 'prcc_gothec' = Trim(This.GetColumnName()) Then
		This.SetColumn('prcc_ptasob') 
	End If
End IF

end event

type tabpage_3 from userobject within tab_cuartel
integer x = 18
integer y = 176
integer width = 2674
integer height = 900
long backcolor = 16777215
string text = "Cajas"
long tabtextcolor = 33554432
long tabbackcolor = 12639424
long picturemaskcolor = 536870912
dw_cajas dw_cajas
end type

on tabpage_3.create
this.dw_cajas=create dw_cajas
this.Control[]={this.dw_cajas}
end on

on tabpage_3.destroy
destroy(this.dw_cajas)
end on

type dw_cajas from uo_dw within tabpage_3
integer x = 55
integer y = 48
integer width = 2551
integer height = 580
integer taborder = 11
string dataobject = "dw_mant_cuarteles_cajas"
boolean vscrollbar = false
boolean border = false
end type

event itemchanged;call super::itemchanged;Long		ll_fila
String	ls_campo, ls_busca,ls_Nula

ls_campo = dwo.Name
SetNull(ls_Nula)

Choose Case ls_campo
	Case  "prcc_cjtem1","prcc_cjtem2","prcc_cjtem3","prcc_cjtem4","prcc_cjtem5" 		  
		  	IF Dec(Data) > 999999 OR Dec(Data) < 0 THEN
				This.SetItem(Row, ls_Campo, Dec(ls_Nula))
				Return 1
			END IF
End Choose 
end event

event dwnkey;call super::dwnkey;If key = KeyTab! Then
	If 'prcc_cajcuar' = Trim(This.GetColumnName()) Then
		This.SetColumn('prcc_cjtem1') 
	End If
End IF

end event

type tabpage_4 from userobject within tab_cuartel
integer x = 18
integer y = 176
integer width = 2674
integer height = 900
long backcolor = 16777215
string text = "Observaciones"
long tabtextcolor = 33554432
long tabbackcolor = 12639424
long picturemaskcolor = 536870912
dw_observa dw_observa
end type

on tabpage_4.create
this.dw_observa=create dw_observa
this.Control[]={this.dw_observa}
end on

on tabpage_4.destroy
destroy(this.dw_observa)
end on

type dw_observa from uo_dw within tabpage_4
integer x = 55
integer y = 48
integer width = 2537
integer height = 624
integer taborder = 10
string dataobject = "dw_mant_cuarteles_observciones"
boolean vscrollbar = false
boolean border = false
end type

event dwnkey;call super::dwnkey;If key = KeyTab! Then
	If 'prcc_obsman' = Trim(This.GetColumnName()) Then
		This.SetColumn('prcc_observ') 
	End If
End IF

end event

type tabpage_5 from userobject within tab_cuartel
integer x = 18
integer y = 176
integer width = 2674
integer height = 900
long backcolor = 16777215
string text = "Variedades~r~nRoyalty"
long tabtextcolor = 33554432
long tabbackcolor = 12639424
long picturemaskcolor = 536870912
dw_variedad dw_variedad
pb_elimina pb_elimina
pb_inserta pb_inserta
end type

on tabpage_5.create
this.dw_variedad=create dw_variedad
this.pb_elimina=create pb_elimina
this.pb_inserta=create pb_inserta
this.Control[]={this.dw_variedad,&
this.pb_elimina,&
this.pb_inserta}
end on

on tabpage_5.destroy
destroy(this.dw_variedad)
destroy(this.pb_elimina)
destroy(this.pb_inserta)
end on

type dw_variedad from uo_dw within tabpage_5
integer x = 55
integer y = 48
integer width = 1815
integer height = 864
integer taborder = 11
string dataobject = "dw_mues_cuartelvariedad"
boolean border = false
end type

event itemchanged;call super::itemchanged;Long		ll_fila
String		ls_Columna,ls_Nula

ls_Columna = dwo.Name
SetNull(ls_Nula)

Choose Case ls_Columna			
	Case "vari_codigo"
		  If Not iuo_Variedades2.existe(This.Object.espe_codigo[Row], Integer(data), True, sqlca) Then
			  This.SetItem(row, ls_Columna, Integer(ls_Nula))
			  Return 1
	     End If
		  
End Choose
end event

type pb_elimina from picturebutton within tabpage_5
integer x = 2309
integer y = 452
integer width = 302
integer height = 244
integer taborder = 40
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string picturename = "\Desarrollo 17\Imagenes\Botones\Signo Menos.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\Signo Menos-bn.png"
alignment htextalign = left!
long backcolor = 553648127
end type

event clicked;IF dw_Variedad.rowcount() < 1 THEN RETURN

SetPointer(HourGlass!)

ib_borrar = True
w_main.SetMicroHelp("Validando la eliminación...")

IF MessageBox("Eliminación de Registro", "Está seguro de Eliminar este Registro", Question!, YesNo!) = 1 THEN
	IF dw_Variedad.DeleteRow(0) = 1 THEN
		ib_borrar = False
		w_main.SetMicroHelp("Borrando Registro...")
		SetPointer(Arrow!)
	ELSE
		ib_borrar = False
	END IF

 IF dw_Variedad.RowCount() = 0 THEN
		This.Enabled = False
	ELSE
		il_fila = dw_Variedad.GetRow()
	END IF
END IF
end event

type pb_inserta from picturebutton within tabpage_5
integer x = 2309
integer y = 180
integer width = 302
integer height = 244
integer taborder = 30
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string picturename = "\Desarrollo 17\Imagenes\Botones\Signo Mas.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\Signo Mas-bn.png"
alignment htextalign = left!
long backcolor = 553648127
end type

event clicked;Long ll_Fila

If dw_Variedad.RowCount() > 0 Then This.Enabled = False

ll_Fila = dw_Variedad.InsertRow(0)

dw_Variedad.Object.prod_codigo[ll_Fila] = dw_1.Object.prod_codigo[il_Fila]
dw_Variedad.Object.prpr_codigo[ll_Fila] = dw_1.Object.prpr_codigo[il_Fila]
dw_Variedad.Object.prcc_codigo[ll_Fila] = dw_1.Object.prcc_codigo[il_Fila]
dw_Variedad.Object.espe_codigo[ll_Fila] = dw_1.Object.espe_codigo[il_Fila]

dw_Variedad.ScrollToRow(ll_Fila)
dw_Variedad.SetRow(ll_Fila)
dw_Variedad.SetFocus()
dw_Variedad.SetColumn(1)
end event

type dw_2 from uo_dw within w_mant_deta_cuarteles
boolean visible = false
integer x = 2935
integer y = 916
integer width = 274
integer height = 204
integer taborder = 11
boolean bringtotop = true
string dataobject = "dw_mues_cuartelvariedad"
end type

