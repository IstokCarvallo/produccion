$PBExportHeader$w_mant_deta_prodpredio.srw
forward
global type w_mant_deta_prodpredio from w_mant_detalle_csd
end type
end forward

global type w_mant_deta_prodpredio from w_mant_detalle_csd
integer x = 123
integer y = 96
integer width = 3323
integer height = 1364
string title = "PREDIOS"
end type
global w_mant_deta_prodpredio w_mant_deta_prodpredio

type variables
String is_rut, is_codigo
Integer ii_zonas

uo_Zonas	iuo_Zonas
uo_agronomo  iuo_agronomo
uo_provincias  iuo_provincias

DataWindowChild  dw_zona, idwc_agronomos, idwc_comunas, idwc_provincias, idwc_regiones
end variables

forward prototypes
public function boolean existeprovincia (integer ai_provincia, integer ai_region)
end prototypes

public function boolean existeprovincia (integer ai_provincia, integer ai_region);Integer li_region, li_provincia
String   ls_Nombre
Boolean	lb_Retorno = True

SELECT	regi_codigo, prov_codigo, prov_nombre
	INTO	:li_region, :li_provincia, :ls_Nombre
	FROM	dba.provincias
  WHERE	regi_codigo	=	:ai_region
  AND    prov_codigo =  :ai_provincia;
  
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla de Provincias")
	
	lb_Retorno	=	False
ELSEIF sqlca.SQLCode=100 THEN
	MessageBox("Atención","Codigo de Provincia No ha sido creado. Ingrese Otro.")
	lb_Retorno	=	False
END IF

RETURN lb_Retorno
end function

on w_mant_deta_prodpredio.create
call super::create
end on

on w_mant_deta_prodpredio.destroy
call super::destroy
end on

event ue_recuperadatos;call super::ue_recuperadatos;ias_campo[1] = String(dw_1.GetItemNumber(il_fila, "prpr_codigo"))
ias_campo[2] = String(dw_1.GetItemNumber(il_fila, "zona_codigo"))
ias_campo[3] = String(dw_1.GetItemNumber(il_fila, "ccag_codigo"))
ias_campo[4] = String(dw_1.GetItemNumber(il_fila, "regi_codigo"))
ias_campo[5] = String(dw_1.GetItemNumber(il_fila, "prov_codigo"))
ias_campo[6] = String(dw_1.GetItemNumber(il_fila, "comu_codigo"))
ias_campo[7] = dw_1.GetItemString(il_fila, "prpr_nombre")
ias_campo[8] = dw_1.GetItemString(il_fila, "prpr_direcc")
ias_campo[9] = dw_1.GetItemString(il_fila, "prpr_nrorol")
ias_campo[10] = dw_1.GetItemString(il_fila, "prpr_nrfoja")
ias_campo[11] = dw_1.GetItemString(il_fila, "prpr_numero")
ias_campo[12] = dw_1.GetItemString(il_fila, "prpr_conser")
ias_campo[13] = String(dw_1.GetItemNumber(il_fila, "prpr_superf"))
ias_campo[14] = dw_1.GetItemString(il_fila, "prpr_rutrep")
ias_campo[15] = dw_1.GetItemString(il_fila, "prpr_nomrep")
ias_campo[16] = dw_1.GetItemString(il_fila, "prpr_admini")
ias_campo[17] = dw_1.GetItemString(il_fila, "prpr_nomadm")
ias_campo[18] = dw_1.GetItemString(il_fila, "prpr_colum1")
ias_campo[19] = dw_1.GetItemString(il_fila, "prpr_colum2")
ias_campo[20] = dw_1.GetItemString(il_fila, "prpr_colum3")
ias_campo[21] = dw_1.GetItemString(il_fila, "prpr_colum4")
ias_campo[22] = dw_1.GetItemString(il_fila, "prpr_colum5")
ias_campo[23] = String(dw_1.GetItemDate(il_fila, "prpr_fecins"))

IF istr_mant.agrega = False and istr_mant.borra = False THEN
		dw_1.SetTabOrder("prpr_codigo", 0)
		dw_1.SetTabOrder("prpr_nombre", 0)
		dw_1.Modify("prpr_codigo.BackGround.Color = " + String(RGB(192,220,192)))
		dw_1.Modify("prpr_nombre.BackGround.Color = " + String(RGB(192,220,192)))
END IF




end event

event ue_deshace;call super::ue_deshace;IF UpperBound(ias_campo) > 0 THEN
	dw_1.SetItem(il_fila, "prpr_codigo", Integer(ias_campo[1]))
	dw_1.SetItem(il_fila, "zona_codigo", Integer(ias_campo[2]))
	dw_1.SetItem(il_fila, "ccag_codigo", Integer(ias_campo[3]))
	dw_1.SetItem(il_fila, "regi_codigo", Integer(ias_campo[4]))
	dw_1.SetItem(il_fila, "prov_codigo", Integer(ias_campo[5]))
	dw_1.SetItem(il_fila, "comu_codigo", Integer(ias_campo[6]))
	dw_1.SetItem(il_fila, "prpr_nombre", ias_campo[7])
	dw_1.SetItem(il_fila, "prpr_direcc", ias_campo[8])
	dw_1.SetItem(il_fila, "prpr_nrorol", ias_campo[9])
	dw_1.SetItem(il_fila, "prpr_nrfoja", ias_campo[10])
	dw_1.SetItem(il_fila, "prpr_numero", ias_campo[11])
	dw_1.SetItem(il_fila, "prpr_conser", ias_campo[12])
	dw_1.SetItem(il_fila, "prpr_superf", Integer(ias_campo[13]))	
	dw_1.SetItem(il_fila, "prpr_rutrep", ias_campo[14])
	dw_1.SetItem(il_fila, "prpr_nomrep", ias_campo[15])
	dw_1.SetItem(il_fila, "prpr_admini", ias_campo[16])
	dw_1.SetItem(il_fila, "prpr_nomadm", ias_campo[17])
	dw_1.SetItem(il_fila, "prpr_colum1", ias_campo[18])
	dw_1.SetItem(il_fila, "prpr_colum2", ias_campo[19])
	dw_1.SetItem(il_fila, "prpr_colum3", ias_campo[20])
	dw_1.SetItem(il_fila, "prpr_colum4", ias_campo[21])
	dw_1.SetItem(il_fila, "prpr_colum5", ias_campo[22])
	dw_1.SetItem(il_fila, "prpr_fecins", date(ias_campo[23]))	
END IF
end event

event ue_antesguardar;call super::ue_antesguardar;String	ls_mensaje, ls_colu[]
Long		li_cont

IF Isnull(dw_1.Object.prpr_codigo[il_fila]) OR dw_1.Object.prpr_codigo[il_fila] = 0 THEN
	li_cont	++
	ls_mensaje 			= ls_mensaje + "~nCódigo Predio"
	ls_colu[li_cont]	= "prpr_codigo"
END IF

IF Isnull(dw_1.Object.zona_codigo[il_fila]) OR dw_1.Object.zona_codigo[il_fila] = 0 THEN
	li_cont	++
	ls_mensaje 			= ls_mensaje + "~nCódigo Zona"
	ls_colu[li_cont]	= "zona_codigo"
END IF

IF Isnull(dw_1.Object.ccag_codigo[il_fila]) OR dw_1.Object.ccag_codigo[il_fila] = 0 THEN
	li_cont	++
	ls_mensaje 			= ls_mensaje + "~nCódigo Agronomo"
	ls_colu[li_cont]	= "ccag_codigo"
END IF

IF Isnull(dw_1.Object.regi_codigo[il_fila]) OR dw_1.Object.regi_codigo[il_fila] = 0 THEN
	li_cont	++
	ls_mensaje 			= ls_mensaje + "~nCódigo Región"
	ls_colu[li_cont]	= "regi_codigo"
END IF

IF Isnull(dw_1.Object.prov_codigo[il_fila]) OR dw_1.Object.prov_codigo[il_fila] = 0 THEN
	li_cont	++
	ls_mensaje 			= ls_mensaje + "~nCódigo Provincia"
	ls_colu[li_cont]	= "prov_codigo"
END IF

IF Isnull(dw_1.Object.comu_codigo[il_fila]) OR dw_1.Object.comu_codigo[il_fila] = 0 THEN
	li_cont	++
	ls_mensaje 			= ls_mensaje + "~nCódigo Comuna"
	ls_colu[li_cont]	= "comu_codigo"
END IF

IF Isnull(dw_1.Object.prpr_superf[il_fila]) OR dw_1.Object.prpr_superf[il_fila] = 0 THEN
	li_cont	++
	ls_mensaje 			= ls_mensaje + "~nSuperficie"
	ls_colu[li_cont]	= "prpr_superf"
END IF

IF Isnull(dw_1.Object.prpr_nombre[il_fila]) OR dw_1.Object.prpr_nombre[il_fila] = "" THEN
	li_cont	++
	ls_mensaje 			= ls_mensaje + "~nNombre Predio"
	ls_colu[li_cont]	= "prpr_nombre"
END IF

IF Isnull(dw_1.Object.prpr_direcc[il_fila]) OR dw_1.Object.prpr_direcc[il_fila] = "" THEN
	li_cont	++
	ls_mensaje 			= ls_mensaje + "~nDirección"
	ls_colu[li_cont]	= "prpr_direcc"
END IF

IF Isnull(dw_1.Object.prpr_nrorol[il_fila]) OR dw_1.Object.prpr_nrorol[il_fila] = "" THEN
	li_cont	++
	ls_mensaje 			= ls_mensaje + "~nNumero Rol"
	ls_colu[li_cont]	= "prpr_nrorol"
END IF

IF Isnull(dw_1.Object.prpr_nrfoja[il_fila]) OR dw_1.Object.prpr_nrfoja[il_fila] = "" THEN
	li_cont	++
	ls_mensaje 			= ls_mensaje + "~nNúmero Foja"
	ls_colu[li_cont]	= "prpr_nrfoja"
END IF

IF Isnull(dw_1.Object.prpr_numero[il_fila]) OR dw_1.Object.prpr_numero[il_fila] = "" THEN
	li_cont	++
	ls_mensaje 			= ls_mensaje + "~nNúmero"
	ls_colu[li_cont]	= "prpr_numero"
END IF

IF Isnull(dw_1.Object.prpr_conser[il_fila]) OR dw_1.Object.prpr_conser[il_fila] = "" THEN
	li_cont	++
	ls_mensaje 			= ls_mensaje + "~nConservador"
	ls_colu[li_cont]	= "prpr_conser"
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
//istr_mant.dw.ShareData(dw_1)

iuo_Zonas		=	Create	uo_Zonas
iuo_agronomo	=  Create   uo_agronomo
iuo_provincias =	Create	uo_provincias

dw_1.GetChild("zona_codigo", dw_zona)
dw_zona.SetTransObject(sqlca)
dw_zona.Retrieve()

dw_1.GetChild("ccag_codigo", idwc_agronomos)
idwc_agronomos.SetTransObject(sqlca)
idwc_agronomos.Retrieve(0)
idwc_agronomos.SetSort("ccag_nombre A")
idwc_agronomos.Sort()
//idwc_agronomos.InsertRow(0)

dw_1.GetChild("regi_codigo", idwc_regiones)
idwc_regiones.SetTransObject(sqlca)
idwc_regiones.Retrieve(0)

dw_1.GetChild("prov_codigo", idwc_provincias)
idwc_provincias.SetTransObject(sqlca)
idwc_provincias.Retrieve(0)

dw_1.GetChild("comu_codigo", idwc_comunas)
idwc_comunas.SetTransObject(sqlca)
idwc_comunas.Retrieve()
end event

event ue_nuevo;call super::ue_nuevo;is_codigo	=	""
Is_Rut		=	""
end event

type pb_ultimo from w_mant_detalle_csd`pb_ultimo within w_mant_deta_prodpredio
end type

type pb_siguiente from w_mant_detalle_csd`pb_siguiente within w_mant_deta_prodpredio
end type

type pb_anterior from w_mant_detalle_csd`pb_anterior within w_mant_deta_prodpredio
end type

type pb_primero from w_mant_detalle_csd`pb_primero within w_mant_deta_prodpredio
end type

type pb_cancela from w_mant_detalle_csd`pb_cancela within w_mant_deta_prodpredio
integer x = 3022
integer y = 344
end type

type pb_acepta from w_mant_detalle_csd`pb_acepta within w_mant_deta_prodpredio
integer x = 3017
integer y = 128
end type

type pb_salir from w_mant_detalle_csd`pb_salir within w_mant_deta_prodpredio
integer x = 3017
integer y = 560
end type

type dw_1 from w_mant_detalle_csd`dw_1 within w_mant_deta_prodpredio
integer x = 0
integer width = 2734
integer height = 1152
string dataobject = "dw_mant_prodpredio"
end type

event dw_1::itemchanged;Long		ll_fila
Integer	li_Zona
String	ls_campo, ls_busca, ls_Nula

ls_campo = GetColumnName()

SetNull(ls_Nula)

CHOOSE CASE ls_campo
	CASE "prpr_codigo"
		ls_busca = Data
		ll_fila = This.Find("prpr_codigo = " + ls_busca, 1, This.RowCount())
		IF ll_fila > 0 and ll_fila <> il_fila THEN
			MessageBox("Error","Código de Predio ya fue ingresado anteriormente",Information!, Ok!)
			This.SetItem(Row, "prpr_codigo", Integer(ias_campo[1]))
			RETURN 1
		END IF
		
		CASE "zona_codigo"
			IF iuo_zonas.existe(Integer(data),TRUE, sqlca) THEN
				dw_1.GetChild("ccag_codigo", idwc_agronomos)
				idwc_agronomos.Retrieve(Integer(Data))
				dw_1.SetItem(Row, "ccag_codigo", Integer(ls_Nula))
			ELSE
				This.Setitem(Row, "zona_codigo", Integer(ls_Nula))
				RETURN 1
			END IF
		
		CASE "ccag_codigo"
		  IF Not iuo_agronomo.existe(dw_1.Object.zona_codigo[Row],Integer(data),TRUE,sqlca) THEN
			  This.SetItem(Row, "ccag_codigo", Integer(ls_Nula))
			  RETURN 1
	     END IF	
		
		CASE "prpr_rutrep"
				IF Len(Data) > 6 THEN
					is_Rut = Fill("0",10 - Len(data)) + data
				ELSE
					is_Rut = Fill("0",6 - Len(data)) + data
				END IF
				
				is_rut = F_verrut(is_Rut, True)
				IF is_rut = ""  THEN
					This.SetItem(row, "prpr_rutrep", ls_Nula)
					RETURN 1
				END IF
				
		CASE "prpr_admini"
				IF Len(Data) > 6 THEN
					is_codigo = Fill("0",10 - Len(data)) + data
				ELSE
					is_codigo = Fill("0",6 - Len(data)) + data
				END IF
				
				is_codigo = F_verrut(is_codigo, True)
				IF is_codigo = ""  THEN
					This.SetItem(Row, "prpr_admini", ls_Nula)
					RETURN 1
				END IF
				
		CASE "regi_codigo"
				IF Integer(data) <> 0 THEN
					dw_1.GetChild("prov_codigo", idwc_provincias)
					idwc_provincias.Retrieve(Integer(Data))
					dw_1.SetItem(Row, "prov_codigo", Integer(ls_Nula))
				ELSE
					This.Setitem(Row, "regi_codigo", Integer(ls_Nula))
					RETURN 1
				END IF
				
		CASE "prov_codigo"
				IF NOT iuo_provincias.existe(dw_1.Object.regi_codigo[Row],Integer(data),True,Sqlca) THEN
					This.SetItem(Row,"prov_codigo", Integer(ls_Nula))
					This.SetFocus()
					RETURN 1
				END IF		
END CHOOSE

end event

event dw_1::itemfocuschanged;call super::itemfocuschanged;IF is_rut <> "" THEN
	IF dwo.Name = "prpr_rutrep" THEN
		This.Object.prpr_rutrep.EditMask.Mask = "XXXXXXXXXX"

		IF is_rut <> "" THEN
			This.SetItem(il_fila, "prpr_rutrep", String(Double(Mid(is_rut, 1, 9)), "#########") + Mid(is_rut, 10))
		END IF
	ELSE
		This.Object.prpr_rutrep.EditMask.Mask = "###.###.###-!"
		This.SetItem(il_fila, "prpr_rutrep", is_rut)
	END IF
END IF

IF is_codigo <> "" THEN
	IF dwo.Name = "prpr_admini" THEN
		This.Object.prpr_admini.EditMask.Mask = "XXXXXXXXXX"

		IF is_codigo <> "" THEN
			This.SetItem(il_fila, "prpr_admini", String(Double(Mid(is_codigo, 1, 9)), "#########") + Mid(is_codigo, 10))
		END IF
	ELSE
		This.Object.prpr_admini.EditMask.Mask = "###.###.###-!"
		This.SetItem(il_fila, "prpr_admini", is_codigo)
	END IF
END IF
end event

