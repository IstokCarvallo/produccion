$PBExportHeader$w_mant_deta_paramtemporada.srw
$PBExportComments$Ventana de Detalle de Temporadas.
forward
global type w_mant_deta_paramtemporada from w_mant_detalle_csd
end type
end forward

global type w_mant_deta_paramtemporada from w_mant_detalle_csd
integer width = 3739
integer height = 2204
end type
global w_mant_deta_paramtemporada w_mant_deta_paramtemporada

type variables
String	is_rutrl1,is_rutrl2
end variables

forward prototypes
public function boolean duplicado (string columna)
end prototypes

public function boolean duplicado (string columna);Long		ll_fila

ll_fila	= dw_1.Find("pate_tempor = " + columna , 1,dw_1.RowCount())

IF ll_fila > 0 and ll_fila <> il_fila THEN
	MessageBox("Error","Código de Temporada " + columna + "  ya fue ingresado anteriormente",Information!, Ok!)
	istr_mant.argumento[3]	=	""
	RETURN True
ELSE
	RETURN False
END IF
end function

on w_mant_deta_paramtemporada.create
call super::create
end on

on w_mant_deta_paramtemporada.destroy
call super::destroy
end on

event ue_recuperadatos;call super::ue_recuperadatos;ias_campo[1]	=	String(dw_1.Object.pate_tempor[il_Fila])
ias_campo[2]	=	dw_1.Object.pate_nombre[il_Fila]
ias_campo[3]	=	dw_1.Object.pate_abrevi[il_Fila]
ias_campo[4]	=	String(Date(dw_1.Object.pate_inicio[il_Fila]))
ias_campo[5]	=	String(Date(dw_1.Object.pate_termin[il_Fila]))
ias_campo[6]	=	dw_1.Object.pate_agead1[il_Fila]
ias_campo[7]	=	dw_1.Object.pate_agead2[il_Fila]
ias_campo[8]	=	dw_1.Object.pate_embar1[il_Fila]
ias_campo[9]	=	dw_1.Object.pate_embar2[il_Fila]
ias_campo[10]	=	dw_1.Object.pate_ciaseg[il_Fila]
ias_campo[11]	=	dw_1.Object.pate_firma1[il_Fila]
ias_campo[12]	=	dw_1.Object.pate_firma2[il_Fila]
ias_campo[13]	=	dw_1.Object.pate_firma3[il_Fila]
ias_campo[14]	=	dw_1.Object.pate_repco1[il_Fila]
ias_campo[15]	=	dw_1.Object.pate_rurec1[il_Fila]
ias_campo[16]	=	dw_1.Object.pate_carec1[il_Fila]
ias_campo[17]	=	dw_1.Object.pate_repco2[il_Fila]
ias_campo[18]	=	dw_1.Object.pate_rurec2[il_Fila]
ias_campo[19]	=	dw_1.Object.pate_carec2[il_Fila]
ias_campo[20]	=	String(dw_1.Object.pate_tasain[il_Fila])
ias_campo[21]	=	String(dw_1.Object.pate_financ[il_fila])

IF Not istr_Mant.Agrega AND Not istr_Mant.Borra THEN
	dw_1.Object.pate_tempor.Protect				=	1
	dw_1.Object.pate_tempor.Color	=	RGB(255,255,255)
	dw_1.Object.pate_tempor.BackGround.Color	=	553648127

	is_rutrl1	=	ias_campo[15]
	is_rutrl2	=	ias_campo[18]
ELSE
	dw_1.Object.pate_estado[il_fila]	=	1
	dw_1.Object.pate_vigent[il_fila]	=	0
END IF
end event

event ue_deshace();call super::ue_deshace;IF dw_1.GetItemStatus(il_Fila, 0, Primary!) = DataModified! THEN
	dw_1.SetItem(il_fila, "pate_tempor", Integer(ias_campo[1]))
	dw_1.SetItem(il_fila, "pate_nombre", ias_campo[2])
	dw_1.SetItem(il_fila, "pate_abrevi", ias_campo[3])
	dw_1.SetItem(il_fila, "pate_inicio", DateTime(Date(ias_campo[4])))
	dw_1.SetItem(il_fila, "pate_termin", DateTime(Date(ias_campo[5])))
	dw_1.SetItem(il_fila, "pate_agead1", ias_campo[6])
	dw_1.SetItem(il_fila, "pate_agead2", ias_campo[7])
	dw_1.SetItem(il_fila, "pate_embar1", ias_campo[8])
	dw_1.SetItem(il_fila, "pate_embar2", ias_campo[9])
	dw_1.SetItem(il_fila, "pate_ciaseg", ias_campo[10])
	dw_1.SetItem(il_fila, "pate_firma1", ias_campo[11])
	dw_1.SetItem(il_fila, "pate_firma2", ias_campo[12])
	dw_1.SetItem(il_fila, "pate_firma3", ias_campo[13])
	dw_1.SetItem(il_fila, "pate_repco1", ias_campo[14])
	dw_1.SetItem(il_fila, "pate_rurec1", ias_campo[15])
	dw_1.SetItem(il_fila, "pate_carec1", ias_campo[16])
	dw_1.setItem(il_fila, "pate_repco2", ias_campo[17])
	dw_1.SetItem(il_fila, "pate_rurec2", ias_campo[18])
	dw_1.SetItem(il_fila, "pate_carec2", ias_campo[19])
	dw_1.SetItem(il_fila, "pate_tasain", Dec(ias_campo[20]))
	dw_1.Object.pate_financ[il_fila]	=	Dec(ias_campo[21])
END IF

end event

event ue_antesguardar();Integer	li_cont
String	ls_mensaje, ls_colu[]

IF Isnull(dw_1.GetItemNumber(il_fila, "pate_tempor")) OR dw_1.GetItemNumber(il_fila, "pate_tempor") = 0 THEN
	li_cont ++
	ls_mensaje 			= ls_mensaje + "~nCódigo de Temporada"
	ls_colu[li_cont]	= "pate_tempor"
END IF

IF Isnull(dw_1.GetItemString(il_fila, "pate_nombre")) OR dw_1.GetItemString(il_fila, "pate_nombre") = "" THEN
	li_cont ++
	ls_mensaje			= ls_mensaje + "~nNombre de Temporada"
	ls_colu[li_cont]	= "pate_nombre"
END IF

IF Isnull(dw_1.GetItemString(il_fila, "pate_abrevi")) OR dw_1.GetItemString(il_fila, "pate_abrevi") = "" THEN
	li_cont ++
	ls_mensaje			= ls_mensaje + "~nAbreviacion de Temporada"
	ls_colu[li_cont]	= "pate_abrevi"
END IF

IF Isnull(dw_1.Object.pate_inicio[il_fila]) THEN
	li_cont ++
	ls_mensaje			= ls_mensaje + "~nFecha de Inicio Temporada"
	ls_colu[li_cont]	= "pate_inicio"
END IF

IF Isnull(dw_1.Object.pate_termin[il_fila]) THEN
	li_cont ++
	ls_mensaje			= ls_mensaje + "~nFecha de Termino Temporada"
	ls_colu[li_cont]	= "pate_termin"
END IF

IF Isnull(dw_1.GetItemNumber(il_fila, "pate_tasain")) OR dw_1.GetItemNumber(il_fila, "pate_tasain") = 0 THEN
	li_cont ++
	ls_mensaje			= ls_mensaje + "~nTasa de Interes"
	ls_colu[li_cont]	= "pate_tasain"
END IF

IF li_cont > 0 THEN
	MessageBox("Error de Consistencia", "Falta el ingreso de :" + ls_mensaje + ".", StopSign!, Ok!)
	dw_1.SetColumn(ls_colu[1])
	dw_1.SetFocus()
	Message.DoubleParm = -1
	RETURN 
ELSE
	IF NOT dw_1.uf_validate(il_fila) THEN
		dw_1.SetFocus()
		Message.DoubleParm = -1
		RETURN
	END IF		
END IF

is_rutrl1	= ''
is_rutrl2	= ''
end event

event open;x	= 40
y	= 200
PostEvent("ue_recuperadatos")

istr_mant	=	Message.PowerObjectParm

dw_1.SetTransObject(sqlca)

istr_mant.dw.ShareData(dw_1)
end event

event ue_nuevo();call super::ue_nuevo;dw_1.Object.pate_estado[il_fila]	=	1
dw_1.Object.pate_vigent[il_fila]	=	0
end event

type pb_ultimo from w_mant_detalle_csd`pb_ultimo within w_mant_deta_paramtemporada
end type

type pb_siguiente from w_mant_detalle_csd`pb_siguiente within w_mant_deta_paramtemporada
end type

type pb_anterior from w_mant_detalle_csd`pb_anterior within w_mant_deta_paramtemporada
end type

type pb_primero from w_mant_detalle_csd`pb_primero within w_mant_deta_paramtemporada
end type

type pb_cancela from w_mant_detalle_csd`pb_cancela within w_mant_deta_paramtemporada
integer x = 3264
integer y = 552
end type

type pb_acepta from w_mant_detalle_csd`pb_acepta within w_mant_deta_paramtemporada
integer x = 3264
integer y = 272
end type

type pb_salir from w_mant_detalle_csd`pb_salir within w_mant_deta_paramtemporada
integer x = 3259
integer y = 848
end type

type dw_1 from w_mant_detalle_csd`dw_1 within w_mant_deta_paramtemporada
integer x = 91
integer width = 3049
integer height = 1892
string dataobject = "dw_mant_paramtemporada"
end type

event dw_1::itemchanged;String	ls_columna, ls_null
Integer	li_null
Date		ld_null, ld_fecha

ls_columna = GetColumnName()

SetNull(ls_null)
SetNull(li_null)
SetNull(ld_null)

CHOOSE CASE ls_columna
			
	CASE "pate_tempor"	
		IF Duplicado(data) THEN
			This.SetItem(il_fila, "pate_tempor", li_null)
			RETURN 1
		END IF	
		
	CASE "pate_inicio"
		ld_fecha	=	Date(This.Object.pate_termin[row])
		IF NOT Isnull(ld_fecha) THEN
			IF Date(Mid(data,1,10)) > ld_fecha THEN
				MessageBox("Atención","La Fecha de Término debe ser Superior a la Fecha de Inicio",Exclamation!)
				This.SetItem(il_fila, "pate_termin", ld_null)
			END IF					
		END IF
		
	CASE "pate_termin"
		ld_fecha	=	Date(This.Object.pate_inicio[row])
		
		IF NOT Isnull(ld_fecha) THEN
			IF Date(Mid(data,1,10)) < ld_fecha THEN
				MessageBox("Atención","La Fecha de Término debe ser Superior a la Fecha de Inicio",Exclamation!)
				This.SetItem(il_fila, "pate_termin", ld_null)
			END IF					
		END IF

	CASE "pate_rurec1"
		is_rutrl1 = F_verrut(data, True)
		
		IF is_rutrl1 = ""  THEN
			This.SetItem(1, "pate_rurec1", ls_Null)
			Return 1
		END IF
		
	CASE "pate_rurec2"
		is_rutrl2 = F_verrut(data, True)
		
		IF is_rutrl2 = ""  THEN
			This.SetItem(1, "pate_rurec2", ls_Null)
			RETURN 1
		END IF

END CHOOSE
end event

event dw_1::itemfocuschanged;IF is_rutrl1 <> "" THEN
	IF dwo.Name = "pate_rurec1" THEN
		This.Object.pate_rurec1.EditMask.Mask	=	"XXXXXXXXXX"
		
		IF is_rutrl1 <> "" THEN
			This.SetItem(il_fila, "pate_rurec1", String(Double(Mid(is_rutrl1, 1, 9)), "#########") + Mid(is_rutrl1, 10))
		END IF
	ELSE
		This.Object.pate_rurec1.EditMask.Mask	=	"###.###.###-!"
		This.SetItem(il_fila, "pate_rurec1", is_rutrl1)
	END IF
END IF

IF is_rutrl2 <> "" THEN
	IF dwo.Name = "pate_rurec2" THEN
		This.Object.pate_rurec2.EditMask.Mask	=	"XXXXXXXXXX"

		IF is_rutrl2 <> "" THEN
			This.SetItem(il_fila, "pate_rurec2", String(Double(Mid(is_rutrl2, 1, 9)), "#########") + Mid(is_rutrl2, 10))
		END IF
	ELSE
		This.Object.pate_rurec2.EditMask.Mask	=	"###.###.###-!"
		This.SetItem(il_fila, "pate_rurec2", is_rutrl2)
	END IF
END IF
end event

event dw_1::losefocus;call super::losefocus;is_rutrl1	=	String(Double(Mid(is_rutrl1, 1, Len(is_rutrl1) - 1)), "000000000") + &
					Mid(is_rutrl1, Len(is_rutrl1))
is_rutrl2	=	String(Double(Mid(is_rutrl2, 1, Len(is_rutrl2) - 1)), "000000000") + &
					Mid(is_rutrl2, Len(is_rutrl2))

dw_1.SetItem(il_fila, "pate_rurec1", is_rutrl1)
dw_1.SetItem(il_fila, "pate_rurec2", is_rutrl2)
end event

event dw_1::constructor;call super::constructor;This.uf_add_validation('pate_financ >= 0 and pate_financ <= 100','Valor fuera de Rango para Porcentaje de Financiamiento')
This.uf_add_validation('pate_tasain >= 0 and pate_tasain <= 100','Valor fuera de Rango para Tasa de Interés')

end event

