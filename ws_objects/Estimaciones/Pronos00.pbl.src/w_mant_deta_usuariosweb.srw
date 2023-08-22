$PBExportHeader$w_mant_deta_usuariosweb.srw
forward
global type w_mant_deta_usuariosweb from w_mant_detalle
end type
end forward

global type w_mant_deta_usuariosweb from w_mant_detalle
integer width = 2295
integer height = 1132
string title = "ACCESO USUARIOS WEB"
end type
global w_mant_deta_usuariosweb w_mant_deta_usuariosweb

type variables
uo_Productores		iuo_Productor
uo_ProdPredio		iuo_Predio
end variables

forward prototypes
public function boolean duplicado (string as_columna, string as_valor)
public subroutine wf_buscaproductor ()
public subroutine wf_buscapredios ()
end prototypes

public function boolean duplicado (string as_columna, string as_valor);Long		ll_Fila
String		ls_Productor, ls_Predio

ls_Productor	=	String(dw_1.Object.prod_codigo[il_Fila])
ls_Predio	=	String(dw_1.Object.prpr_codigo[il_Fila])

CHOOSE CASE as_Columna
	CASE "prod_codigo"
		ls_Productor		=	as_Valor
	
	CASE "prpr_codigo"
		ls_Predio		=	as_Valor
	
END CHOOSE

ll_fila	= istr_mant.dw.Find("prod_codigo = " + ls_Productor + " AND prpr_codigo = " + ls_Predio, 1 , istr_mant.dw.RowCount())

IF ll_fila > 0 and ll_fila <> il_FilaAnc THEN 
	MessageBox("Atención","Registro ya fue ingresado anteriormente.",Information!, Ok!)
	RETURN True
ELSE
	RETURN False
END IF
end function

public subroutine wf_buscaproductor ();str_Busqueda	lstr_Busq

OpenWithParm(w_busc_productores, lstr_Busq)

lstr_busq	= Message.PowerObjectParm

If lstr_busq.argum[2] = "" or  Duplicado('prod_codigo', lstr_busq.argum[1]) Then
	dw_1.SetColumn("prod_codigo")
	dw_1.SetFocus()
Else
	iuo_Productor.Codigo					=	Long(lstr_busq.argum[1])
	dw_1.Object.prod_codigo[il_Fila]	=	Long(lstr_busq.argum[1])
	dw_1.Object.prod_nombre[il_Fila]	=	lstr_busq.argum[2]
	dw_1.SetFocus()
End If
end subroutine

public subroutine wf_buscapredios ();str_Busqueda	lstr_Busq

lstr_Busq.Argum[2] = String(iuo_Productor.Codigo)

OpenWithParm(w_busc_prodpredio, lstr_Busq)

lstr_busq	= Message.PowerObjectParm

If lstr_busq.argum[2] = "" or  Duplicado('prpr_codigo', lstr_busq.argum[1]) Then
	dw_1.SetColumn("prpr_codigo")
	dw_1.SetFocus()
Else
	iuo_Predio.Codigo						=	Long(lstr_busq.argum[1])
	dw_1.Object.prpr_codigo[il_Fila]	=	Long(lstr_busq.argum[1])
	dw_1.Object.prpr_nombre[il_Fila]	=	lstr_busq.argum[3]
	dw_1.SetFocus()
End If
end subroutine

on w_mant_deta_usuariosweb.create
call super::create
end on

on w_mant_deta_usuariosweb.destroy
call super::destroy
end on

event ue_recuperadatos;call super::ue_recuperadatos;ias_campo[1] = dw_1.GetItemString(il_fila, "usua_codigo")  
ias_campo[2] = String(dw_1.GetItemNumber(il_fila, "prod_codigo"))  
ias_campo[3] = String(dw_1.GetItemNumber(il_fila, "prpr_codigo"))


If Not istr_mant.Agrega And Not istr_mant.Borra Then
	dw_1.Object.prod_codigo.Protect = 1
	dw_1.Object.prod_codigo.BackGround.Color = 553648127
	dw_1.Object.prod_codigo.Color = RGB(255,255,255)
	
	dw_1.Object.prpr_codigo.Protect = 1
	dw_1.Object.prpr_codigo.BackGround.Color = 553648127
	dw_1.Object.prpr_codigo.Color = RGB(255,255,255)
	
	dw_1.Object.b_productor.Enabled = False
	dw_1.Object.b_predio.Enabled 		= False
Else
	dw_1.Object.usua_codigo[il_fila] = istr_mant.argumento[1]
End If

end event

event ue_deshace;call super::ue_deshace;IF UpperBound(ias_campo) > 0 THEN
	dw_1.SetItem(il_fila, "usua_codigo", ias_campo[1])
	dw_1.SetItem(il_fila, "prod_codigo", Long(ias_campo[2]))
	dw_1.SetItem(il_fila, "prpr_codigo", Integer(ias_campo[3]))
End If
end event

event ue_antesguardar;call super::ue_antesguardar;Integer	li_cont
String	ls_mensaje, ls_calibr, ls_colu[]

IF Isnull(dw_1.GetItemNumber(il_fila, "prod_codigo")) OR dw_1.GetItemNumber(il_fila, "prod_codigo") = 0 THEN
	li_cont ++
	ls_mensaje			= ls_mensaje + "~nCódigo de Productor"
	ls_colu[li_cont]	= "prod_codigo"
END IF

IF IsNull(dw_1.GetItemNumber(il_fila, "prpr_codigo")) OR dw_1.GetItemNumber(il_fila, "prpr_codigo") = 0 THEN 
	li_cont ++
	ls_mensaje			= ls_mensaje + "~nCódigo de Predio"
	ls_colu[li_cont]	= "prpr_codigo"
END IF

IF li_cont > 0 THEN
	MessageBox("Error de Consistencia", "Falta el ingreso de : " + ls_mensaje + ".", StopSign!, Ok!)
	dw_1.SetColumn(ls_colu[1])
	dw_1.SetFocus()
	Message.DoubleParm = -1
END IF
end event

event ue_nuevo;call super::ue_nuevo;dw_1.Object.usua_codigo[il_fila] = istr_mant.argumento[1]
end event

event open;call super::open;iuo_Productor	=	Create uo_Productores	
iuo_Predio		=	Create uo_ProdPredio
end event

type pb_ultimo from w_mant_detalle`pb_ultimo within w_mant_deta_usuariosweb
end type

type pb_siguiente from w_mant_detalle`pb_siguiente within w_mant_deta_usuariosweb
end type

type pb_anterior from w_mant_detalle`pb_anterior within w_mant_deta_usuariosweb
end type

type pb_primero from w_mant_detalle`pb_primero within w_mant_deta_usuariosweb
end type

type pb_cancela from w_mant_detalle`pb_cancela within w_mant_deta_usuariosweb
integer x = 1874
integer y = 500
end type

type pb_acepta from w_mant_detalle`pb_acepta within w_mant_deta_usuariosweb
integer x = 1874
integer y = 284
end type

type pb_salir from w_mant_detalle`pb_salir within w_mant_deta_usuariosweb
integer x = 1870
integer y = 716
end type

type dw_1 from w_mant_detalle`dw_1 within w_mant_deta_usuariosweb
integer y = 104
integer width = 1710
integer height = 876
string dataobject = "dw_mant_usuarioweb"
end type

event dw_1::itemchanged;call super::itemchanged;String		ls_Columna, ls_Null

ls_Columna 	=	dwo.Name
SetNull(ls_null)

Choose Case ls_Columna
	Case "prod_codigo"
		If Not iuo_Productor.Existe(Long(Data), True, Sqlca) Or Duplicado(ls_Columna, Data) Then
			This.SetItem(Row, ls_Columna, Long(ls_Null))
			Return 1
		Else
			This.SetItem(Row, 'prod_nombre', iuo_Productor.Nombre)
		End If
		
	Case "prpr_codigo"
		If Not iuo_Predio.Existe(Integer(Data), iuo_Productor.Codigo, True, Sqlca) Or Duplicado(ls_Columna, Data) Then
			This.SetItem(Row, ls_Columna, Long(ls_Null))
			Return 1
		Else
			This.SetItem(Row, 'prpr_nombre', iuo_Predio.Nombre)
		End If
		
End Choose
end event

event dw_1::buttonclicked;call super::buttonclicked;String		ls_Boton

ls_Boton 	=	dwo.Name

Choose Case ls_Boton
	Case "b_productor"
		wf_BuscaProductor()
		
		
	Case "b_predio"
		wf_BuscaPredios()
		
End Choose
end event

