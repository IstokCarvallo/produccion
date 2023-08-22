$PBExportHeader$w_mant_deta_ctlcallotes.srw
$PBExportComments$Ventana de Mantención de la Tabla CTLCALLOTES
forward
global type w_mant_deta_ctlcallotes from w_mant_detalle_csd
end type
end forward

global type w_mant_deta_ctlcallotes from w_mant_detalle_csd
integer x = 123
integer y = 96
integer width = 2469
integer height = 1780
end type
global w_mant_deta_ctlcallotes w_mant_deta_ctlcallotes

type variables
DataWindowChild	 	idwc_productor,idwc_especie, idwc_variedad, idwc_embalaje,&
                 			    idwc_calibre, idwc_packing, idwc_planta, idwc_cliente
							
uo_calibre				iuo_calibre
uo_embalajesprod		iuo_embalajes	
uo_especie				iuo_especies	
uo_plantadesp			iuo_plantadesp
uo_productores		iuo_productores	
uo_variedades			iuo_variedades
uo_etiquetas			iuo_Etiquetas
end variables

forward prototypes
public function boolean duplicado (string as_valor, integer ai_tipo)
public function boolean existelote ()
end prototypes

public function boolean duplicado (string as_valor, integer ai_tipo);Long     ll_fila

ll_fila = dw_1.Find("cclo_numero = " + as_valor, 1, dw_1.RowCount())

IF ll_fila > 0 AND ll_fila <> il_fila THEN
	MessageBox("Error","Registro ya fue ingresado anteriormente",Information!, Ok!)
	RETURN True
ELSE
	RETURN False
END IF

end function

public function boolean existelote ();string ls_busca, ls_calibre, ls_embalaje
long ll_fila


ls_Busca = 'clie_codigo = ' + String(dw_1.Object.clie_codigo[il_fila]) + ' And plde_codigo = ' + String(dw_1.Object.plde_codigo[il_fila]) + ' And etiq_codigo = ' + String(dw_1.Object.etiq_codigo[il_fila]) + &
		' And prod_codigo = ' + String(dw_1.Object.prod_codigo[il_fila]) + ' and espe_codigo = ' + String(dw_1.Object.espe_codigo[il_fila]) + &
		' And vari_codigo = ' + String(dw_1.Object.vari_codigo[il_fila]) + ' And vaca_calibr = "' + dw_1.Object.vaca_calibr[il_fila] + &
		'" And plde_codpak = ' + String(dw_1.Object.plde_codpak[il_fila]) + ' And emba_codigo = "' + dw_1.Object.emba_codigo[il_fila]  + &
		'" and String(cclo_fecemb, "dd/mm/yyyy") = "' + String(dw_1.Object.cclo_fecemb[il_fila], 'dd/mm/yyyy') +  '"'


ll_fila = dw_1.Find(ls_Busca, 1, dw_1.RowCount())

IF ll_fila > 0 and ll_fila <> il_fila then
	Messagebox("Atención","Datos Ingresados coinciden con lote N° " + string(dw_1.Object.cclo_numero[ll_fila]) + '.')
	RETURN TRUE
else
	RETURN FALSE
end if
end function

on w_mant_deta_ctlcallotes.create
call super::create
end on

on w_mant_deta_ctlcallotes.destroy
call super::destroy
end on

event ue_recuperadatos;call super::ue_recuperadatos;Date		ld_hoy
String		ls_usuario
Integer	li_grupo

ld_hoy=	Today()
ls_usuario= Upper(Gstr_Us.Nombre)

ias_campo[1]	= 	String(dw_1.Object.clie_codigo[il_Fila])
ias_campo[2]	= 	String(dw_1.Object.plde_codigo[il_Fila])
ias_campo[3]	= 	String(dw_1.Object.cclo_numero[il_Fila])
ias_campo[4]	= 	String(dw_1.Object.prod_codigo[il_Fila])
ias_campo[5]	= 	String(dw_1.Object.espe_codigo[il_Fila])
ias_campo[6]	= 	String(dw_1.Object.vari_codigo[il_Fila])
ias_campo[7]	= 	String(dw_1.Object.emba_codigo[il_Fila])
ias_campo[8]	= 	String(dw_1.Object.vaca_calibr[il_Fila])
ias_campo[9]	= 	String(dw_1.Object.plde_codpak[il_Fila])
ias_campo[10]	= 	String(dw_1.Object.cclo_fecemb[il_Fila])
ias_campo[11]	= 	String(dw_1.Object.cclo_tamlot[il_Fila])
ias_campo[12]	=	String(dw_1.object.vari_nombre[il_fila])
ias_campo[13]	=	String(dw_1.object.etiq_codigo[il_fila])
ias_campo[14]	=	String(dw_1.object.cclo_tipori[il_fila])

dw_1.SetItem(il_fila, "clie_codigo", Integer(istr_mant.argumento[1]))
dw_1.SetItem(il_fila, "plde_codigo", Integer(istr_mant.argumento[2]))
dw_1.SetItem(il_fila, "cclo_fecemb", Date(ias_campo[10]))
dw_1.SetItemStatus(il_fila, "clie_codigo", Primary!, NotModIfied!)
dw_1.SetItemStatus(il_fila, "plde_codigo", Primary!, NotModIfied!)

If istr_mant.agrega = False Then
	idwc_variedad.Retrieve( Integer(ias_campo[5]))
	idwc_calibre.Retrieve( Integer(ias_campo[5]), Integer(ias_campo[6]))
	dw_1.Object.plde_codigo.Protect	=	1
	dw_1.Object.cclo_numero.Protect	=	1
	dw_1.Object.plde_codigo.Color		=	RGB(255,255,255)
	dw_1.Object.cclo_numero.Color	=	RGB(255,255,255)
	dw_1.Object.plde_codigo.BackGround.Color		=	553648127
	dw_1.Object.cclo_numero.BackGround.Color	=	553648127
End If

If dw_1.Rowcount() > 0 Then	
	li_Grupo = BuscaGrupo(ls_Usuario)					
	 IF  li_Grupo	<=  2 THEN 
		dw_1.SetFocus()
		pb_acepta.Enabled	= True
		pb_cancela.Enabled	= True
	Else
		dw_1.Enabled			= False	
		pb_acepta.Enabled	= False	
		pb_salir.Enabled		= True 	
	End If 	
End If 	


end event

event ue_deshace;If UpperBound(ias_campo) > 0 Then
	dw_1.SetItem(il_fila, "clie_codigo", Integer(ias_campo[1]))
	dw_1.SetItem(il_fila, "plde_codigo", Integer(ias_campo[2]))
	dw_1.SetItem(il_fila, "cclo_numero", Integer(ias_campo[3]))
	dw_1.SetItem(il_fila, "prod_codigo", Long(ias_campo[4]))
	dw_1.SetItem(il_fila, "espe_codigo", Integer(ias_campo[5]))
	dw_1.SetItem(il_fila, "vari_codigo", Integer(ias_campo[6]))
	dw_1.SetItem(il_fila, "emba_codigo", Integer(ias_campo[7]))
	dw_1.SetItem(il_fila, "vaca_calibr", Integer(ias_campo[8]))
	dw_1.SetItem(il_fila, "plde_codpak", Integer(ias_campo[9]))
	dw_1.SetItem(il_fila, "cclo_fecemb", date(ias_campo[10]))
	dw_1.SetItem(il_fila, "cclo_tamlot", Integer(ias_campo[11]))
	dw_1.SetItem(il_fila, "vari_nombre", ias_campo[12])
	dw_1.SetItem(il_fila, "etiq_codigo", Integer(ias_campo[13]))
	dw_1.SetItem(il_fila, "cclo_tipori", Integer(ias_campo[14]))
End If
end event

event ue_antesguardar;dw_1.accepttext()
String	ls_mensaje, ls_colu[]
Integer	li_cont,li_numero

If IsNull(dw_1.Object.cclo_numero[il_fila]) OR dw_1.Object.cclo_numero[il_fila] = 0 Then
	li_cont	++
	ls_mensaje 			= ls_mensaje + "~nNúmero"
	ls_colu[li_cont]	= "cclo_numero"
End If

If IsNull(dw_1.Object.prod_codigo[il_fila]) OR dw_1.Object.prod_codigo[il_fila] = 0 Then
	li_cont	++
	ls_mensaje 			= ls_mensaje + "~nProductor"
	ls_colu[li_cont]	= "prod_codigo"
End If

If IsNull(dw_1.Object.espe_codigo[il_fila]) OR dw_1.Object.espe_codigo[il_fila] = 0 Then
	li_cont	++
	ls_mensaje 			= ls_mensaje + "~nEspecie"
	ls_colu[li_cont]	= "espe_codigo"
End If

If IsNull(dw_1.Object.vari_codigo[il_fila]) OR dw_1.Object.vari_codigo[il_fila] = 0 Then
	li_cont	++
	ls_mensaje 			= ls_mensaje + "~nVariedad"
	ls_colu[li_cont]	= "vari_codigo"
End If

If IsNull(dw_1.Object.Emba_codigo[il_fila]) OR trim(dw_1.Object.Emba_codigo[il_fila]) = "" Then
	li_cont	++
	ls_mensaje 			= ls_mensaje + "~nEmbalaje"
	ls_colu[li_cont]	= "Emba_codigo"
End If

If IsNull(dw_1.Object.plde_codpak[il_fila]) OR dw_1.Object.plde_codpak[il_fila] = 0 Then
	li_cont	++
	ls_mensaje 			= ls_mensaje + "~nPacking"
	ls_colu[li_cont]	= "plde_codpak"
End If

If IsNull(dw_1.Object.cclo_fecemb[il_fila]) Then
	li_cont	++
	ls_mensaje 			= ls_mensaje + "~nFecha Embalaje"
	ls_colu[li_cont]	= "cclo_fecemb"
End If

If IsNull(dw_1.Object.cclo_tamlot[il_fila]) OR dw_1.Object.cclo_tamlot[il_fila] = 0 Then
	li_cont	++
	ls_mensaje 			= ls_mensaje + "~nTamaño Lote"
	ls_colu[li_cont]	= "cclo_tamlot"
End If

If IsNull(dw_1.Object.etiq_codigo[il_fila]) OR dw_1.Object.etiq_codigo[il_fila] = 0 Then
	li_cont	++
	ls_mensaje 		= ls_mensaje + "~nEtiquetas"
	ls_colu[li_cont]	= "etiq_codigo"
End If

If li_cont > 0 Then
	MessageBox("Error de Consistencia", "Falta el Ingreso de :" + ls_mensaje + ".", StopSign!, Ok!)
	dw_1.SetColumn(ls_colu[1])
	dw_1.SetFocus()
	Message.DoubleParm = -1
End If
end event

event ue_nuevo;call super::ue_nuevo;Date	ld_hoy

ld_hoy	=	Today()

dw_1.Object.clie_codigo[il_fila]		=	Integer(istr_mant.argumento[1])
dw_1.Object.plde_codigo[il_fila]	=	Integer(istr_mant.argumento[2])
dw_1.Object.cclo_fecemb[il_fila]	= 	ld_hoy
dw_1.Object.cclo_tipori[il_fila]		= 	1


dw_1.SetItemStatus(il_fila, "clie_codigo", Primary!, NotModified!)
dw_1.SetItemStatus(il_fila, "plde_codigo", Primary!, NotModified!)

end event

event open;Integer		li_Cliente, li_Planta
/*
Argumentos :
					[1]	=	Código de Cliente
					[2]	=	Código de Planta
*/

x	= 100
y	= 450

This.Icon	=	Gstr_apl.Icono

PostEvent("ue_recuperadatos")

istr_mant = Message.PowerObjectParm

iuo_calibre				= 	Create uo_calibre
iuo_embalajes			=	Create uo_embalajesprod
iuo_especies			=  Create uo_especie		
iuo_plantadesp      	=  Create uo_plantadesp		
iuo_productores		=  Create uo_productores
iuo_variedades			=  Create uo_variedades
iuo_Etiquetas			=	Create uo_etiquetas

li_cliente	=	Integer(istr_mant.argumento[1])
li_planta	=	Integer(istr_mant.argumento[2])

dw_1.GetChild("clie_codigo", idwc_cliente)
idwc_cliente.SetTransObject(sqlca)
idwc_cliente.Retrieve()
dw_1.SetItem(1, "clie_codigo",li_cliente)

dw_1.GetChild("plde_codigo", idwc_planta)
dw_1.GetChild("prod_codigo", idwc_productor)
dw_1.GetChild("espe_codigo", idwc_especie)
dw_1.GetChild("emba_codigo", idwc_embalaje)
dw_1.GetChild("plde_codpak", idwc_packing)
dw_1.Getchild("vari_codigo", idwc_variedad)
dw_1.Getchild("Vaca_Calibr", idwc_calibre)

idwc_planta.SetTransObject(SQLCA)
idwc_productor.SetTransObject(SQLCA)
idwc_especie.SetTransObject(SQLCA)
idwc_embalaje.SetTransObject(SQLCA)
idwc_packing.SetTransObject(SQLCA)
idwc_variedad.SetTransObject(SQLCA)
idwc_calibre.SetTransObject(SQLCA)

idwc_planta.Retrieve(1)
idwc_productor.Retrieve(-1)
idwc_especie.Retrieve()
idwc_embalaje.Retrieve()
idwc_packing.Retrieve(2)

dw_1.SetItem(1, "plde_codigo",li_planta)

IF idwc_variedad.Retrieve(gi_CodEspecie)	=	0 THEN 
	idwc_variedad.insertrow(0)
END IF

IF idwc_calibre.Retrieve(gi_CodEspecie, gi_CodVariedad)	=	0 THEN 
	idwc_calibre.insertrow(0)
END IF

dw_1.SetTransObject(sqlca)
istr_mant.dw.ShareData(dw_1)
end event

type pb_ultimo from w_mant_detalle_csd`pb_ultimo within w_mant_deta_ctlcallotes
end type

type pb_siguiente from w_mant_detalle_csd`pb_siguiente within w_mant_deta_ctlcallotes
end type

type pb_anterior from w_mant_detalle_csd`pb_anterior within w_mant_deta_ctlcallotes
end type

type pb_primero from w_mant_detalle_csd`pb_primero within w_mant_deta_ctlcallotes
end type

type pb_cancela from w_mant_detalle_csd`pb_cancela within w_mant_deta_ctlcallotes
integer x = 2194
integer y = 392
end type

type pb_acepta from w_mant_detalle_csd`pb_acepta within w_mant_deta_ctlcallotes
integer x = 2181
integer y = 176
end type

event pb_acepta::clicked;istr_mant.respuesta = 1

IF istr_mant.agrega THEN	
	Parent.TriggerEvent("ue_nuevo")
ELSE
	CloseWithReturn(Parent, istr_mant)
END IF
end event

type pb_salir from w_mant_detalle_csd`pb_salir within w_mant_deta_ctlcallotes
integer x = 2181
integer y = 608
end type

type dw_1 from w_mant_detalle_csd`dw_1 within w_mant_deta_ctlcallotes
integer x = 105
integer y = 112
integer width = 1806
integer height = 1488
string dataobject = "dw_mant_ctlcallotes"
end type

event dw_1::itemchanged;String		ls_columna, ls_Null, ls_codigo
Integer	ll_Fila

SetNull(ls_Null)
This.AcceptText()

ls_columna	=	dwo.Name

Choose Case ls_columna		
	Case "prod_codigo" 
   		If Not iuo_productores.existe(Integer(Data), True, Sqlca) Then
 			this.SetItem(Row, "prod_codigo", Long(ls_Null))
			Return 1
		Else
			If existelote() Then
				 This.SetItem(Row, "prod_codigo", Long(ls_Null))
				 Return 1
			else
				This.Object.prod_nombre[Row]	=	iuo_productores.Nombre
			End if
		End If		
	
	Case "espe_codigo"
		
		If Not iuo_especies.existe(Integer(Data), True, Sqlca) or existelote()Then
			This.SetItem(Row, "espe_codigo", Integer(ls_Null))
			Return 1
		Else	
			This.Object.espe_nombre[Row]	=	iuo_especies.Nombre
			IF idwc_variedad.Retrieve(Long(Data)) = 0 Then 	Messagebox("Atención","Especie Ingresada no Tiene Variedades asociadas")
			This.Object.vari_nombre[Row]	=	ls_Null			

			idwc_calibre.Retrieve(Long(Data), This.Object.vari_codigo[Row])
		End If	
	
	Case "vari_codigo"
		If IsNull(This.Object.espe_codigo[Row]) Then
			Messagebox("Error","Ingrese una Especie")
		ElseIf Not iuo_variedades.existe(This.Object.espe_codigo[Row],Integer(Data),True,Sqlca)  or existelote() Then		
			This.SetItem(Row, "vari_codigo", Long(ls_Null))		  
			Return 1
		Else
			This.Object.vari_nombre[Row]	=	iuo_variedades.NombreVariedad
   			idwc_calibre.Retrieve(This.Object.espe_codigo[Row], Long(Data))
		End If
	
	Case "vaca_calibr"		
		If IsNull(This.Object.espe_codigo[Row]) OR IsNull(This.Object.vari_codigo[Row]) Then
			MessageBox("Error","Ingrese una Especie y una Variedad")
		ElseIf Not iuo_calibre.existe(This.Object.espe_codigo[Row],&
												This.Object.vari_codigo[Row],Data,True,Sqlca) or existelote()Then		
			This.SetItem(Row, "vaca_calibr", ls_Null)
			Return 1
		End If
		
	Case "emba_codigo"       
		If Not iuo_Embalajes.existe(Integer(istr_mant.argumento[1]),Data, True, Sqlca) or existelote() Then
 	 		This.SetItem(Row, "emba_codigo", ls_Null)
			Return 1
		End If		
	
	Case "cclo_numero"
		If Duplicado(Data, 1) or existelote() Then			
			dw_1.SetItem(Row, "cclo_numero", Long(ls_Null))
			Return 1
		End If	

	Case "plde_codpak"       
		If Not iuo_plantadesp.existepacking(Integer(Data), True,Sqlca) or existelote() Then
 	 		This.SetItem(Row, "plde_codpak", Long(ls_Null))
			Return 1
		Else
			This.Object.plde_nombre[Row]	=	iuo_plantadesp.Nombre
		End If

	Case "etiq_codigo"       
		If Not iuo_Etiquetas.Existe(Integer(Data), True,Sqlca) or existelote() Then
 	 		This.SetItem(Row, ls_Columna, Long(ls_Null))
			Return 1
		End If
		
	Case "cclo_fecemb"
		If existelote() Then
			This.SetItem(Row, ls_Columna, Date(ls_Null))
			Return 1
		End If
End Choose
end event

