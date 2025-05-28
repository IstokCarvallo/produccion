$PBExportHeader$w_maed_ctlcalplanillacondicionambiente.srw
$PBExportComments$Ingresador de antecedentes para Planilla Cuantitativa por especies.
forward
global type w_maed_ctlcalplanillacondicionambiente from w_mant_encab_deta_csd
end type
type tab_1 from tab within w_maed_ctlcalplanillacondicionambiente
end type
type tabpage_1 from userobject within tab_1
end type
type dw_temperatura from uo_dw within tabpage_1
end type
type tabpage_1 from userobject within tab_1
dw_temperatura dw_temperatura
end type
type tabpage_2 from userobject within tab_1
end type
type dw_condicion from uo_dw within tabpage_2
end type
type tabpage_2 from userobject within tab_1
dw_condicion dw_condicion
end type
type tabpage_3 from userobject within tab_1
end type
type dw_firmeza from uo_dw within tabpage_3
end type
type tabpage_3 from userobject within tab_1
dw_firmeza dw_firmeza
end type
type tabpage_4 from userobject within tab_1
end type
type dw_psi from datawindow within tabpage_4
end type
type tabpage_4 from userobject within tab_1
dw_psi dw_psi
end type
type tabpage_5 from userobject within tab_1
end type
type mle_observ from multilineedit within tabpage_5
end type
type tabpage_5 from userobject within tab_1
mle_observ mle_observ
end type
type tab_1 from tab within w_maed_ctlcalplanillacondicionambiente
tabpage_1 tabpage_1
tabpage_2 tabpage_2
tabpage_3 tabpage_3
tabpage_4 tabpage_4
tabpage_5 tabpage_5
end type
end forward

global type w_maed_ctlcalplanillacondicionambiente from w_mant_encab_deta_csd
integer width = 5038
integer height = 2300
string menuname = ""
windowstate windowstate = maximized!
event ue_validapassword ( )
tab_1 tab_1
end type
global w_maed_ctlcalplanillacondicionambiente w_maed_ctlcalplanillacondicionambiente

type variables
DataWindowChild	idwc_plantas, idwc_especies, idwc_zonas, idwc_productores, &
						idwc_predios, idwc_cuarteles, idwc_variedades, idwc_inspectores, idwc_camaras

Datawindow    		dw_3,dw_4,dw_5,dw_6

uo_zonas             		iuo_zonas
uo_plantadesp        	iuo_plantas
uo_variedades        	iuo_variedades
uo_productores       	iuo_productor
uo_ctlcalinspectores	iuo_inspectores
uo_especie         	 	iuo_especies
uo_prodcuarteles     	iuo_prodcuarteles
uo_prodpredio			iuo_prodpredio
uo_camarasbode		iuo_camaras
uo_lotesfrutagranel	iuo_Lotes

Integer 	ii_sw //este sw controla si la planilla existe con otra especie




end variables

forward prototypes
protected function integer wf_modifica ()
public subroutine habilitaencab (boolean habilita)
public subroutine habilitaingreso (string ls_columna)
protected function boolean wf_actualiza_db (boolean borrando)
public function boolean wf_visibilidad (integer especie)
public function long wf_recupera (datawindow adw, string objeto, integer tipo, integer cliente, integer planta, integer especie, long numero)
public subroutine wf_calcula_promedio (integer fila, string columna)
public subroutine wf_calcula (datawindow adw, integer tipo)
public function boolean wf_planilla (long planilla, integer planta)
public function boolean duplicado (string valor)
public function boolean wf_buscalote ()
public function boolean wf_recuperadistribucion (integer ai_cliente, integer ai_planta, long al_numero, integer ai_especie, integer ai_variedad)
public subroutine wf_desviacion_estandar (long al_fila, string as_columna)
public subroutine wf_child (integer especie, integer productor, integer predio, integer planta)
public subroutine wf_calcula_libra (integer fila, string columna)
end prototypes

event ue_validapassword();istr_mant.Argumento[1]	=	"Control de Calidad"
istr_mant.Argumento[2]	=	gstr_parlote.paswor

OpenWithParm(w_password, istr_mant)

istr_mant	=	Message.PowerObjectParm

IF istr_mant.Respuesta = 0 THEN Close(This)
end event

protected function integer wf_modifica ();RETURN 1
end function

public subroutine habilitaencab (boolean habilita);If Habilita Then
	dw_2.Object.coam_numero.Protect				=	0
	dw_2.Object.zona_codigo.Protect				=	0
	dw_2.Object.plde_codigo.Protect					=	0
	dw_2.Object.vari_codigo.Protect					=	0
	dw_2.Object.prod_codigo.Protect					=	0
	dw_2.Object.prpr_codigo.Protect					=	0
	dw_2.Object.prcc_codigo.Protect					=	0
	dw_2.Object.ccin_codigo.Protect					=	0
	dw_2.Object.cama_codigo.Protect				=	0
	dw_2.Object.coam_numero.BackGround.Color	=	Rgb(255,255,255)
	dw_2.Object.zona_codigo.BackGround.Color	=	Rgb(255,255,255)
	dw_2.Object.plde_codigo.BackGround.Color	=	Rgb(255,255,255)
	dw_2.Object.vari_codigo.BackGround.Color	=	Rgb(255,255,255)
	dw_2.Object.prod_codigo.BackGround.Color	=	Rgb(255,255,255)
	dw_2.Object.prpr_codigo.BackGround.Color	=	Rgb(255,255,255)
	dw_2.Object.prcc_codigo.BackGround.Color	=	Rgb(255,255,255)
	dw_2.Object.ccin_codigo.BackGround.Color	=	Rgb(255,255,255)
	dw_2.Object.cama_codigo.BackGround.Color	=	Rgb(255,255,255)
  	pb_buscar.Enabled   									= True
Else
	dw_2.Object.coam_numero.Protect				=	1
	dw_2.Object.zona_codigo.Protect				=	1
	dw_2.Object.plde_codigo.Protect					=	1
	dw_2.Object.vari_codigo.Protect					=	1
	dw_2.Object.prod_codigo.Protect					=	1
	dw_2.Object.prpr_codigo.Protect					=	1
	dw_2.Object.prcc_codigo.Protect					=	1
	dw_2.Object.ccin_codigo.Protect					=	1
	dw_2.Object.cama_codigo.Protect				=	1
	dw_2.Object.coam_numero.BackGround.Color	=	553648127
	dw_2.Object.zona_codigo.BackGround.Color	=	553648127
	dw_2.Object.plde_codigo.BackGround.Color	=	553648127
	dw_2.Object.vari_codigo.BackGround.Color	=	553648127
	dw_2.Object.prod_codigo.BackGround.Color	=	553648127
	dw_2.Object.prpr_codigo.BackGround.Color	=	553648127
	dw_2.Object.prcc_codigo.BackGround.Color	=	553648127
	dw_2.Object.ccin_codigo.BackGround.Color	=	553648127
	dw_2.Object.cama_codigo.BackGround.Color	=	553648127
	pb_buscar.Enabled   									= False
End If
end subroutine

public subroutine habilitaingreso (string ls_columna);Boolean	lb_Estado = True

dw_2.AcceptText()

IF ls_Columna <> "zona_codigo" AND &
	(dw_2.Object.zona_codigo[1]) = 0 OR IsNull(dw_2.Object.zona_codigo[1]) THEN
	lb_Estado	=	False
END IF

IF ls_Columna <> "plde_codigo" AND &
	(dw_2.Object.plde_codigo[1]) = 0 OR IsNull(dw_2.Object.plde_codigo[1]) THEN
	lb_Estado	=	False
END IF

IF ls_Columna <> "vari_codigo" AND &
	(dw_2.Object.vari_codigo[1]) = 0 OR IsNull(dw_2.Object.vari_codigo[1]) THEN
	lb_Estado	=	False
END IF
	
IF ls_Columna <> "prod_codigo" AND &
	(dw_2.Object.prod_codigo[1]) = 0 OR IsNull(dw_2.Object.prod_codigo[1]) THEN
	lb_Estado	=	False
END IF

IF ls_Columna <> "ccin_codigo" AND &
	(dw_2.Object.ccin_codigo[1]) = 0 OR IsNull(dw_2.Object.ccin_codigo[1]) THEN
	lb_Estado	=	False
END IF

IF ls_Columna <> "coam_numero" AND &
	(dw_2.Object.coam_numero[1]) = 0 OR IsNull(dw_2.Object.coam_numero[1]) THEN
	lb_Estado	=	False
END IF

pb_grabar.Enabled = lb_Estado
end subroutine

protected function boolean wf_actualiza_db (boolean borrando);Boolean	lb_AutoCommit, lb_Retorno

IF Not dw_2.uf_check_required(0) THEN RETURN False

lb_AutoCommit		=	sqlca.AutoCommit
sqlca.AutoCommit	=	False

IF Borrando THEN
	IF dw_6.Update(True, False) = 1 THEN
		IF dw_5.Update(True, False) = 1 THEN
			IF dw_4.Update(True,False) =	1 THEN
				IF dw_3.Update(True, False) = 1 THEN
					IF dw_2.Update(True, False) = 1 THEN
						Commit;
						
						IF sqlca.SQLCode <> 0 THEN
							F_ErrorBaseDatos(sqlca, This.Title)
						ELSE
							lb_Retorno	=	True
							
							dw_6.ResetUpdate()
							dw_5.ResetUpdate()
							dw_4.ResetUpdate()
							dw_3.ResetUpdate()
							dw_2.ResetUpdate()
						END IF
					ELSE
						F_ErrorBaseDatos(sqlca, This.Title)
						
						RollBack;
					END IF
				ELSE
					F_ErrorBaseDatos(sqlca, This.Title)
					
					RollBack;
				END IF
			ELSE
				F_ErrorBaseDatos(sqlca, This.Title)
				RollBack;
			END IF
		ELSE
		  F_ErrorBaseDatos(sqlca, This.Title)
		  RollBack;
		END IF
	ELSE
		F_ErrorBaseDatos(sqlca, This.Title)
		RollBack;
	END IF
ELSE
  IF dw_2.Update(True, False) = 1 THEN
	  IF dw_3.Update(True, False) = 1 THEN
	     IF dw_4.Update(True, False) = 1 THEN
		     IF dw_5.Update(True,False) =	1	THEN
				  IF dw_6.Update(True,False) =	1	THEN
						Commit;
				
						IF sqlca.SQLCode <> 0 THEN
							F_ErrorBaseDatos(sqlca, This.Title)
						ELSE
							lb_Retorno	=	True
							
							dw_2.ResetUpdate()
							dw_3.ResetUpdate()
							dw_4.ResetUpdate()
							dw_5.ResetUpdate()
							dw_6.ResetUpdate()
						END IF
					ELSE
					  F_ErrorBaseDatos(sqlca, This.Title)
					  RollBack;
					END IF	
				ELSE
					F_ErrorBaseDatos(sqlca, This.Title)
					RollBack;
				END IF
			 ELSE
				F_ErrorBaseDatos(sqlca, This.Title)
				RollBack;
			 END IF
		 ELSE
			F_ErrorBaseDatos(sqlca, This.Title)
			RollBack;
		 END IF
	ELSE
		F_ErrorBaseDatos(sqlca, This.Title)
		RollBack;
	END IF
END IF

sqlca.AutoCommit	=	lb_AutoCommit

RETURN lb_Retorno
return false
end function

public function boolean wf_visibilidad (integer especie);Boolean	lb_Retorno = True

Choose Case Especie		
	Case 41		
		Tab_1.TabPage_1.Visible	=	True
		Tab_1.TabPage_2.Visible	=	True
		Tab_1.TabPage_3.Visible	=	True
		Tab_1.TabPage_4.Visible	=	True		
End Choose

Return lb_Retorno
end function

public function long wf_recupera (datawindow adw, string objeto, integer tipo, integer cliente, integer planta, integer especie, long numero);DataStore	lds_Recupera
Long    		ll_Retorno, ll_Fila, ll_Busca, ll_New
String			ls_Titulo, ls_Prefijo, ls_Codigo
Integer		li_Codigo

lds_Recupera	=	Create DataStore

adw.Retrieve(Cliente, Planta, Especie, Numero)

lds_Recupera.DataObject =	Objeto
lds_Recupera.SetTransObject(Sqlca)

Choose Case Tipo
	Case 1
		ls_Prefijo = 'tega_'

	Case 2
		ls_Prefijo = 'cond_'
		
	Case 3
		ls_Prefijo = 'fico_'
		
End Choose 

If lds_Recupera.Retrieve(Especie) = 0 Then
	MessageBox('Alerta...', 'No Existen Titulos para Opción.', StopSign!, OK!)	
	ll_Retorno = -1
Else
	For ll_fila = 1 To lds_Recupera.RowCount()
		li_Codigo	=	lds_Recupera.GetItemNumber(ll_Fila, 2)
		ls_Titulo	= lds_Recupera.GetItemString(ll_Fila, 3)
		
		ll_Busca	= adw.Find(ls_Prefijo + "Codigo = " + String(li_Codigo), 1, adw.RowCount())
		
	  	If ll_Busca = 0 Then
			 ll_New	=	adw.InsertRow(0)
			 adw.SetItem(ll_New, ls_Prefijo + 'codigo', li_Codigo)
			 adw.SetItem(ll_new, ls_Prefijo + 'nombre', ls_titulo)
		End If
	Next
	ll_Retorno = adw.RowCount()
End If

Destroy lds_Recupera	

Return ll_Retorno
end function

public subroutine wf_calcula_promedio (integer fila, string columna);Decimal{2}	ld_valor,ld_media,ld_suma, ld_null
String			ls_columna
Long			ll_fila
Integer		ai_columna, li_Cuenta, li_Div

dw_6.AcceptText()
SetNull(ld_null)
ai_columna	=	Integer(Columna)

If Integer(istr_mant.argumento[3]) = fila Then
	If Integer(istr_mant.argumento[6]) < ai_columna Then
		istr_mant.argumento[6]	=	String(ai_columna)
	Else
		ai_columna = Integer(istr_mant.argumento[6])
	End If
Else
	istr_mant.argumento[6] = '0'
	istr_mant.argumento[3] = String(Fila)
	If Integer(istr_mant.argumento[6]) < ai_columna Then
		istr_mant.argumento[6]	=	String(ai_columna)
	Else
		ai_columna = Integer(istr_mant.argumento[6])
	End If
End If

For ll_fila= 1 To 20
	If ll_fila >= 10 Then
		ls_columna	=	"cofi_mues" +  String(ll_fila)
	Else
		ls_columna	=	"cofi_mues" + String(0) + String(ll_fila)
	End If
	
	If Not IsNull(dw_6.GetitemNumber(Fila,ls_columna)) And dw_6.GetitemNumber(Fila,ls_columna) > 0 Then li_cuenta++
	
	ld_valor   =  dw_6.GetitemNumber(Fila,ls_columna)

	If IsNull(ld_valor) And li_cuenta < ai_columna Then
		Messagebox("Atención","Muestra" +"  " + String(ll_fila)+ " " + "no puede ser Nula",exclamation!)
		dw_6.SetItem(Fila,"cofi_medias",ld_null)
		dw_6.SetColumn(ls_columna)
		dw_6.SetFocus()
		Return
	Else
		If li_cuenta > ai_columna Then
			li_Div = li_cuenta 
		Else
			li_Div = ai_columna 
		End If
	
		If IsNull(ld_valor) Then ld_valor = 0 
		
		ld_suma  = ld_suma + ld_valor 
		ld_media = ld_suma / li_Div
		dw_6.SetItem(Fila,"cofi_medias",ld_media)
	End If
Next
end subroutine

public subroutine wf_calcula (datawindow adw, integer tipo);Decimal{2}	ld_Total, ld_Total1
Long			ll_Fila
Integer		li_Acum = 0, li_Acum1 = 0

adw.AcceptText()

For ll_Fila = 1 To adw.RowCount()
	If Tipo = 1 Then
		If Not IsNull(adw.Object.anms_mues01[ll_Fila]) And adw.Object.anms_mues01[ll_Fila] >= 0 Then
			ld_Total += adw.Object.anms_mues01[ll_Fila]
			li_Acum++
		End If
	Else
		If Not IsNull(adw.Object.coco_valore[ll_Fila]) And adw.Object.coco_valore[ll_Fila] >= 0 Then
			ld_Total += adw.Object.coco_valore[ll_Fila]
			li_Acum++
		End If
		
		If Not IsNull(adw.Object.coco_colume[ll_Fila]) And adw.Object.coco_colume[ll_Fila] >= 0 Then
			ld_Total1 += adw.Object.coco_colume[ll_Fila]
			li_Acum1++
		End If
		
	End If
Next

If li_Acum > 0 Then
	ld_Total = Round(ld_Total / li_Acum, 1)
	adw.Object.t_total.Text = String(ld_Total, '#,##0.0')
End If

If li_Acum1 > 0 Then
	ld_Total1 = Round(ld_Total1 / li_Acum1, 1)
	adw.Object.t_total_1.Text = String(ld_Total1, '#,##0.0')
End If

For ll_Fila = 1 To adw.RowCount()	
	adw.Object.coco_medias[ll_Fila] = ld_Total
	adw.Object.coco_medcol[ll_Fila] = ld_Total1
Next
end subroutine

public function boolean wf_planilla (long planilla, integer planta);Long    ll_existe, ll_productor
Integer li_variedad, li_especie, li_zona, li_predio

ii_sw = 0

SELECT coam_numero, vari_codigo, espe_codigo,  prod_codigo, zona_codigo, prpr_codigo
	INTO :ll_existe, :li_variedad, :li_especie, :ll_productor,:li_zona, :li_predio
	FROM dbo.ctlcalcondicionambienteenca
	WHERE	clie_codigo = :gi_codexport
	AND 		plde_codigo = :Planta 
	AND 		coam_numero = :Planilla
	Using Sqlca;

If sqlca.SQLCode = -1 Then
	F_errorbasedatos(sqlca,"Lectura tabla ctlcalrecepcionfrutasenca  ")
	Return False
ElseIf sqlca.sqlcode	=	0 Then

	If li_especie = dw_2.Object.espe_codigo[1] Then
		istr_busq.argum[1] = String(Planilla)
		istr_busq.argum[3] = String(Planta)
		istr_busq.argum[4] = String(li_zona)
		istr_busq.argum[5] = String(ll_productor)
		istr_busq.argum[8] = String(li_predio)
		ii_sw = 0
	Else
		ii_sw = 1
	End If
	
	Return True
Else
	Return False
End If
end function

public function boolean duplicado (string valor);return True
end function

public function boolean wf_buscalote ();Boolean	lb_Retorno = True
Long      ll_fila

IF (IsNull(iuo_Plantas.Codigo) Or iuo_Plantas.Codigo = 0) Or &
	(IsNull(iuo_Especies.Codigo) Or iuo_Especies.Codigo = 0) Or &
	(IsNull(iuo_Zonas.Codigo) Or iuo_Zonas.Codigo = 0) THen
		Messagebox("Atención","Debe Seleccionar Zona, Planta y Especie", StopSign!, OK!)
		dw_2.SetFocus()
		lb_Retorno = False
ELSE	
	istr_busq.argum[1] 	=	String(iuo_Plantas.Codigo)
	istr_busq.argum[2] 	=	String(iuo_Especies.Codigo)
	istr_busq.argum[4] 	=	String(dw_2.Object.zona_codigo[1])
	istr_busq.argum[5] 	=	'RECEPCION'
	istr_busq.argum[6] 	=	String(iuo_variedades.Variedad)
	istr_busq.argum[7] 	=	String(iuo_Productor.Codigo)
	istr_busq.argum[8] 	=	String(gi_Codexport)
	istr_busq.argum[20]	=	String(1)
	
	If istr_busq.argum[6] =	'0' Then istr_busq.argum[6] = '-1'
	If istr_busq.argum[7] =	'0' Then istr_busq.argum[7] = '-1'
	
	OpenWithParm(w_busc_lotefrutagranel, istr_busq)
	
	istr_busq	= Message.PowerObjectParm
	
	IF istr_busq.argum[3] <> "" THEN
		dw_2.Object.lote_codigo[1]		=	Integer(istr_busq.argum[2])
		dw_2.Object.plde_codigo[1]		=	Integer(istr_busq.argum[3])
		dw_2.Object.prod_codigo[1]	=	Long(istr_busq.argum[5])
		dw_2.Object.vari_codigo[1]		=	Integer(istr_busq.argum[6])
		dw_2.Object.prpr_codigo[1]		=	Integer(istr_busq.argum[8])
		dw_2.Object.prcc_codigo[1] 	=	Integer(istr_busq.argum[9])
		dw_2.Object.coam_totbul[1]	=	Real(istr_busq.argum[10])
		
		iuo_Variedades.Variedad			=	Integer(istr_busq.argum[6])

		wf_recuperadistribucion(gi_Codexport, dw_2.Object.plde_codigo[1], dw_2.Object.coam_numero[1], dw_2.Object.espe_codigo[1], dw_2.Object.vari_codigo[1])
	End If
End If

Return lb_Retorno
end function

public function boolean wf_recuperadistribucion (integer ai_cliente, integer ai_planta, long al_numero, integer ai_especie, integer ai_variedad);Boolean	lb_Retorno = True
Long		ll_fila, ll_fild, ll_New, ll_variedad, ll_Secuen
String 	ls_calibre

DataStore	lds_Recupera

lds_Recupera	=	Create DataStore

dw_6.Retrieve(ai_cliente,ai_planta,al_numero,ai_especie,ai_variedad)

lds_Recupera.Dataobject =	'dw_mues_distribu_calibre'
lds_Recupera.SetTransObject(SQLca)

lds_Recupera.Retrieve(ai_especie,ai_variedad)

FOR ll_fila = 1 TO lds_Recupera.RowCount()
	 ll_secuen	=	lds_Recupera.Object.ccdc_secuen[ll_fila]
	 ll_variedad	=	lds_Recupera.Object.vari_codigo[ll_fila]
	 ls_calibre 	=	lds_Recupera.Object.ccdc_calibr[ll_fila]
		  
	 ll_fild	= dw_6.Find("ccdc_secuen = " + String(ll_secuen),  1, dw_6.RowCount())
	 	 				
  	 If ll_fild = 0 Then
		 ll_New = dw_6.InsertRow(0)
		 dw_6.Object.ccdc_secuen[ll_New] 	= ll_secuen
		 dw_6.Object.ccdc_calibr[ll_New] 		= ls_calibre
		 dw_6.Object.vari_codigo[ll_New] 		= ll_variedad
	End If
Next
	 
Return lb_Retorno

end function

public subroutine wf_desviacion_estandar (long al_fila, string as_columna);Decimal{2}	ld_valor, ld_null, ld_cuadra,ld_sumvar,ld_desvia, ld_sumvar1,var
String			ls_columna
Long			ll_fila
Integer  		ai_columna

dw_6.AcceptText()

SetNull(ld_null)
ai_columna	=	Integer(as_columna)

For ll_Fila= 1 to ai_columna
	If ll_fila >= 10 Then
		ls_columna	=	"cofi_mues"  + String(ll_fila)
	Else
		ls_columna	=	"cofi_mues0" +  String(ll_fila)
	End If
		
	ld_valor   =  dw_6.GetitemNumber(al_fila,ls_columna)

	ld_cuadra	= ld_cuadra + ld_valor^2	// suma muestras al cuadrado
	ld_sumvar	= (ld_sumvar + ld_valor)	// Suma Muestras
	ld_sumvar1	= ld_sumvar^2					// suma total de muestras al cuadrado
Next

If ai_columna > 1 Then
	var = ld_sumvar1/ai_columna
	ld_desvia   = sqrt(((ld_cuadra - var)/(ai_columna - 1)))
End If

dw_6.Object.cofi_desest[al_Fila] = ld_desvia	
end subroutine

public subroutine wf_child (integer especie, integer productor, integer predio, integer planta);dw_2.GetChild("plde_codigo", idwc_plantas)
idwc_plantas.SetTransObject(Sqlca)
If idwc_plantas.Retrieve() = 0 Then idwc_plantas.InsertRow(0)

dw_2.GetChild("espe_codigo", idwc_especies)
idwc_especies.SetTransObject(Sqlca)
If idwc_especies.Retrieve() = 0 Then idwc_especies.InsertRow(0)

dw_2.GetChild("vari_codigo", idwc_variedades)
idwc_variedades.SetTransObject(Sqlca)
If idwc_variedades.Retrieve(Especie) = 0 Then idwc_variedades.InsertRow(0)

dw_2.GetChild("prod_codigo", idwc_productores)
idwc_productores.SetTransObject(Sqlca)
If idwc_productores.Retrieve(-1) = 0 Then idwc_productores.InsertRow(0)

dw_2.GetChild("prpr_codigo", idwc_predios)
idwc_predios.SetTransObject(Sqlca)
If idwc_predios.Retrieve(Productor) = 0 Then idwc_predios.InsertRow(0)

dw_2.GetChild("prcc_codigo", idwc_cuarteles)
idwc_cuarteles.SetTransObject(Sqlca)
If idwc_cuarteles.Retrieve(Productor, Predio) = 0 Then idwc_cuarteles.InsertRow(0)

dw_2.GetChild("zona_codigo", idwc_zonas)
idwc_zonas.SetTransObject(Sqlca)
If idwc_zonas.Retrieve() = 0 Then idwc_zonas.InsertRow(0)

dw_2.GetChild("ccin_codigo", idwc_inspectores)
idwc_inspectores.SetTransObject(Sqlca)
If idwc_inspectores.Retrieve() = 0 Then idwc_inspectores.InsertRow(0)

dw_2.GetChild("cama_codigo", idwc_camaras)
idwc_camaras.SetTransObject(Sqlca)
If idwc_camaras.Retrieve(Planta) = 0 Then idwc_camaras.InsertRow(0)
end subroutine

public subroutine wf_calcula_libra (integer fila, string columna);Decimal{2}	ld_Total, ld_Frutos
String			ls_columna
Long			ll_Fila
Integer		li_Cuenta

dw_6.AcceptText()

For ll_fila= 1 To 20
	If ll_fila >= 10 Then
		ls_columna	=	"cofi_mues" +  String(ll_fila)
	Else
		ls_columna	=	"cofi_mues0" + String(ll_fila)
	End If
	
	If Not IsNull(dw_6.GetitemNumber(Fila,ls_columna)) And &
		(dw_6.GetitemNumber(Fila,ls_columna) > 0 And dw_6.GetitemNumber(Fila,ls_columna) < 10) Then li_cuenta++
		
	If IsNull(dw_4.Object.caco_totfru[1]) Or dw_4.Object.caco_totfru[1] = 0 Then 
		ld_Total	= 1
	Else
		ld_Total	= dw_4.Object.caco_totfru[1]
	End If
	
	ld_Frutos = (li_Cuenta * 100) / ld_Total
	dw_6.SetItem(Fila,"cofi_porfru",ld_Frutos)
Next
end subroutine

on w_maed_ctlcalplanillacondicionambiente.create
int iCurrent
call super::create
this.tab_1=create tab_1
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.tab_1
end on

on w_maed_ctlcalplanillacondicionambiente.destroy
call super::destroy
destroy(this.tab_1)
end on

event ue_seleccion;call super::ue_seleccion;istr_busq.argum[1]	=	String(iuo_Plantas.Codigo)
istr_busq.argum[2]	=	String(iuo_Especies.Codigo)
istr_busq.argum[4]	=	String(gi_CodExport)

OpenWithParm(w_busc_ctlcalplanillacondicionambiente, istr_busq)

istr_busq = Message.PowerObjectParm

If UpperBound(istr_busq.argum) > 4 Then
	If istr_busq.argum[5] <> "" Then
		iuo_Plantas.Codigo			= Integer(istr_Busq.Argum[1])
		iuo_Especies.Codigo		= Integer(istr_Busq.Argum[2])
		istr_mant.Argumento[1]	= istr_Busq.Argum[5]

		This.TriggerEvent("ue_recuperadatos")
	Else
		pb_buscar.SetFocus()
		HabilitaEncab(True)
	End If
End If
end event

event ue_recuperadatos;Long 		ll_fila_e, Respuesta

dw_2.SetTransObject(Sqlca)
Tab_1.TabPage_5.mle_observ.Text = ''

Do	
	ll_fila_e	= dw_2.Retrieve(gi_codexport, iuo_Plantas.Codigo, iuo_Especies.Codigo, Long(istr_mant.Argumento[1]))

	If ll_fila_e = -1 Then
		respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
										Information!, RetryCancel!)
	Else
		If ll_fila_e > 0 Then
			Do
				If iuo_especies.Codigo = 41 Then
					wf_recupera(dw_3, 'dw_mues_ctlcaltemperaturagases', 1, gi_Codexport, iuo_Plantas.codigo, iuo_Especies.Codigo, Long(istr_mant.Argumento[1]))
					wf_recupera(dw_4, 'dw_mues_ctlcalcondicionambiente', 2, gi_Codexport, iuo_Plantas.codigo, iuo_Especies.Codigo, Long(istr_mant.Argumento[1]))
					wf_recupera(dw_5, 'dw_mues_ctlcalfirmezacolumela', 3, gi_Codexport, iuo_Plantas.codigo, iuo_Especies.Codigo, Long(istr_mant.Argumento[1]))
					wf_recuperadistribucion(gi_Codexport, iuo_Plantas.codigo, iuo_Especies.Codigo, Long(istr_mant.Argumento[1]), dw_2.Object.vari_codigo[1])
					wf_calcula(dw_5, 2)
				End If
				
				wf_child(dw_2.Object.espe_codigo[1], dw_2.Object.prod_codigo[1], dw_2.Object.prpr_codigo[1], dw_2.Object.plde_codigo[1])
			Loop While Respuesta = 1

			Tab_1.TabPage_5.mle_observ.Text = dw_2.Object.coam_observ[1]
			
			pb_imprimir.Enabled	= True
			pb_grabar.Enabled	= True
			pb_eliminar.Enabled	= True
		End If
		
		If Respuesta = 2 Then Close(This)
	End If
Loop While Respuesta = 1

If Respuesta = 2 Then Close(This)
end event

event ue_nuevo;Integer  	li_Grupo, li_null
Long		ll_modif2, ll_modif3, ll_modif4, ll_modif5, ll_modif6

SetNull(li_null)
ib_ok	= True

istr_mant.Solo_Consulta	=	istr_mant.UsuarioSoloConsulta

IF Not istr_mant.Solo_Consulta THEN
	CHOOSE CASE wf_modifica()
		CASE -1
			ib_ok = False
		CASE 0
			ll_modif2		=	dw_2.GetNextModified(0, Primary!)
			ll_modif3		=	dw_3.GetNextModified(0, Primary!)
			ll_modif4		=	dw_4.GetNextModified(0, Primary!)	
			ll_modif5		=	dw_5.GetNextModified(0, Primary!)		
			ll_modif6		=	dw_6.GetNextModified(0, Primary!)
						
			IF dw_3.RowCount() > 0 OR dw_4.RowCount() > 0 OR dw_5.RowCount() > 0 OR &
			   dw_6.RowCount() > 0 THEN
				CHOOSE CASE MessageBox("Grabar registro(s)","Desea Grabar la información ?", Question!, YesNoCancel!)
					CASE 1
						Message.DoubleParm = 0
						This.TriggerEvent("ue_guardar")
						IF message.DoubleParm = -1 THEN ib_ok = False
					CASE 3
						ib_ok	= False
						RETURN
				END CHOOSE
			END IF
	END CHOOSE
END IF

IF Not ib_ok THEN RETURN

dw_3.Reset()
dw_4.Reset()
dw_5.Reset()
dw_6.Reset()

pb_grabar.Enabled		=	False
pb_eliminar.Enabled	=	False
pb_imprimir.Enabled	=	False
dw_2.Enabled			=	True

dw_2.SetRedraw(False)
dw_2.Reset()
dw_2.InsertRow(0)
dw_2.SetRedraw(True)

If iuo_especies.Codigo = 41 Then
	wf_recupera(dw_3, 'dw_mues_ctlcaltemperaturagases', 1, gi_Codexport, iuo_Plantas.codigo, iuo_Especies.Codigo, 0)
	wf_recupera(dw_4, 'dw_mues_ctlcalcondicionambiente', 2, gi_Codexport, iuo_Plantas.codigo, iuo_Especies.Codigo, 0)
	wf_recupera(dw_5, 'dw_mues_ctlcalfirmezacolumela', 3, gi_Codexport, iuo_Plantas.codigo, iuo_Especies.Codigo, 0)
	wf_recuperadistribucion(gi_Codexport, iuo_Plantas.codigo, 0, iuo_Especies.Codigo, -1)
End If

HabilitaEncab(True)

dw_2.Object.clie_codigo[1]	=	gi_CodExport
dw_2.Object.espe_codigo[1]	=	iuo_Especies.Codigo
dw_2.Object.zona_codigo[1]	=	iuo_Zonas.Codigo

dw_2.GetChild("prod_codigo", idwc_productores)
idwc_productores.SetTransObject(Sqlca)
If idwc_productores.Retrieve(iuo_Zonas.Codigo) = 0 Then idwc_productores.InsertRow(0)

dw_2.Object.plde_codigo[1]		=	iuo_Plantas.Codigo
dw_2.Object.coam_feccon[1]	=	Today()

li_Grupo = BuscaGrupo(Upper(Gstr_Us.Nombre))

If (li_Grupo > 2) Then
	pb_eliminar.Visible	=	False
	TriggerEvent('resize')
End If

istr_mant.argumento[1] = '0'
istr_mant.argumento[2] = '0'
istr_mant.argumento[3] = '0'
istr_mant.argumento[4] = '0'
istr_mant.argumento[5] = '0'
istr_mant.argumento[6] = '0'

dw_2.SetFocus()
dw_2.SetColumn("coam_numero")
Tab_1.TabPage_5.mle_observ.Text = ''
end event

event open;x				= 0
y				= 0
This.Height	= 2020
im_menu		= m_principal

This.Icon										=	Gstr_apl.Icono
This.ParentWindow().ToolBarVisible	=	True
im_menu.Item[1].Item[6].Enabled		=	True
im_menu.Item[7].Visible					=	True

pb_nuevo.PostEvent(Clicked!)

dw_3		=	tab_1.tabpage_1.dw_Temperatura
dw_4		=	tab_1.tabpage_2.dw_Condicion
dw_5		=	tab_1.tabpage_3.dw_firmeza
dw_6  	=  tab_1.tabpage_4.dw_psi

dw_2.SetTransObject(sqlca)
dw_3.SetTransObject(sqlca)
dw_4.SetTransObject(sqlca)
dw_5.SetTransObject(sqlca)
dw_6.SetTransObject(sqlca)

iuo_zonas       		= 	Create uo_zonas
iuo_plantas     		= 	Create uo_plantadesp
iuo_variedades  	= 	Create uo_variedades 
iuo_productor   	= 	Create uo_productores
iuo_inspectores  	= 	Create uo_ctlcalinspectores
iuo_especies   		= 	Create uo_especie
iuo_prodcuarteles	= 	Create uo_prodcuarteles
iuo_prodpredio    	= 	Create uo_prodpredio
iuo_camaras			= 	Create uo_camarasbode
iuo_Lotes			= 	Create uo_lotesfrutagranel

If Not iuo_Especies.Existe(Integer(Message.StringParm), True, Sqlca) Then Return
This.Title	= "Condicion Camaras AC: " + iuo_especies.Nombre
If Not iuo_Zonas.Existe(gi_codZona, True, Sqlca) Then Return
If Not iuo_Plantas.Existe(gi_codPlanta, True, Sqlca) Then Return

wf_child(iuo_Especies.Codigo, 0, 0, gi_codPlanta)

istr_mant.UsuarioSoloConsulta	=	OpcionSoloConsulta()

If Not wf_Visibilidad(iuo_Especies.Codigo) Then Return

GrabaAccesoAplicacion(True, id_FechaAcceso, it_HoraAcceso, This.Title, "Acceso a Aplicación", 1)
end event

event ue_imprimir;SetPointer(HourGlass!)

Long		Fila

DataWindowChild  dwc_packing

istr_info.titulo	=	"INFORME DE PARAMETROS CONDICION AMBIENTE"
istr_info.copias	=	1
OpenWithParm(vinf,istr_info)

If iuo_Especies.Codigo = 41 Then
	vinf.dw_1.DataObject	=	"dw_info_planillacondicionambiente" 
End If

vinf.dw_1.SetTransObject(sqlca)

fila	=	vinf.dw_1.Retrieve(gi_codexport, dw_2.Object.plde_codigo[1], iuo_Especies.Codigo, dw_2.Object.coam_numero[1], &
						-1,-1,-1,-1,-1,-1,-1,Date('19000101'),Today())

If Fila	=	-1 Then
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base de datos :~n" + sqlca.SQLErrText, StopSign!, Ok!)
ElseIf fila	=	0 Then
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
Else
	F_Membrete(vinf.dw_1)
	vinf.dw_1.Modify('DataWindow.Zoom = 94')
	If gs_Ambiente <> 'Windows' Then F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
End If

SetPointer(Arrow!)
end event

event resize;call super::resize;Integer	maximo, li_posic_x, li_posic_y, li_visible = 0

IF dw_2.width > il_AnchoDw_1 THEN
	maximo		=	dw_2.width
	
ELSE
	Tab_1.width	=	This.WorkSpaceWidth() - 400
	maximo		=	Tab_1.width
END IF

dw_2.x											= 37 + Round((This.WorkSpaceWidth() - dw_2.width - 400) / 2, 0)
dw_2.y											= 37

Tab_1.width									= This.WorkSpaceWidth() - 400

Tab_1.x											= 37 + Round((This.WorkSpaceWidth() - 400 - Tab_1.width ) / 2, 0)
Tab_1.y											= 64 + dw_2.Height

Tab_1.Height									= This.WorkSpaceHeight() - Tab_1.y - 41

Tab_1.TabPage_1.dw_Temperatura.Height	=	Tab_1.Height -	250
Tab_1.TabPage_2.dw_Condicion.Height		=	Tab_1.Height -	250
Tab_1.TabPage_3.dw_Firmeza.Height		=	Tab_1.Height -	250
Tab_1.TabPage_4.dw_psi.Height				=	Tab_1.Height -	250
Tab_1.TabPage_5.mle_observ.Height		=	Tab_1.Height -	250

Tab_1.TabPage_1.dw_Temperatura.Width	=	Tab_1.Width -	200
Tab_1.TabPage_2.dw_Condicion.Width		=	Tab_1.Width -	200
Tab_1.TabPage_3.dw_Firmeza.Width			=	Tab_1.Width -	200
Tab_1.TabPage_4.dw_psi.Width				=	Tab_1.Width -	200
Tab_1.TabPage_5.mle_observ.Width			=	Tab_1.Width -	200
end event

event ue_borrar;IF dw_2.RowCount() < 1 THEN RETURN

SetPointer(HourGlass!)

ib_borrar = True
w_main.SetMicroHelp("Validando la eliminación...")

Message.DoubleParm = 0

This.TriggerEvent ("ue_validaborrar")

IF Message.DoubleParm = -1 THEN RETURN

IF dw_3.RowCount() > 0 THEN dw_3.RowsMove(1,dw_3.RowCount(),Primary!,dw_3,1,Delete!)
IF dw_4.RowCount() > 0 THEN dw_4.RowsMove(1,dw_4.RowCount(),Primary!,dw_4,1,Delete!)
IF dw_5.RowCount() > 0 THEN dw_5.RowsMove(1,dw_5.RowCount(),Primary!,dw_5,1,Delete!)
IF dw_6.RowCount() > 0 THEN dw_6.RowsMove(1,dw_6.RowCount(),Primary!,dw_6,1,Delete!)

IF dw_2.DeleteRow(0) = 1 THEN
		ib_borrar = False
		w_main.SetMicroHelp("Borrando Registro...")
		IF wf_actualiza_db(True) THEN
			w_main.SetMicroHelp("Registro Borrado...")
			This.TriggerEvent("ue_nuevo")
			SetPointer(Arrow!)
		ELSE
			w_main.SetMicroHelp("Registro no Borrado...")
		END IF			
ELSE
	ib_borrar = False
	MessageBox(This.Title,"No se puede borrar actual registro.")
END IF

end event

event ue_antesguardar;call super::ue_antesguardar;Long	ll_Fila


For ll_Fila = 1 to dw_3.RowCount()
	dw_3.Object.clie_codigo[ll_Fila]		=	gi_CodExport
	dw_3.Object.plde_codigo[ll_Fila]		=	iuo_Plantas.Codigo
	dw_3.Object.espe_codigo[ll_Fila]		=	iuo_Especies.Codigo
	dw_3.Object.coam_numero[ll_Fila]		=	dw_2.Object.coam_numero[1]
Next

For ll_Fila = 1 to dw_4.RowCount()
	dw_4.Object.clie_codigo[ll_Fila]		=	gi_CodExport
	dw_4.Object.plde_codigo[ll_Fila]		=	iuo_Plantas.Codigo
	dw_4.Object.espe_codigo[ll_Fila]		=	iuo_Especies.Codigo
	dw_4.Object.coam_numero[ll_Fila]	=	dw_2.Object.coam_numero[1]
Next

For ll_Fila = 1 to dw_5.RowCount()
	dw_5.Object.clie_codigo[ll_Fila]		=	gi_CodExport
	dw_5.Object.plde_codigo[ll_Fila]		=	iuo_Plantas.Codigo
	dw_5.Object.espe_codigo[ll_Fila]		=	iuo_Especies.Codigo
	dw_5.Object.coam_numero[ll_Fila]	=	dw_2.Object.coam_numero[1]
Next

For ll_Fila = 1 to dw_6.RowCount()
	dw_6.Object.clie_codigo[ll_Fila]		=	gi_CodExport
	dw_6.Object.plde_codigo[ll_Fila]		=	iuo_Plantas.Codigo
	dw_6.Object.espe_codigo[ll_Fila]		=	iuo_Especies.Codigo
	dw_6.Object.coam_numero[ll_Fila]	=	dw_2.Object.coam_numero[1]
	dw_6.Object.vari_codigo[ll_Fila]		=	iuo_Variedades.Variedad
Next

dw_2.Object.coam_observ[1]				=	Tab_1.TabPage_5.mle_observ.Text
end event

type dw_1 from w_mant_encab_deta_csd`dw_1 within w_maed_ctlcalplanillacondicionambiente
boolean visible = false
integer x = 1358
integer y = 1784
integer width = 914
integer height = 612
boolean titlebar = false
string title = ""
boolean hscrollbar = false
boolean hsplitscroll = true
end type

type dw_2 from w_mant_encab_deta_csd`dw_2 within w_maed_ctlcalplanillacondicionambiente
integer x = 265
integer y = 36
integer width = 2318
integer height = 888
integer taborder = 10
string dataobject = "dw_mant_ctlcalcondicionambiente_enca"
boolean controlmenu = true
end type

event dw_2::itemchanged;String	ls_Columna, ls_Nula

SetNull(ls_Nula)
ls_Columna	=	dwo.Name

Choose Case ls_Columna
	Case "coam_numero"
		If wf_Planilla(Long(data), iuo_Plantas.Codigo) Then
			istr_mant.Argumento[1]	=	Data
			If ii_sw = 0 Then
				Parent.TriggerEvent("ue_recuperadatos")
			Else
				Messagebox("Atención","Planilla Digitada existe para otra Especie, no puede ingresar", Exclamation!, Ok!)
				dw_2.Object.pmen_numero.Protect	=	0
				dw_2.SetItem(1,"coam_numero",Integer(ls_Nula))
				Return 1
			End If
			dw_2.SetColumn("coam_numero")
			dw_2.SetFocus()
		End If

	Case "zona_codigo"
		If Not iuo_Zonas.Existe(Integer(data), True, Sqlca) Then
			This.SetItem(1, ls_Columna, Long(ls_nula))
			Return 1
		Else
			If idwc_productores.Retrieve(Integer(Data)) = 0 Then idwc_productores.InsertRow(0)
		End If

	Case "plde_codigo"
		If Not iuo_Plantas.Existe(Integer(data), True, Sqlca) Then
			This.SetItem(1, ls_Columna, Long(ls_nula))
			Return 1
		Else
			This.GetChild('cama_codigo', idwc_Camaras)
			idwc_Camaras.SetTransObject(sqlca)
			If idwc_Camaras.Retrieve(iuo_Plantas.Codigo) = 0 Then idwc_Camaras.InsertRow(0)
		End If
		
	Case "espe_codigo"
		If Not iuo_Especies.Existe(Integer(data), True, Sqlca) Then
			This.SetItem(1, ls_Columna, Long(ls_nula))
			Return 1
		Else
			This.GetChild('vari_codigo', idwc_Variedades)
			idwc_Variedades.SetTransObject(sqlca)
			If idwc_Variedades.Retrieve(iuo_Especies.Codigo) = 0 Then idwc_Variedades.InsertRow(0)
		End If

	Case "vari_codigo"
		If Not iuo_Variedades.Existe(iuo_Especies.Codigo, Integer(data), True, Sqlca) Then
			This.SetItem(1, ls_Columna, Long(ls_nula))
			Return 1
		Else
			wf_recuperadistribucion(gi_Codexport, iuo_Plantas.codigo, This.Object.coam_numero[1], This.Object.espe_codigo[1], Long(Data))
		End If
		
	Case "prod_codigo"
		If Not iuo_productor.Existe(Long(data),True, sqlca) Then
		     This.SetItem(1, ls_Columna, Long(ls_nula))
			Return 1
		Else
			dw_2.GetChild("prpr_codigo", idwc_predios)
			If idwc_predios.Retrieve(iuo_productor.Codigo) = 0 Then idwc_predios.InsertRow(0)
			dw_2.Object.prpr_codigo[1] = Integer(ls_Nula)
			
			dw_2.GetChild("prcc_codigo", idwc_cuarteles)
			If idwc_cuarteles.Retrieve(iuo_productor.Codigo, 0) = 0 Then idwc_cuarteles.InsertRow(0)
			dw_2.Object.prcc_codigo[1] = Integer(ls_Nula)		
		End IF	
		  
	Case "prpr_codigo"
		If Not iuo_prodpredio.existepredioprod(iuo_Productor.Codigo, Integer(data), True, sqlca) Then
			This.SetItem(1, ls_Columna, Long(ls_nula))
			Return 1
		Else
			dw_2.GetChild("prcc_codigo", idwc_cuarteles)
			If idwc_cuarteles.Retrieve(iuo_Productor.Codigo, iuo_prodpredio.Codigo) = 0 Then idwc_cuarteles.InsertRow(0)
			dw_2.Object.prcc_codigo[1] = Integer(ls_Nula)				
		End If
		  
	Case "prcc_codigo"
		IF Not iuo_prodcuarteles.existe(iuo_Productor.Codigo, iuo_prodpredio.Codigo, Integer(data), True, sqlca) Then
			This.SetItem(1, ls_Columna, Long(ls_nula))
			Return 1
		Else
			If iuo_Especies.Codigo = iuo_prodcuarteles.Especie Then
				If Not iuo_Variedades.Existe(iuo_Especies.Codigo, iuo_prodcuarteles.Variedad, True, Sqlca) Then
					This.SetItem(1, ls_Columna, Long(ls_nula))
					Return 1
				Else
					 This.Object.vari_codigo[Row] = iuo_prodcuarteles.Variedad
				End If
				
			Else
				MessageBox('Error...', 'Cuartel Seleccionado no es de Especie: ' + iuo_Especies.Nombre, StopSign!, Ok!)
				This.SetItem(1, ls_Columna, Long(ls_nula))
				Return 1
			End If
		End If
	
	Case "ccin_codigo"
		IF Not iuo_inspectores.existe(sqlca,Integer(data),True) Then
			This.SetItem(1, ls_Columna, Integer(ls_nula))
			Return 1
		End If
		
	Case "cama_codigo"
		IF Not iuo_camaras.existe(iuo_Plantas.Codigo, Integer(data),True, Sqlca) Then
			This.SetItem(1, ls_Columna, Integer(ls_nula))
			Return 1
		End If
		
	Case 'lote_codigo'
		If Not iuo_Lotes.Existe(iuo_Plantas.Codigo, iuo_Especies.Codigo, Long(data), True, Sqlca) Then
			This.SetItem(1, ls_Columna, Integer(ls_nula))
			Return 1
		Else
			This.GetChild("prpr_codigo", idwc_predios)
			idwc_predios.SetTransObject(Sqlca)
			If idwc_predios.Retrieve(iuo_Lotes.Productor) = 0 Then idwc_predios.InsertRow(0)
			
			This.GetChild("prcc_codigo", idwc_cuarteles)
			idwc_cuarteles.SetTransObject(Sqlca)
			If idwc_cuarteles.Retrieve(iuo_Lotes.Productor, iuo_Lotes.Predio) = 0 Then idwc_cuarteles.InsertRow(0)
			iuo_Variedades.Existe(This.Object.espe_codigo[1], iuo_Lotes.Variedad, False, Sqlca)
			
			wf_recuperadistribucion(gi_Codexport, This.Object.plde_codigo[1], This.Object.coam_numero[1], This.Object.espe_codigo[1], iuo_Lotes.Variedad)
			
			This.Object.lote_codigo[1]	=	iuo_Lotes.Numero
			This.Object.prod_codigo[1]	=	iuo_Lotes.Productor
			This.Object.vari_codigo[1]	=	iuo_Lotes.Variedad
			This.Object.prpr_codigo[1]	=	iuo_Lotes.Predio
			This.Object.prcc_codigo[1] 	=	iuo_Lotes.CentroCosto
			This.Object.coam_totbul[1]	=	iuo_Lotes.TotalBulto
		End If
		
End Choose

habilitaingreso(ls_Columna)
end event

event dw_2::sqlpreview;//
end event

event dw_2::buttonclicked;call super::buttonclicked;String	ls_Boton

ls_Boton = dwo.Name

Choose Case ls_Boton
	Case 'b_lote'
		wf_BuscaLote()
		
End Choose
end event

type pb_nuevo from w_mant_encab_deta_csd`pb_nuevo within w_maed_ctlcalplanillacondicionambiente
integer x = 4736
integer y = 416
integer taborder = 40
end type

type pb_eliminar from w_mant_encab_deta_csd`pb_eliminar within w_maed_ctlcalplanillacondicionambiente
integer x = 4736
integer y = 596
integer taborder = 60
end type

type pb_grabar from w_mant_encab_deta_csd`pb_grabar within w_maed_ctlcalplanillacondicionambiente
integer x = 4736
integer y = 780
integer taborder = 50
end type

type pb_imprimir from w_mant_encab_deta_csd`pb_imprimir within w_maed_ctlcalplanillacondicionambiente
integer x = 4736
integer y = 952
integer taborder = 70
end type

type pb_salir from w_mant_encab_deta_csd`pb_salir within w_maed_ctlcalplanillacondicionambiente
integer x = 4736
integer y = 1136
integer taborder = 80
end type

type pb_ins_det from w_mant_encab_deta_csd`pb_ins_det within w_maed_ctlcalplanillacondicionambiente
boolean visible = false
integer x = 4736
integer y = 1428
integer taborder = 0
end type

type pb_eli_det from w_mant_encab_deta_csd`pb_eli_det within w_maed_ctlcalplanillacondicionambiente
boolean visible = false
integer x = 4741
integer y = 1600
integer taborder = 0
end type

type pb_buscar from w_mant_encab_deta_csd`pb_buscar within w_maed_ctlcalplanillacondicionambiente
integer x = 4736
integer y = 236
integer taborder = 30
boolean default = true
end type

type tab_1 from tab within w_maed_ctlcalplanillacondicionambiente
integer x = 69
integer y = 1000
integer width = 4576
integer height = 1136
integer taborder = 20
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16777215
boolean multiline = true
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

on tab_1.create
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

on tab_1.destroy
destroy(this.tabpage_1)
destroy(this.tabpage_2)
destroy(this.tabpage_3)
destroy(this.tabpage_4)
destroy(this.tabpage_5)
end on

type tabpage_1 from userobject within tab_1
integer x = 18
integer y = 168
integer width = 4539
integer height = 952
long backcolor = 16777215
string text = "Temperatura y~r~nNivel de Gases"
long tabtextcolor = 33554432
long tabbackcolor = 30586022
long picturemaskcolor = 30586022
dw_temperatura dw_temperatura
end type

on tabpage_1.create
this.dw_temperatura=create dw_temperatura
this.Control[]={this.dw_temperatura}
end on

on tabpage_1.destroy
destroy(this.dw_temperatura)
end on

type dw_temperatura from uo_dw within tabpage_1
integer x = 37
integer y = 28
integer width = 1285
integer height = 896
integer taborder = 11
string dataobject = "dw_mues_ctlcalanalisistemperatura"
boolean hscrollbar = true
boolean hsplitscroll = true
boolean livescroll = true
end type

event sqlpreview;//
end event

type tabpage_2 from userobject within tab_1
integer x = 18
integer y = 168
integer width = 4539
integer height = 952
long backcolor = 16777215
string text = "Inspección de~r~nCondición"
long tabtextcolor = 33554432
long tabbackcolor = 12632256
long picturemaskcolor = 536870912
dw_condicion dw_condicion
end type

on tabpage_2.create
this.dw_condicion=create dw_condicion
this.Control[]={this.dw_condicion}
end on

on tabpage_2.destroy
destroy(this.dw_condicion)
end on

type dw_condicion from uo_dw within tabpage_2
integer x = 37
integer y = 28
integer width = 2624
integer height = 896
integer taborder = 11
string dataobject = "dw_mues_ctlcalanalisiscondicion"
boolean hscrollbar = true
boolean hsplitscroll = true
boolean livescroll = true
end type

event sqlpreview;//
end event

event itemchanged;call super::itemchanged;String	ls_Columna
Long     ll_fila, ll_total, ll_defectos

ls_Columna	=	dwo.Name

Choose Case ls_Columna
	Case "caco_totfru"
		For ll_fila = 2 To dw_4.RowCount()
			dw_4.Object.caco_totfru[ll_fila] =  Long(data)
		Next
		
	Case "caco_cantid"
		dw_4.Object.caco_totfru[row] =  dw_4.Object.caco_totfru[1]

End Choose
end event

type tabpage_3 from userobject within tab_1
integer x = 18
integer y = 168
integer width = 4539
integer height = 952
long backcolor = 16777215
string text = "Firmeza~r~nMaduracion Forzada"
long tabtextcolor = 33554432
long tabbackcolor = 12632256
long picturemaskcolor = 536870912
dw_firmeza dw_firmeza
end type

on tabpage_3.create
this.dw_firmeza=create dw_firmeza
this.Control[]={this.dw_firmeza}
end on

on tabpage_3.destroy
destroy(this.dw_firmeza)
end on

type dw_firmeza from uo_dw within tabpage_3
integer x = 37
integer y = 28
integer width = 1381
integer height = 896
integer taborder = 11
string dataobject = "dw_mues_ctlcalanalisisfirmezacolumela"
boolean hsplitscroll = true
boolean livescroll = true
end type

event itemchanged;call super::itemchanged;String		ls_Columna, ls_colu
Integer	li_numero

ls_Columna	=	dwo.Name

Choose Case ls_Columna
	Case "coco_valore"
		wf_Calcula(This, 2)
		
	Case "coco_colume"
		wf_Calcula(This, 2)
End Choose
end event

event sqlpreview;//
end event

type tabpage_4 from userobject within tab_1
integer x = 18
integer y = 168
integer width = 4539
integer height = 952
long backcolor = 16777215
string text = "Firmeza por~r~nCalibres"
long tabtextcolor = 33554432
long tabbackcolor = 12632256
long picturemaskcolor = 536870912
dw_psi dw_psi
end type

on tabpage_4.create
this.dw_psi=create dw_psi
this.Control[]={this.dw_psi}
end on

on tabpage_4.destroy
destroy(this.dw_psi)
end on

type dw_psi from datawindow within tabpage_4
accessiblerole accessiblerole = cursorrole!
integer x = 37
integer y = 28
integer width = 4466
integer height = 896
integer taborder = 30
string dataobject = "dw_mues_ctlcalfirmezapsi"
boolean vscrollbar = true
boolean hsplitscroll = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event itemchanged;String		ls_Columna, ls_colu, ls_Null
Integer	li_numero

SetNull(ls_Columna)
ls_Columna	=	dwo.Name

Choose Case ls_Columna
	Case "cofi_mues01", "cofi_mues02", "cofi_mues03", "cofi_mues04","cofi_mues05", &
	  	   "cofi_mues06", "cofi_mues07", "cofi_mues08", "cofi_mues09","cofi_mues10",&
  		   "cofi_mues11", "cofi_mues12", "cofi_mues13", "cofi_mues14","cofi_mues15", &
	  	   "cofi_mues16", "cofi_mues17", "cofi_mues18", "cofi_mues19","cofi_mues20"
	
			If IsNull(data) Then
				If Integer(Mid(ls_columna,10,2))>= 1 Then
					li_Numero	=	Integer(Mid(ls_columna,10,2)) - 1
					ls_colu 		= 	Fill("0",2 - Len(String(li_Numero))) + String(li_Numero)
					istr_mant.argumento[3] = ls_Colu
				End If
			End If
			
			If IsNull(dw_4.Object.caco_totfru[1]) Or dw_4.Object.caco_totfru[1] = 0 Then 
				MessageBox('Atencion', 'No se ha ingresado el total de frutos, debe ingresarlo para continuar', StopSign!, OK!)
				This.SetItem(Row, ls_columna, Integer(ls_Null))
				tab_1.tabpage_2.dw_Condicion.SetFocus()
				Return 1
			End If
				  
			wf_Calcula_Promedio(Row, Mid(ls_columna,10,2))
			wf_Desviacion_Estandar(Row,Mid(ls_columna,10,2))
			wf_Calcula_Libra(Row,Mid(ls_columna,10,2))
End Choose
end event

event itemerror;Return 1
end event

type tabpage_5 from userobject within tab_1
integer x = 18
integer y = 168
integer width = 4539
integer height = 952
long backcolor = 16777215
string text = "Observaciones"
long tabtextcolor = 33554432
long tabbackcolor = 12632256
long picturemaskcolor = 536870912
mle_observ mle_observ
end type

on tabpage_5.create
this.mle_observ=create mle_observ
this.Control[]={this.mle_observ}
end on

on tabpage_5.destroy
destroy(this.mle_observ)
end on

type mle_observ from multilineedit within tabpage_5
integer x = 37
integer y = 28
integer width = 4466
integer height = 896
integer taborder = 20
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
boolean autovscroll = true
integer limit = 600
borderstyle borderstyle = stylelowered!
end type

