$PBExportHeader$w_maed_ctlcalplanillamadurez.srw
$PBExportComments$Ingresador de antecedentes para Planilla Cuantitativa por especies.
forward
global type w_maed_ctlcalplanillamadurez from w_mant_encab_deta_csd
end type
type tab_1 from tab within w_maed_ctlcalplanillamadurez
end type
type tabpage_7 from userobject within tab_1
end type
type dw_peso from uo_dw within tabpage_7
end type
type tabpage_7 from userobject within tab_1
dw_peso dw_peso
end type
type tabpage_8 from userobject within tab_1
end type
type dw_firmeza from uo_dw within tabpage_8
end type
type tabpage_8 from userobject within tab_1
dw_firmeza dw_firmeza
end type
type tabpage_3 from userobject within tab_1
end type
type dw_solidossolubles from uo_dw within tabpage_3
end type
type tabpage_3 from userobject within tab_1
dw_solidossolubles dw_solidossolubles
end type
type tabpage_2 from userobject within tab_1
end type
type dw_colorsemilla from uo_dw within tabpage_2
end type
type tabpage_2 from userobject within tab_1
dw_colorsemilla dw_colorsemilla
end type
type tabpage_1 from userobject within tab_1
end type
type dw_materiaseca from uo_dw within tabpage_1
end type
type tabpage_1 from userobject within tab_1
dw_materiaseca dw_materiaseca
end type
type tabpage_4 from userobject within tab_1
end type
type dw_brotrytis from datawindow within tabpage_4
end type
type tabpage_4 from userobject within tab_1
dw_brotrytis dw_brotrytis
end type
type tabpage_5 from userobject within tab_1
end type
type dw_categoriafruta from datawindow within tabpage_5
end type
type tabpage_5 from userobject within tab_1
dw_categoriafruta dw_categoriafruta
end type
type tabpage_6 from userobject within tab_1
end type
type dw_madurez from uo_dw within tabpage_6
end type
type tabpage_6 from userobject within tab_1
dw_madurez dw_madurez
end type
type tab_1 from tab within w_maed_ctlcalplanillamadurez
tabpage_7 tabpage_7
tabpage_8 tabpage_8
tabpage_3 tabpage_3
tabpage_2 tabpage_2
tabpage_1 tabpage_1
tabpage_4 tabpage_4
tabpage_5 tabpage_5
tabpage_6 tabpage_6
end type
end forward

global type w_maed_ctlcalplanillamadurez from w_mant_encab_deta_csd
integer width = 3758
integer height = 2576
string menuname = ""
windowstate windowstate = maximized!
event ue_validapassword ( )
tab_1 tab_1
end type
global w_maed_ctlcalplanillamadurez w_maed_ctlcalplanillamadurez

type variables
DataWindowChild	idwc_plantas, idwc_especies, idwc_zonas, idwc_productores, &
						idwc_predios, idwc_cuarteles, idwc_variedades, idwc_inspectores

Datawindow    		dw_3,dw_4,dw_5,dw_6,dw_7, dw_8, dw_9, dw_10

uo_zonas             		iuo_zonas
uo_plantadesp        	iuo_plantas
uo_variedades        	iuo_variedades
uo_productores       	iuo_productor
uo_ctlcalinspectores	iuo_inspectores
uo_especie         	 	iuo_especies
uo_prodcuarteles     	iuo_prodcuarteles
uo_prodpredio			iuo_prodpredio

Integer 	ii_especie, ii_sw, il_valida //este sw controla si la planilla existe con otra especie
String	is_columna




end variables

forward prototypes
protected function integer wf_modifica ()
public subroutine habilitaencab (boolean habilita)
public subroutine habilitaingreso (string ls_columna)
protected function boolean wf_actualiza_db (boolean borrando)
public function boolean wf_visibilidad (integer especie)
public function long wf_recupera (datawindow adw, string objeto, integer tipo, integer cliente, integer planta, integer especie, long numero)
public subroutine wf_calcula_color ()
public subroutine wf_child ()
public function boolean wf_planilla (long planilla, integer planta)
public function boolean duplicado (string valor)
public subroutine wf_desviacion_estandar (long al_fila, string as_columna, integer tipo, datawindow adw)
public subroutine wf_calcula_promedio (long fila, string columna, integer tipo, datawindow adw)
public subroutine wf_calcula_minmax (long al_fila, string as_columna, integer tipo, datawindow adw, boolean minimo)
public function boolean wf_filtra (integer ai_especie, datawindowchild adwc)
public function any wf_valida (datawindow adw, integer fila, string columna, string valor)
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
	dw_2.Object.pmen_numero.Protect				=	0
	dw_2.Object.zona_codigo.Protect					=	0
	dw_2.Object.plde_codigo.Protect					=	0
	dw_2.Object.vari_codigo.Protect					=	0
	dw_2.Object.prod_codigo.Protect					=	0
	dw_2.Object.prpr_codigo.Protect					=	0
	dw_2.Object.prcc_codigo.Protect					=	0
	dw_2.Object.ccin_codigo.Protect					=	0
	dw_2.Object.pmen_numero.BackGround.Color	=	RGB(255,255,255)
	dw_2.Object.zona_codigo.BackGround.Color	=	RGB(255,255,255)
	dw_2.Object.plde_codigo.BackGround.Color		=	RGB(255,255,255)
	dw_2.Object.vari_codigo.BackGround.Color		=	RGB(255,255,255)
	dw_2.Object.prod_codigo.BackGround.Color		=	RGB(255,255,255)
	dw_2.Object.prpr_codigo.BackGround.Color		=	RGB(255,255,255)
	dw_2.Object.prcc_codigo.BackGround.Color		=	RGB(255,255,255)
	dw_2.Object.ccin_codigo.BackGround.Color		=	RGB(255,255,255)
  	pb_buscar.Enabled   									= True
Else
	dw_2.Object.pmen_numero.Protect				=	1
	dw_2.Object.zona_codigo.Protect					=	1
	dw_2.Object.plde_codigo.Protect					=	1
	dw_2.Object.vari_codigo.Protect					=	1
	dw_2.Object.prod_codigo.Protect					=	1
	dw_2.Object.prpr_codigo.Protect					=	1
	dw_2.Object.prcc_codigo.Protect					=	1
	dw_2.Object.ccin_codigo.Protect					=	1
	dw_2.Object.pmen_numero.BackGround.Color	=	553648127
	dw_2.Object.zona_codigo.BackGround.Color	=	553648127
	dw_2.Object.plde_codigo.BackGround.Color		=	553648127
	dw_2.Object.vari_codigo.BackGround.Color		=	553648127
	dw_2.Object.prod_codigo.BackGround.Color		=	553648127
	dw_2.Object.prpr_codigo.BackGround.Color		=	553648127
	dw_2.Object.prcc_codigo.BackGround.Color		=	553648127
	dw_2.Object.ccin_codigo.BackGround.Color		=	553648127
	pb_buscar.Enabled   									= False
End If
end subroutine

public subroutine habilitaingreso (string ls_columna);Boolean	lb_Estado = True

dw_2.AcceptText()

If ls_Columna <> "zona_codigo" And &
	(dw_2.Object.zona_codigo[1]) = 0 Or IsNull(dw_2.Object.zona_codigo[1]) Then
	lb_Estado	=	False
End If

If ls_Columna <> "plde_codigo" And &
	(dw_2.Object.plde_codigo[1]) = 0 Or IsNull(dw_2.Object.plde_codigo[1]) Then
	lb_Estado	=	False
End If

If ls_Columna <> "vari_codigo" And &
	(dw_2.Object.vari_codigo[1]) = 0 Or IsNull(dw_2.Object.vari_codigo[1]) Then
	lb_Estado	=	False
End If
	
If ls_Columna <> "prod_codigo" And &
	(dw_2.Object.prod_codigo[1]) = 0 Or IsNull(dw_2.Object.prod_codigo[1]) Then
	lb_Estado	=	False
End If

If iuo_Especies.Codigo <> 26 And iuo_Especies.Codigo <> 27 And iuo_Especies.Codigo <> 78 Then
	If ls_Columna <> "ccin_codigo" And &
		(dw_2.Object.ccin_codigo[1]) = 0 Or IsNull(dw_2.Object.ccin_codigo[1]) Then
		lb_Estado	=	False
	End If
ElseIf iuo_Especies.Codigo =  81 Then
	If ls_Columna <> "prpr_codigo" And &
		(dw_2.Object.prpr_codigo[1]) = 0 Or IsNull(dw_2.Object.prpr_codigo[1]) Then
		lb_Estado	=	False
	End If

	If ls_Columna <> "prcc_codigo" And &
		(dw_2.Object.prcc_codigo[1]) = 0 Or IsNull(dw_2.Object.prcc_codigo[1]) Then
		lb_Estado	=	False
	End If
End If

If ls_Columna <> "pmen_numero" And &
	(dw_2.Object.pmen_numero[1]) = 0 Or IsNull(dw_2.Object.pmen_numero[1])  Then
	lb_Estado	=	False
End If

pb_grabar.Enabled = lb_Estado
end subroutine

protected function boolean wf_actualiza_db (boolean borrando);Boolean	lb_AutoCommit, lb_Retorno

IF Not dw_2.uf_check_required(0) THEN RETURN False

lb_AutoCommit		=	sqlca.AutoCommit
sqlca.AutoCommit	=	False

IF Borrando THEN
	IF dw_10.Update(True, False) = 1 THEN
		IF dw_9.Update(True, False) = 1 THEN
			IF dw_8.Update(True, False) = 1 THEN
				IF dw_7.Update(True, False) = 1 THEN
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
											
											dw_10.ResetUpdate()
											dw_9.ResetUpdate()
											dw_8.ResetUpdate()
											dw_7.ResetUpdate()
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
					F_ErrorBaseDatos(sqlca, This.Title)
					RollBack;
				END IF
			ELSE
				F_ErrorBaseDatos(sqlca, This.Title)
				RollBack;
			END If
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
					  IF dw_7.Update(True,False) =	1	THEN
							IF dw_8.Update(True,False) =	1	THEN
								IF dw_9.Update(True,False) =	1	THEN
									IF dw_10.Update(True,False) =	1	THEN
										
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
											dw_7.ResetUpdate()
											dw_8.ResetUpdate()
											dw_9.ResetUpdate()
											dw_10.ResetUpdate()
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

Return lb_Retorno
end function

public function boolean wf_visibilidad (integer especie);Boolean	lb_Retorno = True

Choose Case Especie
	Case 81
		dw_3.DataObject	=	'dw_mues_ctlcalanalisismateriaseca'
		dw_3.SetTransObject(sqlca)
		
		Tab_1.TabPage_1.Visible	=	True
		Tab_1.TabPage_2.Visible	=	False
		Tab_1.TabPage_3.Visible	=	False
		Tab_1.TabPage_4.Visible	=	False
		Tab_1.TabPage_5.Visible	=	False
		Tab_1.TabPage_6.Visible	=	False
		Tab_1.TabPage_7.Visible	=	False
		Tab_1.TabPage_8.Visible	=	False
		
	Case 41
		dw_3.DataObject	=	'dw_mues_ctlcalanalisismateriaseca_kiwi'
		dw_3.SetTransObject(sqlca)
		
		Tab_1.TabPage_1.Visible				=	True
		Tab_1.TabPage_2.Visible				=	True
		Tab_1.TabPage_3.Visible				=	True
		Tab_1.TabPage_4.Visible				=	True
		Tab_1.TabPage_5.Visible				=	False
		Tab_1.TabPage_6.Visible				=	False
		Tab_1.TabPage_7.Visible				=	True
		Tab_1.TabPage_8.Visible				=	True
		dw_2.Object.pmen_horaan_t.Visible	=	False
		dw_2.Object.pmen_horaan.Visible		=	False
		dw_2.Object.ccin_codigo.x				=	dw_2.Object.pmen_horaan.x
		dw_2.Object.ccin_codigo.y				=	dw_2.Object.pmen_horaan.y
		dw_2.Object.ccin_codigo_t.x			=	dw_2.Object.pmen_horaan_t.x
		dw_2.Object.ccin_codigo_t.y			=	dw_2.Object.pmen_horaan_t.y
		
	Case 26, 27	
		dw_2.DataObject	=	'dw_mant_ctlcalparammadurezenca_citrico'
		dw_2.SetTransObject(sqlca)
		dw_2.Height 		= 1064
		Tab_1.y				= 1116
		This.Height 			= 2308
		
		Tab_1.TabPage_1.Visible				=	False
		Tab_1.TabPage_2.Visible				=	False
		Tab_1.TabPage_3.Visible				=	False
		Tab_1.TabPage_4.Visible				=	False
		Tab_1.TabPage_5.Visible				=	False
		Tab_1.TabPage_6.Visible				=	True
		Tab_1.TabPage_7.Visible				=	False
		Tab_1.TabPage_8.Visible				=	False
		dw_2.Object.pmen_horaan_t.Visible	=	False
		dw_2.Object.pmen_horaan.Visible		=	False		
		dw_2.Object.ccin_codigo.Visible		=	False
		dw_2.Object.ccin_codigo_t.Visible		=	False
		
		
End Choose

Return lb_Retorno
end function

public function long wf_recupera (datawindow adw, string objeto, integer tipo, integer cliente, integer planta, integer especie, long numero);DataStore	lds_Recupera
Long    		ll_Retorno, ll_Fila, ll_Busca, ll_New, ll_Recupera
String			ls_Titulo, ls_Prefijo, ls_Codigo
Integer		li_Codigo,li_val

lds_Recupera	=	Create DataStore

adw.Retrieve(Cliente, Planta, Especie, Numero)

lds_Recupera.DataObject =	Objeto
lds_Recupera.SetTransObject(Sqlca)

Choose Case Tipo
	Case 1
		ls_Prefijo = 'mase_'

	Case 2
		ls_Prefijo = 'cose_'
		
	Case 3
		ls_Prefijo = 'soso_'
		
	Case 4
		ls_Prefijo = 'brot_'
		
	Case 5
		ls_Prefijo = 'ctcg_'

	Case 6
		ls_Prefijo = 'pefr_'

	Case 7
		ls_Prefijo = 'firm_'
		
	Case 8
		ls_Prefijo = 'madu_'
		
End Choose 

Choose Case Especie 
	Case 41,81	
		ll_Recupera	=	 lds_Recupera.Retrieve(Especie,1)
	Case Else	
		ll_Recupera	=	 lds_Recupera.Retrieve(Especie)
End Choose 

If ll_Recupera = 0 Then
	MessageBox('Alerta...', 'No Existen Titulos para Opción.', StopSign!, OK!)	
	ll_Retorno = -1
Else
	For ll_fila = 1 To lds_Recupera.RowCount()
		If Tipo = 5 Then
			ls_Codigo	=	lds_Recupera.GetItemString(ll_Fila, 2)
		Else
			li_Codigo	=	lds_Recupera.GetItemNumber(ll_Fila, 2)
		End If
		ls_Titulo	= lds_Recupera.GetItemString(ll_Fila, 3)
		
		If Tipo = 5 Then
			ll_Busca	= adw.Find(ls_Prefijo + "Codigo = '" + ls_Codigo + "'", 1, adw.RowCount())
		Else
			ll_Busca	= adw.Find(ls_Prefijo + "Codigo = " + String(li_Codigo), 1, adw.RowCount())
		End If
		
	  	If ll_Busca = 0 Then
			 ll_New	=	adw.InsertRow(0)
			 If Tipo = 5 Then
				 adw.SetItem(ll_New, ls_Prefijo + 'codigo', ls_Codigo)
			Else
				 adw.SetItem(ll_New, ls_Prefijo + 'codigo', li_Codigo)
			End If
			 adw.SetItem(ll_new, ls_Prefijo + 'nombre', ls_titulo)
			 If Tipo = 3 Or Tipo = 8 Then
				adw.SetItem(ll_new, ls_Prefijo + 'candig', lds_Recupera.GetItemNumber(ll_Fila, 4))
			 End If
			 
			 If Tipo = 1 Then /*Recupera valores validación %Aceite para Especie Palta*/
				adw.SetItem(ll_new, ls_Prefijo + 'valida', lds_Recupera.GetItemNumber(ll_Fila, 4))
				adw.SetItem(ll_new, ls_Prefijo + 'valore', lds_Recupera.GetItemNumber(ll_Fila,5))
			End If
		End If
	Next
	ll_Retorno = adw.RowCount()
End If

Destroy lds_Recupera	

Return ll_Retorno
end function

public subroutine wf_calcula_color ();Long		ll_Fila
Dec{2}	ld_Valor

If dw_4.RowCount() < 1 Then Return

ld_Valor = 0

For ll_Fila= 1 To dw_4.RowCount()
	ld_Valor += dw_4.Object.ancs_blanca[ll_Fila] + dw_4.Object.ancs_pardas[ll_Fila] + dw_4.Object.ancs_negras[ll_Fila]
Next

If Not IsNull(ld_Valor) Then
	If ld_Valor <> 100 Then
		Messagebox('Alerta', 'La sumatoria debe ser igual a 100%')
	End If
End If
end subroutine

public subroutine wf_child ();dw_2.GetChild("plde_codigo", idwc_plantas)
idwc_plantas.SetTransObject(Sqlca)
If idwc_plantas.Retrieve() = 0 Then idwc_plantas.InsertRow(0)

dw_2.GetChild("espe_codigo", idwc_especies)
idwc_especies.SetTransObject(Sqlca)
If idwc_especies.Retrieve() = 0 Then idwc_especies.InsertRow(0)

dw_2.GetChild("vari_codigo", idwc_variedades)
idwc_variedades.SetTransObject(Sqlca)
If idwc_variedades.Retrieve(iuo_Especies.Codigo) = 0 Then idwc_variedades.InsertRow(0)

dw_2.GetChild("prod_codigo", idwc_productores)
idwc_productores.SetTransObject(Sqlca)
If idwc_productores.Retrieve(iuo_Zonas.Codigo) = 0 Then idwc_productores.InsertRow(0)

dw_2.GetChild("prpr_codigo", idwc_predios)
idwc_predios.SetTransObject(Sqlca)
If idwc_predios.Retrieve(0) = 0 Then idwc_predios.InsertRow(0)

dw_2.GetChild("prcc_codigo", idwc_cuarteles)
idwc_cuarteles.SetTransObject(Sqlca)
If idwc_cuarteles.Retrieve(0, 0) = 0 Then idwc_cuarteles.InsertRow(0)

dw_2.GetChild("zona_codigo", idwc_zonas)
idwc_zonas.SetTransObject(Sqlca)
If idwc_zonas.Retrieve() = 0 Then idwc_zonas.InsertRow(0)

dw_2.GetChild("ccin_codigo", idwc_inspectores)
idwc_inspectores.SetTransObject(Sqlca)
If idwc_inspectores.Retrieve() = 0 Then idwc_inspectores.InsertRow(0)
end subroutine

public function boolean wf_planilla (long planilla, integer planta);Long    ll_existe, ll_productor
Integer li_variedad, li_especie, li_zona, li_predio

ii_sw = 0

SELECT pmen_numero, vari_codigo, espe_codigo,  prod_codigo, zona_codigo, prpr_codigo
	INTO :ll_existe, :li_variedad, :li_especie, :ll_productor,:li_zona, :li_predio
	FROM dba.ctlcalparamadurezenca
	WHERE	clie_codigo = :gi_codexport
	AND 		plde_codigo = :Planta 
	AND 		pmen_numero = :Planilla
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

public function boolean duplicado (string valor);Long		ll_fila
Date		ld_fecha

ld_fecha = Date(valor)

ll_fila	= dw_8.Find ("String(pmde_fecham) = '" + String(ld_fecha) + "'", 1, dw_8.RowCount())

IF ll_fila > 0  THEN
	MessageBox("Error","Registro ya fue ingresado anteriormente",Information!, Ok!)
	RETURN True
ELSE
	RETURN False
END IF

end function

public subroutine wf_desviacion_estandar (long al_fila, string as_columna, integer tipo, datawindow adw);Decimal{2}	ld_valor, ld_null, ld_cuadra,ld_sumvar,ld_desvia, ld_sumvar1,var
String			ls_columna, ls_Prefijo
Long			ll_fila, ll_Tope
Integer  		ai_columna

adw.AcceptText()

SetNull(ld_null)
ai_columna	=	Integer(as_columna)

If iuo_Especies.Codigo = 41 Then
	ll_Tope = 30
Else
	ll_Tope = 10
End If

If Tipo = 1 Then
	ls_Prefijo = 'anms'
Elseif Tipo = 2 Then
	ls_Prefijo = 'anss'
ElseIf Tipo = 3 Then
	ls_Prefijo = 'anpf'
ElseIf Tipo = 4 Then
	ls_Prefijo = 'anfi'
ElseIf Tipo = 5 Then
	ls_Prefijo = 'anma'
End If	

For ll_Fila= 1 to ai_columna
	If ll_fila >= 10 Then
		ls_columna	=	ls_Prefijo + "_mues"  + String(ll_Fila)
	Else
		ls_columna	=	ls_Prefijo + "_mues" + String(0) + String(ll_Fila)
	End If
		
	ld_valor   =  adw.GetitemNumber(al_fila,ls_columna)

	ld_cuadra	= ld_cuadra + ld_valor^2	// suma muestras al cuadrado
	ld_sumvar	= (ld_sumvar + ld_valor)		// Suma Muestras
	ld_sumvar1	= ld_sumvar^2					// suma total de muestras al cuadrado
Next

If ai_columna > 1 Then
	var = ld_sumvar1/ai_columna
	ld_desvia   = sqrt(((ld_cuadra - var)/(ai_columna - 1)))
End If

adw.SetItem(al_Fila, ls_Prefijo + '_desest', ld_desvia)
end subroutine

public subroutine wf_calcula_promedio (long fila, string columna, integer tipo, datawindow adw);Decimal{2}	ld_valor,ld_media,ld_suma, ld_null
String			ls_columna, ls_Prefijo
Long			ll_fila, ll_Tope, ll_CantiDigi
Integer		ai_columna, li_Cuenta, li_Div

adw.AcceptText()

SetNull(ld_null)
ai_columna	=	Integer(Columna)

If iuo_Especies.Codigo = 41 Then
	If Tipo = 4 Then
		ll_Tope = 60
	Else
		ll_Tope = 30
	End If	
Else
	ll_Tope = 10
End If

If Tipo = 1 Then
	ls_Prefijo = 'anms'
Elseif Tipo = 2 Then
	ls_Prefijo = 'anss'
ElseIf Tipo = 3 Then
	ls_Prefijo = 'anpf'
ElseIf Tipo = 4 Then
	ls_Prefijo = 'anfi'
ElseIf Tipo = 5 Then
	ls_Prefijo = 'anma'
End If	

IF Integer(istr_mant.argumento[3]) = Fila THEN
	IF Integer(istr_mant.argumento[6]) < ai_columna THEN
		istr_mant.argumento[6]	=	String(ai_columna)
	ELSE
		ai_columna = Integer(istr_mant.argumento[6])
	END IF
ELSE
	istr_mant.argumento[6] = '0'
	istr_mant.argumento[3] = String(Fila)
	IF Integer(istr_mant.argumento[6]) < ai_columna THEN
		istr_mant.argumento[6]	=	String(ai_columna)
	ELSE
		ai_columna = Integer(istr_mant.argumento[6])
	END IF
END IF

For ll_fila= 1 To ll_Tope
	If ll_fila >= 10 Then
		ls_columna	=	ls_Prefijo + '_mues' +  String(ll_Fila)
	Else
		ls_columna	=	ls_Prefijo + '_mues' + String(0) + String(ll_Fila)
	End If
	
	If Not IsNull(adw.GetitemNumber(Fila,ls_columna)) And adw.GetitemNumber(Fila,ls_columna) > 0 Or  FIla = 5 Then li_cuenta++
	
	ld_valor   =  adw.GetitemNumber(Fila,ls_columna)

	If IsNull(ld_valor) And li_cuenta < ai_columna AND ll_fila <= ai_columna Then
		Messagebox("Atención","Muestra" +"  " + String(ll_fila)+ " " + "no puede ser Nula",exclamation!)
		adw.SetItem(Fila,ls_Prefijo + "_medias",ld_null)
		//adw.SetItem(Fila, ls_columna, dec(ld_null))
		is_columna = ls_columna
		adw.SetColumn(ls_columna)
//		adw.SetFocus()
		il_valida = 1
		RETURN 
	Else
		il_valida = 0
		If li_cuenta > ai_columna Then
			li_Div = li_cuenta 
		Else
			li_Div = ai_columna 
		End If
	
		If IsNull(ld_valor) Then ld_valor = 0 
		
		ld_suma  = ld_suma + ld_valor 
		ld_media = ld_suma / li_Div
		adw.SetItem(Fila,ls_prefijo + "_medias",ld_media)
	End If
Next
end subroutine

public subroutine wf_calcula_minmax (long al_fila, string as_columna, integer tipo, datawindow adw, boolean minimo);Decimal{2}	ld_valor, ld_Minimo
String			ls_columna, ls_Prefijo
Long			ll_fila, ll_Tope
Integer  		ai_Columna

adw.AcceptText()
SetNull(ld_Minimo)

ai_columna	=	Integer(as_columna)

If iuo_Especies.Codigo = 41 Then
	If Tipo = 4 Then
		ll_Tope = 60
	Else
		ll_Tope = 30
	End If	
Else
	ll_Tope = 10
End If

If Tipo = 1 Then
	ls_Prefijo = 'anms'
Elseif Tipo = 2 Then
	ls_Prefijo = 'anss'
ElseIf Tipo = 3 Then
	ls_Prefijo = 'anpf'
ElseIf Tipo = 4 Then
	ls_Prefijo = 'anfi'
ElseIf Tipo = 5 Then
	ls_Prefijo = 'anma'
End If	

For ll_Fila= 1 to ai_columna
	If ll_fila >= 10 Then
		ls_columna	=	ls_Prefijo + "_mues"  + String(ll_Fila)
	Else
		ls_columna	=	ls_Prefijo + "_mues" + String(0) + String(ll_Fila)
	End If
		
	ld_valor   =  adw.GetitemNumber(al_fila,ls_columna)
	
	If IsNull(ld_Minimo) Then 
		ld_Minimo = ld_Valor
	Else
		If Minimo Then
			If ld_Valor <=  ld_Minimo Then ld_Minimo = ld_Valor 
		Else
			If ld_Valor >=  ld_Minimo Then ld_Minimo = ld_Valor 
		End If
	End If

Next

If Minimo Then
	adw.SetItem(al_Fila, ls_Prefijo + '_nromin', ld_Minimo)
Else
	adw.SetItem(al_Fila, ls_Prefijo + '_nromax', ld_Minimo)
End If
end subroutine

public function boolean wf_filtra (integer ai_especie, datawindowchild adwc);Boolean	lb_Retorno = True

adwc.SetFilter('')
If adwc.Filter() = -1 Then 
	lb_Retorno = False
Else
	adwc.SetFilter('espe_codigo = ' + String(ai_Especie))
	If adwc.Filter() = -1 Then lb_Retorno = False
End If

Return lb_Retorno
end function

public function any wf_valida (datawindow adw, integer fila, string columna, string valor);Boolean	lb_Retorno = True
Dec{2}	ld_Valor
String		ls_Valor

If ii_Especie = 81 Then
	If adw.Object.mase_valida[Fila] = 1 Then
		ld_Valor	= adw.GetItemDecimal(Fila, Columna)
		ls_Valor	= String(adw.Object.mase_valore[Fila])
		If adw.Object.mase_valore[Fila] < Dec(Valor) Then
			IF MessageBox("Atención", "¿Esta usted seguro de que el valor de " + String(adw.Object.mase_nombre[Fila]) + &
			+ "  sobre el " + ls_Valor  + "% esta correcto?", Question!, YesNo!) <> 1 THEN
				lb_Retorno = False
			END IF
		End If
	End If
Else
	If adw.Object.madu_valida[Fila] = 1 Then
		If adw.Object.madu_operad[Fila] = 4 Then
			If Not((Dec(Valor) >= adw.Object.madu_inicio[Fila]) And (Dec(Valor) <= adw.Object.madu_termin[Fila])) Then
				MessageBox('Atención...', 'Valor ingresado fuera de rango de validacion.')
				lb_Retorno = False
			End If
		Else
			Choose Case adw.Object.madu_operad[Fila]
				Case 1
					If adw.Object.madu_inicio[Fila] <> Dec(Valor) Then
						MessageBox('Atención...', 'Valor ingresado fuera de rango de validacion.')
						lb_Retorno = False
					End If
					
				Case 2
					If Dec(Valor) < adw.Object.madu_inicio[Fila] Then
						MessageBox('Atención...', 'Valor ingresado fuera de rango de validacion.')
						lb_Retorno = False
					End If
					
				Case 3
					If Dec(Valor) > adw.Object.madu_inicio[Fila] Then
						MessageBox('Atención...', 'Valor ingresado fuera de rango de validacion.')
						lb_Retorno = False
					End If
			End Choose
		End If	
	End If
End If

Return lb_Retorno
end function

on w_maed_ctlcalplanillamadurez.create
int iCurrent
call super::create
this.tab_1=create tab_1
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.tab_1
end on

on w_maed_ctlcalplanillamadurez.destroy
call super::destroy
destroy(this.tab_1)
end on

event ue_seleccion;call super::ue_seleccion;istr_busq.argum[1]	=	String(iuo_Plantas.Codigo)
istr_busq.argum[2]	=	String(iuo_Especies.Codigo)
istr_busq.argum[4]	=	String(gi_CodExport)

OpenWithParm(w_busc_ctlcalplanillamadurez, istr_busq)

istr_busq = Message.PowerObjectParm

If UpperBound(istr_busq.argum) > 4 Then
	If istr_busq.argum[5] <> "" Then
		iuo_Plantas.Codigo		= Integer(istr_Busq.Argum[1])
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

Do	
	ll_fila_e	= dw_2.Retrieve(gi_codexport, iuo_Plantas.Codigo, iuo_Especies.Codigo, Long(istr_mant.Argumento[1]))

	If ll_fila_e = -1 Then
		respuesta = MessageBox("Error en Base de Datos", "No es posible conectar la Base de Datos.", Information!, RetryCancel!)
	Else
		If ll_fila_e > 0 Then
			Do
				If iuo_especies.Codigo = 81 Then
					wf_recupera(dw_3, 'dw_mues_ctlcalmateriaseca', 1, gi_Codexport, iuo_Plantas.codigo, iuo_Especies.Codigo, Long(istr_mant.Argumento[1]))
				ElseIf iuo_especies.Codigo = 41 Then
					wf_recupera(dw_3, 'dw_mues_ctlcalmateriaseca', 1, gi_Codexport, iuo_Plantas.codigo, iuo_Especies.Codigo, Long(istr_mant.Argumento[1]))
					wf_recupera(dw_4, 'dw_mues_ctlcalcolorsemilla', 2, gi_Codexport, iuo_Plantas.codigo, iuo_Especies.Codigo, Long(istr_mant.Argumento[1]))
					wf_recupera(dw_5, 'dw_mues_ctlcalsolidossolubles', 3, gi_Codexport, iuo_Plantas.codigo, iuo_Especies.Codigo, Long(istr_mant.Argumento[1]))
					wf_recupera(dw_6, 'dw_mues_ctlcalbrotytis', 4, gi_Codexport, iuo_Plantas.codigo, iuo_Especies.Codigo, Long(istr_mant.Argumento[1]))
					wf_recupera(dw_7, 'dw_mues_categoriaguarda01', 5, gi_Codexport, iuo_Plantas.codigo, iuo_Especies.Codigo, Long(istr_mant.Argumento[1]))
					wf_recupera(dw_9, 'dw_mues_ctlcalpesofruto', 6, gi_Codexport, iuo_Plantas.codigo, iuo_Especies.Codigo, Long(istr_mant.Argumento[1]))
					wf_recupera(dw_10, 'dw_mues_ctlcalfirmeza', 7, gi_Codexport, iuo_Plantas.codigo, iuo_Especies.Codigo, Long(istr_mant.Argumento[1]))
					wf_Calcula_Color()
				Else
					wf_recupera(dw_8, 'dw_mues_ctlcalmadurez', 8, gi_Codexport, iuo_Plantas.codigo, iuo_Especies.Codigo, Long(istr_mant.Argumento[1]))
				End If
				
				dw_2.GetChild("prod_codigo", idwc_productores)
				idwc_productores.SetTransObject(Sqlca)
				If idwc_productores.Retrieve(dw_2.Object.zona_codigo[1]) = 0 Then idwc_productores.InsertRow(0)
				
				dw_2.GetChild("vari_codigo", idwc_variedades)
				idwc_variedades.SetTransObject(Sqlca)
				If idwc_variedades.Retrieve(dw_2.Object.espe_codigo[1]) = 0 Then idwc_variedades.InsertRow(0)
				
				dw_2.GetChild("prpr_codigo", idwc_predios)
				idwc_predios.SetTransObject(Sqlca)
				If idwc_predios.Retrieve(dw_2.Object.prod_codigo[1]) = 0 Then idwc_predios.InsertRow(0)
				
				dw_2.GetChild("prcc_codigo", idwc_cuarteles)
				idwc_cuarteles.SetTransObject(Sqlca)
				If idwc_cuarteles.Retrieve(dw_2.Object.prod_codigo[1], dw_2.Object.prpr_codigo[1]) = 0 Then idwc_cuarteles.InsertRow(0)
				
				iuo_Zonas.Codigo	= dw_2.Object.zona_codigo[1]
				iuo_Productor.Codigo			= dw_2.Object.prod_codigo[1]
				iuo_ProdPredio.Codigo		= dw_2.Object.prpr_codigo[1]
				iuo_ProdCuarteles.Cuartel	= dw_2.Object.prcc_codigo[1]
				iuo_Variedades.Variedad		= dw_2.Object.vari_codigo[1]
				iuo_Especies.Codigo			=	dw_2.Object.espe_codigo[1]
				wf_Filtra(iuo_Especies.Codigo, idwc_Cuarteles)
			Loop While Respuesta = 1

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
Long		ll_modif2, ll_modif3, ll_modif4, ll_modif5, ll_modif6, ll_modif7, ll_modif10, & 
         	ll_fildw3, ll_fildw4, ll_fildw5, ll_fildw6, ll_fildw7, ll_fildw8, ll_modif8, ll_fildw9, ll_modif9, ll_fildw10

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
			ll_modif7		=	dw_7.GetNextModified(0, Primary!)
			ll_modif8		=	dw_8.GetNextModified(0, Primary!)
			ll_modif9		=	dw_9.GetNextModified(0, Primary!)
			ll_modif10		=	dw_10.GetNextModified(0, Primary!)
						
			IF dw_3.RowCount() > 0 OR dw_4.RowCount() > 0 OR dw_5.RowCount() > 0 OR &
			   dw_6.RowCount() > 0 OR dw_7.RowCount() > 0  OR dw_8.RowCount() > 0 OR &
			   dw_9.RowCount() > 0  OR dw_10.RowCount() > 0 THEN
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
dw_7.Reset()
dw_8.Reset()
dw_9.Reset()
dw_10.Reset()

pb_grabar.Enabled		=	False
pb_eliminar.Enabled		=	False
pb_imprimir.Enabled		=	False
dw_2.Enabled				=	True

dw_2.SetRedraw(False)
dw_2.Reset()
dw_2.InsertRow(0)
dw_2.SetRedraw(True)

If iuo_especies.Codigo = 81 Then
	wf_recupera(dw_3, 'dw_mues_ctlcalmateriaseca', 1, gi_Codexport, iuo_Plantas.codigo, iuo_Especies.Codigo, 0)
	dw_2.Object.vari_codigo[1]	=	1
ElseIf iuo_especies.Codigo = 41 Then
	wf_recupera(dw_3, 'dw_mues_ctlcalmateriaseca', 1, gi_Codexport, iuo_Plantas.codigo, iuo_Especies.Codigo, 0)
	wf_recupera(dw_4, 'dw_mues_ctlcalcolorsemilla', 2, gi_Codexport, iuo_Plantas.codigo, iuo_Especies.Codigo, 0)
	wf_recupera(dw_5, 'dw_mues_ctlcalsolidossolubles', 3, gi_Codexport, iuo_Plantas.codigo, iuo_Especies.Codigo, 0)
	wf_recupera(dw_6, 'dw_mues_ctlcalbrotytis', 4, gi_Codexport, iuo_Plantas.codigo, iuo_Especies.Codigo, 0)
	wf_recupera(dw_7, 'dw_mues_categoriaguarda01', 5, gi_Codexport, iuo_Plantas.codigo, iuo_Especies.Codigo, 0)
	wf_recupera(dw_9, 'dw_mues_ctlcalpesofruto', 6, gi_Codexport, iuo_Plantas.codigo, iuo_Especies.Codigo, 0)
	wf_recupera(dw_10, 'dw_mues_ctlcalfirmeza', 7, gi_Codexport, iuo_Plantas.codigo, iuo_Especies.Codigo, 0)
ElseIf iuo_Especies.Codigo = 26 Or iuo_Especies.Codigo = 27 Or iuo_Especies.Codigo = 78 Then
	wf_recupera(dw_8, 'dw_mues_ctlcalmadurez', 8, gi_Codexport, iuo_Plantas.codigo, iuo_Especies.Codigo, 0)
End If

HabilitaEncab(True)

dw_2.Object.clie_codigo[1]		=	gi_CodExport
dw_2.Object.espe_codigo[1]	=	iuo_Especies.Codigo
dw_2.Object.zona_codigo[1]	=	iuo_Zonas.Codigo
dw_2.Object.plde_codigo[1]		=	iuo_Plantas.Codigo
dw_2.Object.pmen_fecham[1]	=	Today()
dw_2.Object.pmen_fechaa[1]	=	Today()
dw_2.Object.pmen_horaan[1]	=	Now()

wf_Filtra(iuo_Especies.Codigo, idwc_Cuarteles)

li_Grupo = BuscaGrupo(Upper(Gstr_Us.Nombre))

If (li_Grupo > 2) Then
	pb_eliminar.Visible	=	False
	TriggerEvent('resize')
End If

If iuo_Especies.Codigo = 26 Or iuo_Especies.Codigo = 27 Or iuo_Especies.Codigo = 78 Then
	Tab_1.SelectTab(8)
ElseIf iuo_Especies.Codigo = 81 Then
	Tab_1.SelectTab(5)
End If

istr_mant.argumento[1] = '0'
istr_mant.argumento[2] = '0'
istr_mant.argumento[3] = '0'
istr_mant.argumento[4] = '0'
istr_mant.argumento[5] = '0'
istr_mant.argumento[6] = '0'

dw_2.SetFocus()
dw_2.SetColumn("pmen_numero")
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

dw_3		=	tab_1.tabpage_1.dw_materiaseca
dw_4		=	tab_1.tabpage_2.dw_colorsemilla
dw_5		=	tab_1.tabpage_3.dw_solidossolubles
dw_6  	=  tab_1.tabpage_4.dw_brotrytis
dw_7  	=  tab_1.tabpage_5.dw_categoriafruta
dw_8  	=  tab_1.tabpage_6.dw_madurez
dw_9 	=  tab_1.tabpage_7.dw_peso
dw_10  	=  tab_1.tabpage_8.dw_firmeza

dw_2.SetTransObject(sqlca)
dw_3.SetTransObject(sqlca)
dw_4.SetTransObject(sqlca)
dw_5.SetTransObject(sqlca)
dw_6.SetTransObject(sqlca)
dw_7.SetTransObject(sqlca)
dw_8.SetTransObject(sqlca)
dw_9.SetTransObject(sqlca)
dw_10.SetTransObject(sqlca)

ii_especie = Integer(Message.StringParm)

iuo_zonas       		= Create  uo_zonas
iuo_plantas     		= Create  uo_plantadesp
iuo_variedades  	= Create  uo_variedades 
iuo_productor   	= Create  uo_productores
iuo_inspectores  	= Create  uo_ctlcalinspectores
iuo_especies   		= Create  uo_especie
iuo_prodcuarteles	= Create  uo_prodcuarteles
iuo_prodpredio    	= Create  uo_prodpredio

If Not iuo_Especies.Existe(ii_especie, True, Sqlca) Then Return
This.Title	= "Planilla Parametros Madurez: " + iuo_especies.Nombre
If Not iuo_Zonas.Existe(gi_codZona, True, Sqlca) Then Return
If Not iuo_Plantas.Existe(gi_codPlanta, True, Sqlca) Then Return

If Not wf_Visibilidad(iuo_Especies.Codigo) Then Return

wf_child()

istr_mant.UsuarioSoloConsulta	=	OpcionSoloConsulta()

GrabaAccesoAplicacion(True, id_FechaAcceso, it_HoraAcceso, This.Title, "Acceso a Aplicación", 1)
end event

event ue_imprimir;SetPointer(HourGlass!)

Long		fila

DataWindowChild  dwc_packing

istr_info.titulo	=	"INFORME DE ANALISIS PRECHOSECHA"
istr_info.copias	=	1

OpenWithParm(vinf,istr_info)

If iuo_Especies.Codigo = 41 Then
	vinf.dw_1.DataObject	=	"dw_info_planillamadurez_kiwi" 
ElseIf iuo_Especies.Codigo = 27 Or iuo_Especies.Codigo = 26 Or iuo_Especies.Codigo = 78 Then
	vinf.dw_1.DataObject	=	"dw_info_planillamadurez_citricos"
ElseIf iuo_Especies.Codigo = 81 Then
	vinf.dw_1.DataObject	=	"dw_info_planillamadurez_palta"
End If

vinf.dw_1.SetTransObject(sqlca)

fila	=	vinf.dw_1.Retrieve(gi_codexport, dw_2.Object.plde_codigo[1], iuo_Especies.Codigo, dw_2.Object.pmen_numero[1])

If fila	=	-1 Then
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ElseIf fila	=	0 Then
	MessageBox( "No Existe información", "No existe información para este informe.", &
					StopSign!, Ok!)
Else
	F_Membrete(vinf.dw_1)
	If iuo_Especies.Codigo = 41 Then vinf.dw_1.Object.DataWindow.Zoom = 83
	If gs_Ambiente <> 'Windows' Then F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)

End If

SetPointer(Arrow!)
end event

event resize;Integer	maximo, li_posic_x, li_posic_y, li_visible = 0

IF dw_2.width > il_AnchoDw_1 THEN
	maximo		=	dw_2.width
ELSE
	Tab_1.width	=	This.WorkSpaceWidth() - 400
	maximo		=	Tab_1.width
END IF

dw_2.x					= 37 + Round((This.WorkSpaceWidth() - dw_2.width - 400) / 2, 0)
dw_2.y					= 37

Tab_1.width				= This.WorkSpaceWidth() - 400

Tab_1.x					= 37 + Round((This.WorkSpaceWidth() - 400 - Tab_1.width ) / 2, 0)
Tab_1.y					= 64 + dw_2.Height

Tab_1.Height			= This.WorkSpaceHeight() - Tab_1.y - 41

Tab_1.TabPage_1.dw_materiaseca.Height		=	Tab_1.Height -	250
Tab_1.TabPage_2.dw_colorsemilla.Height		=	Tab_1.Height -	250
Tab_1.TabPage_3.dw_solidossolubles.Height	=	Tab_1.Height -	250
Tab_1.TabPage_4.dw_brotrytis.Height			=	Tab_1.Height -	250
Tab_1.TabPage_5.dw_categoriafruta.Height	=	Tab_1.Height -	250
Tab_1.TabPage_6.dw_madurez.Height			=	Tab_1.Height -	250
Tab_1.TabPage_7.dw_peso.Height				=	Tab_1.Height -	250
Tab_1.TabPage_8.dw_firmeza.Height				=	Tab_1.Height -	250

Tab_1.TabPage_1.dw_materiaseca.Width		=	Tab_1.Width -	200
Tab_1.TabPage_2.dw_colorsemilla.Width		=	Tab_1.Width -	200
Tab_1.TabPage_3.dw_solidossolubles.Width	=	Tab_1.Width -	200
Tab_1.TabPage_4.dw_brotrytis.Width			=	Tab_1.Width -	200
Tab_1.TabPage_5.dw_categoriafruta.Width		=	Tab_1.Width -	200
Tab_1.TabPage_6.dw_madurez.Width			=	Tab_1.Width -	200
Tab_1.TabPage_7.dw_peso.Width					=	Tab_1.Width -	200
Tab_1.TabPage_8.dw_firmeza.Width				=	Tab_1.Width -	200
//
//gb_1.x 					= This.WorkSpaceWidth() - 310
//gb_1.y 					= 5
//gb_1.width				= 275

li_posic_x				= This.WorkSpaceWidth() - 250
//li_posic_y				= gb_1.y + 88

IF pb_buscar.Visible THEN
	pb_buscar.x				= li_posic_x
	pb_buscar.y				= li_posic_y + 10
	pb_buscar.width		= 235
	pb_buscar.height		= 195
	li_visible ++
	li_posic_y += 180
END IF

IF pb_nuevo.Visible THEN
	pb_nuevo.x				= li_posic_x
	pb_nuevo.y				= li_posic_y + 20
	pb_nuevo.width		= 235
	pb_nuevo.height		= 195
	li_visible ++
	li_posic_y += 180
END IF

IF	pb_eliminar.Visible THEN
	pb_eliminar.x			= li_posic_x
	pb_eliminar.y			= li_posic_y + 30
	pb_eliminar.width		= 235
	pb_eliminar.height		= 195
	li_visible ++
	li_posic_y += 180
END IF

IF pb_grabar.Visible THEN
	pb_grabar.x				= li_posic_x
	pb_grabar.y				= li_posic_y + 40
	pb_grabar.width		= 235
	pb_grabar.height		= 195
	li_visible ++
	li_posic_y += 180
END IF

IF pb_imprimir.Visible THEN
	pb_imprimir.x			= li_posic_x
	pb_imprimir.y			= li_posic_y + 50
	pb_imprimir.width		= 235
	pb_imprimir.height	= 195
	li_visible ++
	li_posic_y += 180
END IF

IF pb_salir.Visible THEN
	pb_salir.x				= li_posic_x
	pb_salir.y				= li_posic_y + 60
	pb_salir.width			= 235
	pb_salir.height			= 195
	li_visible ++
	li_posic_y += 180
END IF

//gb_1.height				= 180 * li_visible + 97 /*  (Según Botones Visibles)  */
//gb_2.x 					= gb_1.x
//gb_2.y 					= 1293
//gb_2.width				= 275
//gb_2.height				= 180 * 2 + 97 /*  (2 Botones)  */

pb_ins_det.x			= li_posic_x
//pb_ins_det.y			= gb_2.y + 93
pb_ins_det.width		= 235
pb_ins_det.height		= 195

pb_eli_det.x				= li_posic_x
pb_eli_det.y				= pb_ins_det.y + 180
pb_eli_det.width		= 235
pb_eli_det.height		= 195
end event

event ue_borrar;IF dw_2.RowCount() < 1 THEN RETURN

SetPointer(HourGlass!)

ib_borrar = True
w_main.SetMicroHelp("Validando la eliminación...")

Message.DoubleParm = 0

This.TriggerEvent ("ue_validaborrar")

IF Message.DoubleParm = -1 THEN RETURN

IF dw_3.RowCount()  > 0	THEN dw_3.RowsMove(1,dw_3.RowCount(),Primary!,dw_3,1,Delete!)
IF dw_4.RowCount()  > 0	THEN dw_4.RowsMove(1,dw_4.RowCount(),Primary!,dw_4,1,Delete!)
IF dw_5.RowCount()  > 0	THEN dw_5.RowsMove(1,dw_5.RowCount(),Primary!,dw_5,1,Delete!)
IF dw_6.RowCount()  > 0	THEN dw_6.RowsMove(1,dw_6.RowCount(),Primary!,dw_6,1,Delete!)
IF dw_7.RowCount()  > 0	THEN dw_7.RowsMove(1,dw_7.RowCount(),Primary!,dw_7,1,Delete!)
IF dw_8.RowCount()  > 0	THEN dw_8.RowsMove(1,dw_8.RowCount(),Primary!,dw_8,1,Delete!)
IF dw_9.RowCount()  > 0	THEN dw_9.RowsMove(1,dw_9.RowCount(),Primary!,dw_9,1,Delete!)
IF dw_10.RowCount() > 0	THEN dw_10.RowsMove(1,dw_10.RowCount(),Primary!,dw_10,1,Delete!)

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

event ue_antesguardar;call super::ue_antesguardar;Long		ll_Fila, ll_col
Integer	li_cont, li_tope, li_null
String		ls_colu[], ls_mensaje, ls_coluant, ls_columna

SetNull(li_null)

IF iuo_Especies.Codigo = 26 OR iuo_Especies.Codigo = 27 THEN
	FOR ll_fila = 1 TO dw_8.RowCount()
		FOR ll_col = 2 TO 15
			ls_columna = 'anma_mues'+String(ll_col,'00')
			IF NOT isnull(dw_8.GetitemNumber(ll_fila,ls_columna)) THEN
				ls_coluant = 'anma_mues'+String(ll_col - 1,'00')
				IF isnull(dw_8.GetitemNumber(ll_fila,ls_coluant)) THEN
					MessageBox("Error de Consistencia", "Falta el ingreso de Columna:" + String(ll_col - 1) + ".", STopSign!, Ok!)
					dw_8.SetRow(ll_fila)
					dw_8.SetColumn(ls_coluant)
					dw_8.SetFocus()
					Message.DoubleParm = -1
					RETURN		
				END IF	
			END IF
			
		NEXT 
	NEXT	
END IF

If dw_2.RowCount() > 0 Then
	If Isnull(dw_2.Object.pmen_numero[1]) OR dw_2.Object.pmen_numero[1] = 0 Then
		li_cont ++
		ls_mensaje 			= ls_mensaje + "~nNº Planilla"
		ls_colu[li_cont]	= "ccre_numero"
	End If
	
	If Isnull(dw_2.Object.zona_codigo[1]) OR dw_2.Object.zona_codigo[1] = 0 Then
		li_cont ++
		ls_mensaje 			= ls_mensaje + "~nZona"
		ls_colu[li_cont]	= "zona_codigo"
	End If	
	
	If Isnull(dw_2.Object.plde_codigo[1]) OR dw_2.Object.plde_codigo[1] = 0 Then
		li_cont ++
		ls_mensaje 			= ls_mensaje + "~nPlanta"
		ls_colu[li_cont]	= "plde_codigo"
	End If	
	
	If Isnull(dw_2.Object.vari_codigo[1]) OR dw_2.Object.vari_codigo[1] = 0 Then
		li_cont ++
		ls_mensaje 			= ls_mensaje + "~nVariedad"
		ls_colu[li_cont]	= "vari_codigo"
	End If
	
	If Isnull(dw_2.Object.prod_codigo[1]) OR dw_2.Object.prod_codigo[1] = 0 Then
		li_cont ++
		ls_mensaje 			= ls_mensaje + "~nProductor"
		ls_colu[li_cont]	= "prod_codigo"
	End If
	
	If iuo_Especies.Codigo = 81 Then
		If Isnull(dw_2.Object.prpr_codigo[1]) OR dw_2.Object.prpr_codigo[1] = 0 Then
			li_cont ++
			ls_mensaje 			= ls_mensaje + "~nPredio"
			ls_colu[li_cont]	= "prpr_codigo"
		End If
		
		If Isnull(dw_2.Object.prcc_codigo[1]) OR dw_2.Object.prcc_codigo[1] = 0 Then
			li_cont ++
			ls_mensaje 			= ls_mensaje + "~nCuartel"
			ls_colu[li_cont]	= "prcc_codigo"
		End If
	End If
End If

If li_cont > 0 Then
		MessageBox("Error de Consistencia", "Falta el ingreso de :" + &
					ls_mensaje + ".", STopSign!, Ok!)
		dw_2.SetColumn(ls_colu[1])
		dw_2.SetFocus()
		HabilitaEncab(TRUE)
		Message.DoubleParm = -1
		RETURN		
Else		
	For ll_Fila = 1 to dw_3.RowCount()
		dw_3.Object.clie_codigo[ll_Fila]		=	gi_CodExport
		dw_3.Object.plde_codigo[ll_Fila]		=	iuo_Plantas.Codigo
		dw_3.Object.espe_codigo[ll_Fila]		=	iuo_Especies.Codigo
		dw_3.Object.pmen_numero[ll_Fila]	=	dw_2.Object.pmen_numero[1]
	Next
	
	For ll_Fila = 1 to dw_4.RowCount()
		dw_4.Object.clie_codigo[ll_Fila]		=	gi_CodExport
		dw_4.Object.plde_codigo[ll_Fila]		=	iuo_Plantas.Codigo
		dw_4.Object.espe_codigo[ll_Fila]		=	iuo_Especies.Codigo
		dw_4.Object.pmen_numero[ll_Fila]	=	dw_2.Object.pmen_numero[1]
	Next
	
	For ll_Fila = 1 to dw_5.RowCount()
		dw_5.Object.clie_codigo[ll_Fila]		=	gi_CodExport
		dw_5.Object.plde_codigo[ll_Fila]		=	iuo_Plantas.Codigo
		dw_5.Object.espe_codigo[ll_Fila]		=	iuo_Especies.Codigo
		dw_5.Object.pmen_numero[ll_Fila]	=	dw_2.Object.pmen_numero[1]
	Next
	
	For ll_Fila = 1 to dw_6.RowCount()
		dw_6.Object.clie_codigo[ll_Fila]		=	gi_CodExport
		dw_6.Object.plde_codigo[ll_Fila]		=	iuo_Plantas.Codigo
		dw_6.Object.espe_codigo[ll_Fila]		=	iuo_Especies.Codigo
		dw_6.Object.pmen_numero[ll_Fila]	=	dw_2.Object.pmen_numero[1]
	Next
	
	For ll_Fila = 1 to dw_7.RowCount()
		dw_7.Object.clie_codigo[ll_Fila]		=	gi_CodExport
		dw_7.Object.plde_codigo[ll_Fila]		=	iuo_Plantas.Codigo
		dw_7.Object.espe_codigo[ll_Fila]		=	iuo_Especies.Codigo
		dw_7.Object.pmen_numero[ll_Fila]	=	dw_2.Object.pmen_numero[1]
	Next
	
	For ll_Fila = 1 to dw_8.RowCount()
		dw_8.Object.clie_codigo[ll_Fila]		=	gi_CodExport
		dw_8.Object.plde_codigo[ll_Fila]		=	iuo_PLantas.Codigo
		dw_8.Object.espe_codigo[ll_Fila]		=	iuo_Especies.Codigo
		dw_8.Object.pmen_numero[ll_Fila]	=	dw_2.Object.pmen_numero[1]
	Next
	
	For ll_Fila = 1 to dw_9.RowCount()
		dw_9.Object.clie_codigo[ll_Fila]		=	gi_CodExport
		dw_9.Object.plde_codigo[ll_Fila]		=	iuo_PLantas.Codigo
		dw_9.Object.espe_codigo[ll_Fila]		=	iuo_Especies.Codigo
		dw_9.Object.pmen_numero[ll_Fila]	=	dw_2.Object.pmen_numero[1]
	Next
	
	For ll_Fila = 1 to dw_10.RowCount()
		dw_10.Object.clie_codigo[ll_Fila]		=	gi_CodExport
		dw_10.Object.plde_codigo[ll_Fila]		=	iuo_PLantas.Codigo
		dw_10.Object.espe_codigo[ll_Fila]		=	iuo_Especies.Codigo
		dw_10.Object.pmen_numero[ll_Fila]	=	dw_2.Object.pmen_numero[1]
	Next
End If
end event

type dw_1 from w_mant_encab_deta_csd`dw_1 within w_maed_ctlcalplanillamadurez
boolean visible = false
integer x = 1358
integer y = 1784
integer width = 914
integer height = 612
boolean titlebar = false
string title = ""
boolean hscrollbar = false
boolean border = false
boolean hsplitscroll = true
end type

type dw_2 from w_mant_encab_deta_csd`dw_2 within w_maed_ctlcalplanillamadurez
integer x = 37
integer y = 32
integer width = 2546
integer height = 964
integer taborder = 10
string dataobject = "dw_mant_ctlcalparammadurezenca"
boolean controlmenu = true
end type

event dw_2::itemchanged;String	ls_Columna, ls_Nula

SetNull(ls_Nula)
ls_Columna	=	dwo.Name

Choose Case ls_Columna
	Case "pmen_numero"
		If wf_Planilla(Long(data), iuo_Plantas.Codigo) Then
			istr_mant.Argumento[1]	=	Data
			If ii_sw = 0 Then
				Parent.TriggerEvent("ue_recuperadatos")
			Else
				Messagebox("Atención","Planilla Digitada existe para otra Especie, no puede ingresar", Exclamation!, Ok!)
				dw_2.Object.pmen_numero.Protect	=	0
				dw_2.SetItem(1,"pmen_numero",Integer(ls_Nula))
				Return 1
			End If
			dw_2.SetColumn("pmen_numero")
			dw_2.SetFocus()
		End If
		
	Case "zona_codigo"
		If Not iuo_Zonas.Existe(Integer(data), True, Sqlca) Then
			This.SetItem(1, ls_Columna, Long(ls_nula))
			Return 1
		Else
			This.GetChild('prod_codigo', idwc_Productores)
			idwc_Productores.SetTransObject(sqlca)
			If idwc_Productores.Retrieve(iuo_Zonas.Codigo) = 0 Then idwc_Productores.InsertRow(0)
		End If
	
	Case "plde_codigo"
		If Not iuo_Plantas.Existe(Integer(data), True, Sqlca) Then
			This.SetItem(1, ls_Columna, Long(ls_nula))
			Return 1
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
			wf_Filtra(iuo_Especies.Codigo, idwc_Cuarteles)
		End IF	
		  
	Case "prpr_codigo"
		If Not iuo_prodpredio.existepredioprod(iuo_Productor.Codigo, Integer(data), True, sqlca) Then
			This.SetItem(1, ls_Columna, Long(ls_nula))
			Return 1
		Else
			dw_2.GetChild("prcc_codigo", idwc_cuarteles)
			idwc_cuarteles.SetTransObject(Sqlca)
			If idwc_cuarteles.Retrieve(iuo_Productor.Codigo, iuo_prodpredio.Codigo) = 0 Then idwc_cuarteles.InsertRow(0)
			dw_2.Object.prcc_codigo[1] = Integer(ls_Nula)				
		End If
		  
	Case "prcc_codigo"
		If Not iuo_prodcuarteles.existe(iuo_Productor.Codigo, iuo_prodpredio.Codigo, Integer(data), True, sqlca) Then
			This.SetItem(1, ls_Columna, Long(ls_nula))
			Return 1
		Else
			If iuo_Especies.Codigo = iuo_prodcuarteles.Especie Then
				This.Object.vari_codigo[Row] = iuo_prodcuarteles.Variedad
			Else
				MessageBox('Error...', 'Cuartel Seleccionado no es de Especie: ' + iuo_Especies.Nombre, StopSign!, Ok!)
				This.SetItem(1, ls_Columna, Long(ls_nula))
				Return 1
			End If
		End If  
	
	Case "ccin_codigo"
		If Not iuo_inspectores.existe(sqlca,Integer(data),True) Then
			This.SetItem(1, ls_Columna, Integer(ls_nula))
			Return 1
		End If
End Choose

habilitaingreso(ls_Columna)
end event

event dw_2::sqlpreview;//
end event

type pb_nuevo from w_mant_encab_deta_csd`pb_nuevo within w_maed_ctlcalplanillamadurez
integer x = 3319
integer y = 300
integer taborder = 40
end type

type pb_eliminar from w_mant_encab_deta_csd`pb_eliminar within w_maed_ctlcalplanillamadurez
integer x = 3319
integer y = 480
integer taborder = 60
end type

type pb_grabar from w_mant_encab_deta_csd`pb_grabar within w_maed_ctlcalplanillamadurez
integer x = 3319
integer y = 664
integer taborder = 50
end type

type pb_imprimir from w_mant_encab_deta_csd`pb_imprimir within w_maed_ctlcalplanillamadurez
integer x = 3319
integer y = 836
integer taborder = 70
end type

type pb_salir from w_mant_encab_deta_csd`pb_salir within w_maed_ctlcalplanillamadurez
integer x = 3319
integer y = 1020
integer taborder = 80
end type

type pb_ins_det from w_mant_encab_deta_csd`pb_ins_det within w_maed_ctlcalplanillamadurez
boolean visible = false
integer x = 3319
integer y = 1312
integer taborder = 0
end type

type pb_eli_det from w_mant_encab_deta_csd`pb_eli_det within w_maed_ctlcalplanillamadurez
boolean visible = false
integer x = 3323
integer y = 1484
integer taborder = 0
end type

type pb_buscar from w_mant_encab_deta_csd`pb_buscar within w_maed_ctlcalplanillamadurez
integer x = 3319
integer y = 120
integer taborder = 30
boolean default = true
end type

type tab_1 from tab within w_maed_ctlcalplanillamadurez
integer x = 32
integer y = 1080
integer width = 3223
integer height = 1136
integer taborder = 20
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 30586022
boolean multiline = true
boolean raggedright = true
boolean focusonbuttondown = true
boolean boldselectedtext = true
alignment alignment = center!
integer selectedtab = 1
tabpage_7 tabpage_7
tabpage_8 tabpage_8
tabpage_3 tabpage_3
tabpage_2 tabpage_2
tabpage_1 tabpage_1
tabpage_4 tabpage_4
tabpage_5 tabpage_5
tabpage_6 tabpage_6
end type

on tab_1.create
this.tabpage_7=create tabpage_7
this.tabpage_8=create tabpage_8
this.tabpage_3=create tabpage_3
this.tabpage_2=create tabpage_2
this.tabpage_1=create tabpage_1
this.tabpage_4=create tabpage_4
this.tabpage_5=create tabpage_5
this.tabpage_6=create tabpage_6
this.Control[]={this.tabpage_7,&
this.tabpage_8,&
this.tabpage_3,&
this.tabpage_2,&
this.tabpage_1,&
this.tabpage_4,&
this.tabpage_5,&
this.tabpage_6}
end on

on tab_1.destroy
destroy(this.tabpage_7)
destroy(this.tabpage_8)
destroy(this.tabpage_3)
destroy(this.tabpage_2)
destroy(this.tabpage_1)
destroy(this.tabpage_4)
destroy(this.tabpage_5)
destroy(this.tabpage_6)
end on

type tabpage_7 from userobject within tab_1
integer x = 18
integer y = 168
integer width = 3186
integer height = 952
long backcolor = 30586022
string text = "Peso~r~nFruto"
long tabtextcolor = 33554432
long tabbackcolor = 12632256
long picturemaskcolor = 536870912
dw_peso dw_peso
end type

on tabpage_7.create
this.dw_peso=create dw_peso
this.Control[]={this.dw_peso}
end on

on tabpage_7.destroy
destroy(this.dw_peso)
end on

type dw_peso from uo_dw within tabpage_7
integer x = 50
integer y = 72
integer width = 2725
integer height = 868
integer taborder = 11
string dataobject = "dw_mues_analisispesofruto"
boolean hscrollbar = true
end type

event sqlpreview;//
end event

event itemchanged;call super::itemchanged;String		ls_Columna, ls_colu
Integer	li_numero

ls_Columna	=	dwo.Name
istr_mant.argumento[3] = ''

Choose Case ls_Columna
	Case "anpf_mues01", "anpf_mues02", "anpf_mues03", "anpf_mues04","anpf_mues05", &
	  	   "anpf_mues06", "anpf_mues07", "anpf_mues08", "anpf_mues09","anpf_mues10", &
		   "anpf_mues11", "anpf_mues12", "anpf_mues13", "anpf_mues14","anpf_mues15", &
	  	   "anpf_mues16", "anpf_mues17", "anpf_mues18", "anpf_mues19","anpf_mues20", &
		   "anpf_mues21", "anpf_mues22", "anpf_mues23", "anpf_mues24","anpf_mues25", &
	  	   "anpf_mues26", "anpf_mues27", "anpf_mues28", "anpf_mues29","anpf_mues30"
		If IsNull(data) Then
			If Integer(Mid(ls_columna,10,2))>= 1 Then
				li_Numero	=	Integer(Mid(ls_columna,10,2)) - 1
				ls_colu 		= 	Fill("0",2 - Len(String(li_Numero))) + String(li_Numero)
				istr_mant.argumento[3] = ls_Colu
			End If
		Else
			ls_colu = Mid(ls_columna,10,2)
			wf_Calcula_Promedio(Row, Mid(ls_columna,10,2), 3, This)
			wf_Desviacion_Estandar(Row, Mid(ls_columna,10,2), 3, This)
			wf_Calcula_MinMax(Row, Mid(ls_columna,10,2), 3, This, True)
			wf_Calcula_MinMax(Row, Mid(ls_columna,10,2), 3, This, False)
		End If
		
End Choose
end event

type tabpage_8 from userobject within tab_1
integer x = 18
integer y = 168
integer width = 3186
integer height = 952
long backcolor = 30586022
string text = "Firmeza"
long tabtextcolor = 33554432
long tabbackcolor = 12632256
long picturemaskcolor = 536870912
dw_firmeza dw_firmeza
end type

on tabpage_8.create
this.dw_firmeza=create dw_firmeza
this.Control[]={this.dw_firmeza}
end on

on tabpage_8.destroy
destroy(this.dw_firmeza)
end on

type dw_firmeza from uo_dw within tabpage_8
integer x = 50
integer y = 72
integer width = 2725
integer height = 868
integer taborder = 11
string dataobject = "dw_mues_analisisfirmeza"
boolean hscrollbar = true
end type

event itemchanged;call super::itemchanged;String		ls_Columna, ls_colu
Integer	li_numero

ls_Columna	=	dwo.Name
istr_mant.argumento[3] = ''

Choose Case ls_Columna
	Case "anfi_mues01", "anfi_mues02", "anfi_mues03", "anfi_mues04","anfi_mues05", &
	  	   "anfi_mues06", "anfi_mues07", "anfi_mues08", "anfi_mues09","anfi_mues10", &
		   "anfi_mues11", "anfi_mues12", "anfi_mues13", "anfi_mues14","anfi_mues15", &
	  	   "anfi_mues16", "anfi_mues17", "anfi_mues18", "anfi_mues19","anfi_mues20", &
		   "anfi_mues21", "anfi_mues22", "anfi_mues23", "anfi_mues24","anfi_mues25", &
	  	   "anfi_mues26", "anfi_mues27", "anfi_mues28", "anfi_mues29","anfi_mues30", &
		   "anfi_mues31", "anfi_mues32", "anfi_mues33", "anfi_mues34","anfi_mues35", &
	  	   "anfi_mues36", "anfi_mues37", "anfi_mues38", "anfi_mues39","anfi_mues40", &
		   "anfi_mues41", "anfi_mues42", "anfi_mues43", "anfi_mues44","anfi_mues45", &
	  	   "anfi_mues46", "anfi_mues47", "anfi_mues48", "anfi_mues49","anfi_mues50", &
		   "anfi_mues51", "anfi_mues52", "anfi_mues53", "anfi_mues54","anfi_mues55", &
	  	   "anfi_mues56", "anfi_mues57", "anfi_mues58", "anfi_mues59","anfi_mues60"
		If IsNull(data) Then
			If Integer(Mid(ls_columna,10,2))>= 1 Then
				li_Numero	=	Integer(Mid(ls_columna,10,2)) - 1
				ls_colu 		= 	Fill("0",2 - Len(String(li_Numero))) + String(li_Numero)
				istr_mant.argumento[3] = ls_Colu
			End If
		Else
			ls_colu = Mid(ls_columna,10,2)
			wf_Calcula_Promedio(Row, Mid(ls_columna,10,2), 4, This)
			wf_Desviacion_Estandar(Row, Mid(ls_columna,10,2), 4, This)
			wf_Calcula_MinMax(Row, Mid(ls_columna,10,2), 4, This, True)
			wf_Calcula_MinMax(Row, Mid(ls_columna,10,2), 4, This, False)
		End If
		
End Choose
end event

event sqlpreview;//
end event

type tabpage_3 from userobject within tab_1
integer x = 18
integer y = 168
integer width = 3186
integer height = 952
long backcolor = 30586022
string text = "º Brix"
long tabtextcolor = 33554432
long tabbackcolor = 12632256
long picturemaskcolor = 536870912
dw_solidossolubles dw_solidossolubles
end type

on tabpage_3.create
this.dw_solidossolubles=create dw_solidossolubles
this.Control[]={this.dw_solidossolubles}
end on

on tabpage_3.destroy
destroy(this.dw_solidossolubles)
end on

type dw_solidossolubles from uo_dw within tabpage_3
integer x = 50
integer y = 72
integer width = 2949
integer height = 840
integer taborder = 11
boolean bringtotop = true
string dataobject = "dw_mues_ctlcalanalisissolidossolubles"
boolean hsplitscroll = true
boolean livescroll = true
end type

event itemchanged;call super::itemchanged;String		ls_Columna, ls_colu
Integer	li_numero

ls_Columna	=	dwo.Name
istr_mant.argumento[3] = ''

Choose Case ls_Columna
	Case "anss_mues01", "anss_mues02", "anss_mues03", "anss_mues04","anss_mues05", &
	  	   "anss_mues06", "anss_mues07", "anss_mues08", "anss_mues09","anss_mues10", &
		   "anss_mues11", "anss_mues12", "anss_mues13", "anss_mues14","anss_mues15", &
	  	   "anss_mues16", "anss_mues17", "anss_mues18", "anss_mues19","anss_mues20", &
		   "anss_mues21", "anss_mues22", "anss_mues23", "anss_mues24","anss_mues25", &
	  	   "anss_mues26", "anss_mues27", "anss_mues28", "anss_mues29","anss_mues30"
		If IsNull(data) Then
			If Integer(Mid(ls_columna,10,2))>= 1 Then
				li_Numero	=	Integer(Mid(ls_columna,10,2)) - 1
				ls_colu 		= 	Fill("0",2 - Len(String(li_Numero))) + String(li_Numero)
				istr_mant.argumento[3] = ls_Colu
			End If
		Else
			ls_colu = Mid(ls_columna,10,2)
			wf_Calcula_Promedio(Row, Mid(ls_columna,10,2), 2, This)
			wf_Desviacion_Estandar(Row, Mid(ls_columna,10,2), 2, This)
			wf_Calcula_MinMax(Row, Mid(ls_columna,10,2), 2, This, True)
			wf_Calcula_MinMax(Row, Mid(ls_columna,10,2), 2, This, False)
		End If
		
End Choose
end event

event sqlpreview;//
end event

type tabpage_2 from userobject within tab_1
integer x = 18
integer y = 168
integer width = 3186
integer height = 952
long backcolor = 30586022
string text = "Color~r~nSemilla"
long tabtextcolor = 33554432
long tabbackcolor = 12632256
long picturemaskcolor = 536870912
dw_colorsemilla dw_colorsemilla
end type

on tabpage_2.create
this.dw_colorsemilla=create dw_colorsemilla
this.Control[]={this.dw_colorsemilla}
end on

on tabpage_2.destroy
destroy(this.dw_colorsemilla)
end on

type dw_colorsemilla from uo_dw within tabpage_2
event ue_calculo ( )
integer x = 50
integer y = 72
integer width = 2949
integer height = 864
integer taborder = 11
string dataobject = "dw_mues_ctlcalanalisiscolorsemilla"
boolean hscrollbar = true
boolean hsplitscroll = true
boolean livescroll = true
end type

event ue_calculo();wf_calcula_Color()
end event

event sqlpreview;//
end event

event itemchanged;call super::itemchanged;String	ls_Columna
Long	ll_Fila

ls_Columna	=	dwo.Name

Choose Case ls_Columna
	Case ''
		
End Choose

PostEvent("ue_calculo")
end event

type tabpage_1 from userobject within tab_1
integer x = 18
integer y = 168
integer width = 3186
integer height = 952
long backcolor = 30586022
string text = "Materia Seca"
long tabtextcolor = 33554432
long tabbackcolor = 12632256
long picturemaskcolor = 536870912
dw_materiaseca dw_materiaseca
end type

on tabpage_1.create
this.dw_materiaseca=create dw_materiaseca
this.Control[]={this.dw_materiaseca}
end on

on tabpage_1.destroy
destroy(this.dw_materiaseca)
end on

type dw_materiaseca from uo_dw within tabpage_1
integer x = 50
integer y = 72
integer width = 2949
integer height = 860
integer taborder = 11
string dataobject = "dw_mues_ctlcalanalisismateriaseca"
boolean hscrollbar = true
boolean hsplitscroll = true
boolean livescroll = true
end type

event sqlpreview;//
end event

event itemchanged;call super::itemchanged;String		ls_Columna, ls_colu, ls_nula
Integer	li_numero

ls_Columna	=	dwo.Name
istr_mant.argumento[3] = ''
SetNull(ls_Nula)

Choose Case ls_Columna
	Case "anms_mues01", "anms_mues02", "anms_mues03", "anms_mues04","anms_mues05", &
	  	   "anms_mues06", "anms_mues07", "anms_mues08", "anms_mues09","anms_mues10", &
		   "anms_mues11", "anms_mues12", "anms_mues13", "anms_mues14","anms_mues15", &
	  	   "anms_mues16", "anms_mues17", "anms_mues18", "anms_mues19","anms_mues20", &
		   "anms_mues21", "anms_mues22", "anms_mues23", "anms_mues24","anms_mues25", &
	  	   "anms_mues26", "anms_mues27", "anms_mues28", "anms_mues29","anms_mues30"
		If IsNull(data) Then
			If Integer(Mid(ls_columna,10,2))>= 1 Then
				li_Numero	=	Integer(Mid(ls_columna,10,2)) - 1
				ls_colu 		= 	Fill("0",2 - Len(String(li_Numero))) + String(li_Numero)
				istr_mant.argumento[3] = ls_Colu
			End If
		Else
			ls_colu = Mid(ls_columna,10,2)
			wf_Calcula_Promedio(Row, Mid(ls_columna,10,2), 1, This)
			wf_Calcula_MinMax(Row, Mid(ls_columna,10,2), 1, This, True)
			wf_Calcula_MinMax(Row, Mid(ls_columna,10,2), 1, This, False)
			If wf_Valida(This, Row, ls_columna, Data) = False Then
				This.SetItem(Row, ls_Columna, integer(ls_nula))
				Return 1
			End if
				
			If iuo_Especies.Codigo <> 81 Then 
				wf_Desviacion_Estandar(Row, Mid(ls_columna,10,2), 1, This)
			End If
		End If
		
End Choose
end event

type tabpage_4 from userobject within tab_1
integer x = 18
integer y = 168
integer width = 3186
integer height = 952
long backcolor = 30586022
string text = "Pronóstico~r~nPudrición"
long tabtextcolor = 33554432
long tabbackcolor = 12632256
long picturemaskcolor = 536870912
dw_brotrytis dw_brotrytis
end type

on tabpage_4.create
this.dw_brotrytis=create dw_brotrytis
this.Control[]={this.dw_brotrytis}
end on

on tabpage_4.destroy
destroy(this.dw_brotrytis)
end on

type dw_brotrytis from datawindow within tabpage_4
accessiblerole accessiblerole = cursorrole!
integer x = 50
integer y = 72
integer width = 1486
integer height = 840
integer taborder = 30
string title = "none"
string dataobject = "dw_mues_ctlcalanalisisbrotytis"
boolean vscrollbar = true
boolean hsplitscroll = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type tabpage_5 from userobject within tab_1
integer x = 18
integer y = 168
integer width = 3186
integer height = 952
long backcolor = 30586022
string text = "Categoria Fruta"
long tabtextcolor = 33554432
long tabbackcolor = 12632256
long picturemaskcolor = 536870912
dw_categoriafruta dw_categoriafruta
end type

on tabpage_5.create
this.dw_categoriafruta=create dw_categoriafruta
this.Control[]={this.dw_categoriafruta}
end on

on tabpage_5.destroy
destroy(this.dw_categoriafruta)
end on

type dw_categoriafruta from datawindow within tabpage_5
integer x = 50
integer y = 72
integer width = 1390
integer height = 868
integer taborder = 21
string title = "none"
string dataobject = "dw_mues_ctlcalanalisiscategoria"
boolean hscrollbar = true
boolean vscrollbar = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event itemchanged;String	ls_Columna
Long	ll_Fila

ls_Columna	=	dwo.Name

Choose Case ls_Columna
	Case "ancg_marcas"
		For ll_Fila = 1 To This.RowCount()
			If ll_Fila <> Row Then
				This.Object.ancg_marcas[ll_Fila] = 0
			End If
		Next
		
End Choose
end event

type tabpage_6 from userobject within tab_1
integer x = 18
integer y = 168
integer width = 3186
integer height = 952
long backcolor = 30586022
string text = "Parámetros~r~nMadurez"
long tabtextcolor = 33554432
long tabbackcolor = 12632256
long picturemaskcolor = 536870912
dw_madurez dw_madurez
end type

on tabpage_6.create
this.dw_madurez=create dw_madurez
this.Control[]={this.dw_madurez}
end on

on tabpage_6.destroy
destroy(this.dw_madurez)
end on

type dw_madurez from uo_dw within tabpage_6
integer x = 50
integer y = 72
integer width = 3109
integer height = 868
integer taborder = 11
string dataobject = "dw_mant_mues_paramadurezdeta"
boolean hscrollbar = true
end type

event sqlpreview;//
end event

event itemchanged;String		ls_Columna, ls_colu
Integer	li_numero, li_null

SetNull(li_Null)

ls_Columna	=	dwo.Name
istr_mant.argumento[3] = ''

Choose Case ls_Columna
	Case "anma_mues01", "anma_mues02", "anma_mues03", "anma_mues04","anma_mues05", &
	  	   "anma_mues06", "anma_mues07", "anma_mues08", "anma_mues09","anma_mues10", &
		   "anma_mues11", "anma_mues12", "anma_mues13", "anma_mues14","anma_mues15"
		If IsNull(data) Then
			If Integer(Mid(ls_columna,10,2))>= 1 Then
				li_Numero	=	Integer(Mid(ls_columna,10,2)) - 1
				ls_colu 		= 	Fill("0",2 - Len(String(li_Numero))) + String(li_Numero)
				istr_mant.argumento[3] = ls_Colu
			End If
		Else
			ls_colu = Mid(ls_columna,10,2)
			wf_Calcula_Promedio(Row, Mid(ls_columna,10,2), 5, This)
			wf_Desviacion_Estandar(Row, Mid(ls_columna,10,2), 5, This)
			wf_Calcula_MinMax(Row, Mid(ls_columna,10,2), 5, This, True)
			wf_Calcula_MinMax(Row, Mid(ls_columna,10,2), 5, This, False)
			IF il_valida = 1 THEN
				This.SetItem(Row, is_columna, li_Null)
				Return 1
			End If	
				
			If Not wf_Valida(This, Row, ls_columna, Data) Then
				This.SetItem(Row, ls_Columna, li_Null)
				Return 1
			End If
		End If
		
End Choose
end event

event itemerror;call super::itemerror;Return 1
end event

event losefocus;call super::losefocus;dw_8.AcceptText()
end event

